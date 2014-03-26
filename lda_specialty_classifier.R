## Use R implementation of Latent Dirichlet Allocation (LDA) on problem of classifying the specialty of physician based on his/her procedures performed.
## "Documents": Physicians to be classified based on procedures performed. 
## "Vocabulary": Medical procedures, coded by HCPCS system, performed by the physicians (documents). 

library("tm")
library("topicmodels")

# Looping Parameters #
min_topic <- 50
max_topic <- 120
######################

setwd("Projects/LDA_Specialty/stata/2013_02_01")

spcl_data_loc <- "Projects/LDA_Specialty/data/2013_02_01/code_corpus_0"
spcl_data_source <- DirSource(spcl_data_loc)

spcl_corpus <- Corpus(spcl_data_source, Encoding = "UTF-8")
spcl_dtm <- DocumentTermMatrix(spcl_corpus)
rowTotals <- apply(spcl_dtm, 1, sum)
spcl_dtm <- spcl_dtm[rowTotals > 0]       ## Restriction: drop any terms that never appear in the corpus

## Inspect and output the vocabulary of corpus ##
dim(spcl_dtm)
inspect(spcl_dtm[1:5,])
vocab <- as.vector(spcl_dtm$dimnames$Terms)
file_output <- file("spcl_dtm_vocabulary.txt")
writeLines(vocab, file_output)
close(file_output)
#################################################

# Initialize matrices to store topic model results #
all_alpha <- matrix(0, max_topic - min_topic, 1)
mean_entropy <- matrix(0, max_topic - min_topic, 1)
log_likelihood <- matrix(0, max_topic - min_topic, 1)
perplexity <- matrix(0, max_topic - min_topic, 1)
####################################################

for (k in min_topic:max_topic) {
  training_tm <- list(VEM = LDA(spcl_dtm, k = k),
                      VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE)), 
                      Gibbs = LDA(spcl_dtm, k = k, method = "Gibbs", control = list(burnin = 1000), thin = 100, iter = 1000))
                      CTM = CTM(spcl_dtm , k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
  all_alpha[k-1,] <- sapply(training_tm[1:3], slot, "alpha")
  mean_entropy[k-1,] <- sapply(training_tm, function(x)
                           mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
  log_likelihood[k-1,] <- sapply(training_tm, logLik)
  perplexity[k-1,] <- perplexity(training_tm[["VEM"]])  

  all_alpha[k-1,]
  mean_entropy[k-1,]
  log_likelihood[k-1,]
  perplexity[k-1,]
}

##############################################################################

## Dataset col/row naming to prepare for output
colnames(all_alpha) <- c("VEM", "VEM_fixed", "Gibbs")
colnames(mean_entropy) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")
colnames(log_likelihood) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

rownames(all_alpha) <- rep("k=", max_topic-min_topic)
rownames(mean_entropy) <- rep("k=", max_topic-min_topic)
rownames(log_likelihood) <- rep("k=", max_topic-min_topic)
######

for (i in min_topic:max_topic) {
  rownames(all_alpha)[i-min_topic] <- paste(rownames(all_alpha)[i-min_topic], i, sep = "")
  rownames(mean_entropy)[i-min_topic] <- paste(rownames(mean_entropy)[i-min_topic], i, sep = "")
  rownames(log_likelihood)[i-min_topic] <- paste(rownames(log_likelihood)[i-min_topic], i, sep = "")
}

## Evaluation of Correlated Topic Model (CTM) ##
Topic <- topics(training_tm[["CTM"]], 1)
Terms <- terms(training_tm[["CTM"]], 10)

write.csv(Terms, file = "high_prob_words_10.csv", quote = FALSE, row.names = FALSE)

dist <- posterior(training_tm[["CTM"]])
summary(dist$topic[,1])
summary(dist$topic[,2])
save.image(file= "VEM_runs_" + min_topic + "_" + max_topic + "_topics.RData")
################################################
