# bigdf <- data.frame(dim=sample(letters, replace=T, 4e7), fact1=rnorm(4e7), fact2=rnorm(4e7, 20, 50)) 
# f <- file("npi_code_vector_wide_comma.csv")
# system.time(bigdf <- sqldf("select * from f", dbname = tempfile(), file.format = list(header = T, row.names = F)))

dim(spcl_dtm)
inspect(spcl_dtm[1:5,])
vocab <- as.vector(spcl_dtm$dimnames$Terms)
file_output <- file("spcl_dtm_output.txt")
writeLines(vocab, file_output)
close(file_output)

# Looping Parameters #

library("tm")
library("topicmodels")
library("sqldf")
setwd("G:/ProjectsOnG/Safe_Rx/CPI/Development/LDA_Specialty/EDB2012_12/stata_ivan/2013_02_01")

spcl_data <- "//prism/SASTempOnE/isheu/2013_02_01/code_corpus_0"
spcl_data_source <- DirSource(spcl_data)

spcl_corpus <- Corpus(spcl_data_source, Encoding = "UTF-8")
spcl_dtm <- DocumentTermMatrix(spcl_corpus)
rowTotals <- apply(spcl_dtm, 1, sum)
spcl_dtm <- spcl_dtm[rowTotals > 0]
max_topic <- 160
all_alpha <- matrix(0, max_topic-1, 1)
mean_entropy <- matrix(0, max_topic-1, 1)
log_likelihood <- matrix(0, max_topic-1, 1)
perplexity <- matrix(0, max_topic-1, 1)

for (k in 160:160) {
                      training_tm <- list(VEM = LDA(spcl_dtm, k = k))
  all_alpha[k-1,] <- sapply(training_tm, slot, "alpha")
  mean_entropy[k-1,] <- sapply(training_tm, function(x)
                          mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
  log_likelihood[k-1,] <- sapply(training_tm, logLik)
  perplexity[k-1,] <- perplexity(training_tm[["VEM"]])  
  all_alpha[k-1,]
  mean_entropy[k-1,]
  log_likelihood[k-1,]
  perplexity[k-1,]
}
save.image(file= "VEM_run_160_topics.RData")

##############################################################################

for (k in 60:71) {
                      training_tm <- list(VEM = LDA(spcl_dtm, k = k))
  all_alpha[k-1,] <- sapply(training_tm, slot, "alpha")
  mean_entropy[k-1,] <- sapply(training_tm, function(x)
                          mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
  log_likelihood[k-1,] <- sapply(training_tm, logLik)
}


for (k in 60:max_topic) {
                      training_tm <- list(VEM = LDA(spcl_dtm, k = k),
                      Gibbs = LDA(spcl_dtm, k = k, method = "Gibbs", 
                        control = list(burnin = 1000), thin = 100, iter = 1000),
                      CTM = CTM(spcl_dtm , k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
  all_alpha[k-1,] <- sapply(training_tm[0], slot, "alpha")
  mean_entropy[k-1,] <- sapply(training_tm, function(x)
                          mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
  log_likelihood[k-1,] <- sapply(training_tm, logLik)
}

colnames(all_alpha) <- c("VEM", "VEM_fixed", "Gibbs")
colnames(mean_entropy) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")
colnames(log_likelihood) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

rownames(all_alpha) <- rep("k=", max_topic-1)
rownames(mean_entropy) <- rep("k=", max_topic-1)
rownames(log_likelihood) <- rep("k=", max_topic-1)

for (i in 2:max_topic) {
  rownames(all_alpha)[i-1] <- paste(rownames(all_alpha)[i-1], i, sep = "")
  rownames(mean_entropy)[i-1] <- paste(rownames(mean_entropy)[i-1], i, sep = "")
  rownames(log_likelihood)[i-1] <- paste(rownames(log_likelihood)[i-1], i, sep = "")
}


Topic <- topics(training_tm[["CTM"]], 1)
Terms <- terms(training_tm[["CTM"]], 20)
Terms
write.csv(Terms, file = "high_prob_words_5.csv", quote = FALSE, row.names = FALSE)

dist <- posterior(training_tm[["CTM"]])
summary(dist$topic[,1])
summary(dist$topic[,2])

