## Use R implementation of Latent Dirichlet Allocation (LDA) on problem of classifying the specialty of physician based on his/her procedures performed.
## "Documents": Physicians to be classified based on procedures performed. 
## "Vocabulary": Medical procedures, coded by HCPCS system, performed by physicians. 

library("tm")
library("topicmodels")
library("sqldf")

max_topic <- 20

setwd("Projects/LDA_Specialty/stata/2013_02_01")

spcl_data <- read.table("npi_code_vector_subset1_seed10_wide.txt", header= TRUE, sep="\t")
spcl_data_no_id <- spcl_data[,-1]
spcl_data_source_no_id <- DataframeSource(spcl_data_no_id)
spcl_corpus_no_id <- Corpus(spcl_data_source_no_id)

system.time(training_tm <- list(VEM = LDA(spcl_dtm_no_id, k = 20)))
ctrl <- list(wordLengths = c(1,Inf))

## Create Document Term Matrix from corpus of providers + procedures used
spcl_dtm_no_id <- DocumentTermMatrix(spcl_corpus_no_id, control = ctrl)
system.time(training_tm <- list(VEM = LDA(spcl_dtm_no_id, k = 20)))
training_tm

vem_alpha[1,] <- sapply(training_tm, slot, "alpha")
vem_alpha <- sapply(training_tm, slot, "alpha")
vem_alpha

mean_entropy <- sapply(training_tm, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
mean_entropy

log_likelihood <- sapply(training_tm, logLik)
log_likelihood

TopicList <- topics(training_tm[["VEM"]], 1)
Terms <- terms(training_tm[["VEM"]], 30)

TopicList
Terms

write.csv(Terms, file = "high_prob_words_topics.csv", quote = FALSE)
save.image("ProjectsOnG\\LDA_Specialty\\stata\\2013_02_01\\runVEM_subset1.RData")
