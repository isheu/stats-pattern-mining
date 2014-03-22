library("tm")
library("topicmodels")

# preprocessing
rm(list=ls(all=TRUE))
text <- readLines("othr_rsn_txt_stus11.txt", encoding="UTF-8")
text <- text[-1]    # drop the header
doc <- data.frame(text)
ds <- DataframeSource(doc)
training_txt <- Corpus(ds)

# removeQuotes <- function(x) {
#     x = gsub('"', '', x)
#     return(x)
# }

# training_txt <- tm_map(training_txt, removeQuotes, useMeta = FALSE, lazy = FALSE)
# training_txt <- tm_map(training_txt, stripWhitespace)
# training_txt <- tm_map(training_txt, tolower)
# training_txt <- tm_map(training_txt, removeWords, stopwords("english"))
# training_txt <- tm_map(training_txt, removePunctuation)
# library(Snowball)
# training_txt <- tm_map(training_txt, stemDocument)

# generalize document-term matrix
ctrl <- list(wordLengths = c(1, Inf))
dtm <- DocumentTermMatrix(training_txt, control = ctrl)
dim(dtm)
inspect(dtm[1:5,])

# export all vocabulary
x <- as.vector(dtm$dimnames$Terms)
fileConn <- file("dtm1.txt")
writeLines(x, fileConn)
close(fileConn)

max_topic <- 20
all_alpha <- matrix(0, max_topic-1, 3)
mean_entropy <- matrix(0, max_topic-1, 4)
log_likelihood <- matrix(0, max_topic-1, 4)

for (k in 2:max_topic) {
  training_tm <- list(VEM = LDA(dtm, k = k),
                      VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE)),
                      Gibbs = LDA(dtm, k = k, method = "Gibbs", 
                        control = list(burnin = 1000), thin = 100, iter = 1000),
                      CTM = CTM(dtm , k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
  all_alpha[k-1,] <- sapply(training_tm[1:3], slot, "alpha")
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



topicmodels <- function(DTM, max_topic) {
    all_alpha <- matrix(0, max_topic-1, 3)
    mean_entropy <- matrix(0, max_topic-1, 4)
    log_likelihood <- matrix(0, max_topic-1, 4)

    for (k in 2:max_topic) {
      training_tm <- list(VEM = LDA(DTM, k = k),
                          VEM_fixed = LDA(DTM, k = k, control = list(estimate.alpha = FALSE)),
                          Gibbs = LDA(DTM, k = k, method = "Gibbs", 
                            control = list(burnin = 1000), thin = 100, iter = 1000),
                          CTM = CTM(DTM , k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
      all_alpha[k-1,] <- sapply(training_tm[1:3], slot, "alpha")
      mean_entropy[k-1,] <- sapply(training_tm, function(x)
                              mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
      log_likelihood[k-1,] <- sapply(training_tm, logLik)
    }

    colnames(all_alpha) <- c("VEM", "VEM_fixed", "Gibbs")
    colnames(mean_entropy) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")
    colnames(log_likelihood) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

    for (i in 2:max_topic) {
      rownames(all_alpha)[i-1] <- paste("k=", i, sep = "")
      rownames(mean_entropy)[i-1] <- paste("k=", i, sep = "")
      rownames(log_likelihood)[i-1] <- paste("k=", i, sep = "")
    }

    return(list(all_alpha, mean_entropy, log_likelihood))
}


###
dtmsparse <- removeSparseTerms(dtm, 0.99)

x <- as.vector(dtm$dimnames$Terms)
fileConn <- file("dtmsparse1.txt")
writeLines(x, fileConn)
close(fileConn)

d <- Dictionary(dtmsparse$dimnames$Terms)
dtmsparse_d <- DocumentTermMatrix(training_txt, list(dictionary = d, wordLangths = c(1, Inf)))
m <- as.matrix(dtmsparse_d)
m1 <- m[rowSums(m) != 0,]




k <- 2
tm_lessSparse <- list(VEM = LDA(m1, k = k),
                      VEM_fixed = LDA(m1, k = k, control = list(estimate.alpha = FALSE)))
Terms1 <- terms(tm_lessSparse[["VEM"]], 20)
Terms1
write.csv(Terms1, file = "high_prob_words_less_sparse_5.csv", quote = FALSE, row.names = FALSE)

dist1 <- posterior(tm_lessSparse[["VEM"]])
summary(dist1$topic[,1])
summary(dist1$topic[,2])

sapply(tm_lessSparse, logLik)


b <- posterior(training_tm[["VEM"]])
names(b)




###
rm(list=ls(all=TRUE))
text <- readLines("othr_rsn_txt_stus11.txt", encoding="UTF-8")
text <- text[-1]    # drop the header
doc <- data.frame(text)
ds <- DataframeSource(doc)
training_txt <- Corpus(ds)

# removeQuotes <- function(x) {
#     x = gsub('"', '', x)
#     return(x)
# }

# training_txt <- tm_map(training_txt, removeQuotes, useMeta = FALSE, lazy = FALSE)
training_txt <- tm_map(training_txt, stripWhitespace)
training_txt <- tm_map(training_txt, tolower)
training_txt <- tm_map(training_txt, removeWords, stopwords("english"))
training_txt <- tm_map(training_txt, removePunctuation)
# library(Snowball)
# training_txt <- tm_map(training_txt, stemDocument)

ctrl <- list(wordLengths = c(1, Inf))
dtm <- DocumentTermMatrix(training_txt, control = ctrl)
dim(dtm)
inspect(dtm[1:5,])

x <- as.vector(dtm$dimnames$Terms)
fileConn <- file("output2.txt")
writeLines(x, fileConn)
close(fileConn)

d <- Dictionary(dtm$dimnames$Terms)
dtmsparse_d <- DocumentTermMatrix(training_txt, list(dictionary = d, wordLangths = c(1, Inf)))
m <- as.matrix(dtmsparse_d)
m1 <- m[rowSums(m) != 0,]

k <- 5
training_tm <- list(VEM = LDA(m1, k = k),
                    VEM_fixed = LDA(m1, k = k, control = list(estimate.alpha = FALSE)))

Topic <- topics(training_tm[["VEM"]], 1)
Terms <- terms(training_tm[["VEM"]], 20)
Terms
write.csv(Terms, file = "high_prob_words_8.csv", quote = FALSE, row.names = FALSE)

dist <- posterior(training_tm[["VEM"]])
summary(dist$topic[,1])
summary(dist$topic[,2])
