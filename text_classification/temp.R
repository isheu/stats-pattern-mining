rm(list=ls(all=TRUE))
library("tm")
library("topicmodels")

# function of topic models with four methods
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

    rownames(all_alpha) <- rep("k=", max_topic-1)
    rownames(mean_entropy) <- rep("k=", max_topic-1)
    rownames(log_likelihood) <- rep("k=", max_topic-1)

    for (i in 2:max_topic) {
      rownames(all_alpha)[i-1] <- paste(rownames(all_alpha)[i-1], i, sep = "")
      rownames(mean_entropy)[i-1] <- paste(rownames(mean_entropy)[i-1], i, sep = "")
      rownames(log_likelihood)[i-1] <- paste(rownames(log_likelihood)[i-1], i, sep = "")
    }

    return(list(all_alpha, mean_entropy, log_likelihood))
}

K = 20

# 1-1
text <- readLines("othr_rsn_txt_stus11.txt", encoding="UTF-8")
text <- text[-1]    # drop the header
doc <- data.frame(text)
ds <- DataframeSource(doc)
training_txt <- Corpus(ds)

ctrl <- list(wordLengths = c(1, Inf))
dtm1 <- DocumentTermMatrix(training_txt, control = ctrl)
dim(dtm1)

x <- as.vector(dtm1$dimnames$Terms)
fileConn <- file("dtm1.txt")
writeLines(x, fileConn)
close(fileConn)

tm_dtm1 <- topicmodels(dtm1, K)

write.csv(tm_dtm1, file = "tm_dtm1.csv", quote = FALSE)


# 1-2
dtmsparse1 <- removeSparseTerms(dtm1, 0.99)

y <- as.vector(dtmsparse1$dimnames$Terms)
fileConn <- file("dtmsparse1.txt")
writeLines(y, fileConn)
close(fileConn)

d1 <- Dictionary(dtmsparse1$dimnames$Terms)
dtmsparse_d <- DocumentTermMatrix(training_txt, list(dictionary = d1, wordLangths = c(1, Inf)))
m <- as.matrix(dtmsparse_d)
m1 <- m[rowSums(m) != 0,]

tm_dtmsparse1 <- topicmodels(m1, K)

write.csv(tm_dtmsparse1, file = "tm_dtmsparse1.csv", quote = FALSE)


# 2-1
text1 <- readLines("othr_rsn_txt_stus11.txt", encoding="UTF-8")
text1 <- text1[-1]    # drop the header
doc1 <- data.frame(text1)
ds1 <- DataframeSource(doc1)
training_txt1 <- Corpus(ds1)

# removeQuotes <- function(x) {
#     x = gsub('"', '', x)
#     return(x)
# }

# training_txt1 <- tm_map(training_txt1, removeQuotes, useMeta = FALSE, lazy = FALSE)
training_txt1 <- tm_map(training_txt1, stripWhitespace)
training_txt1 <- tm_map(training_txt1, tolower)
training_txt1 <- tm_map(training_txt1, removeWords, stopwords("english"))
training_txt1 <- tm_map(training_txt1, removePunctuation)
# library(Snowball)
# training_txt1 <- tm_map(training_txt1, stemDocument)

ctrl <- list(wordLengths = c(1, Inf))
dtm2 <- DocumentTermMatrix(training_txt1, control = ctrl)
dim(dtm2)
inspect(dtm2[1:5,])

z <- as.vector(dtm2$dimnames$Terms)
fileConn <- file("dtm2.txt")
writeLines(z, fileConn)
close(fileConn)

d2 <- Dictionary(dtm2$dimnames$Terms)
dtm2_d <- DocumentTermMatrix(training_txt1, list(dictionary = d2, wordLangths = c(1, Inf)))
m <- as.matrix(dtm2_d)
m1 <- m[rowSums(m) != 0,]

tm_dtm2 <- topicmodels(m1, K)

write.csv(tm_dtm2, file = "tm_dtm2.csv", quote = FALSE)


# 2-2
dtmsparse2 <- removeSparseTerms(dtm2, 0.99)

w <- as.vector(dtmsparse2$dimnames$Terms)
fileConn <- file("dtmsparse2.txt")
writeLines(w, fileConn)
close(fileConn)

d3 <- Dictionary(dtmsparse2$dimnames$Terms)
dtmsparse2_d <- DocumentTermMatrix(training_txt1, list(dictionary = d3, wordLangths = c(1, Inf)))
m <- as.matrix(dtmsparse2_d)
m2 <- m[rowSums(m) != 0,]

tm_dtmsparse2 <- topicmodels(m2, K)

write.csv(tm_dtmsparse2, file = "tm_dtmsparse2.csv", quote = FALSE)


k <- 11
dtm1_k11 <- list(VEM = LDA(dtm1, k = k),
                 VEM_fixed = LDA(dtm1, k = k, control = list(estimate.alpha = FALSE)))

Topic_k11 <- topics(dtm1_k11[["VEM"]], 1)
Terms_k11 <- terms(dtm1_k11[["VEM"]], 30)
write.csv(Terms_k11, file = "terms_k11_dtm1.csv", quote = FALSE, row.names = FALSE)

dist11 <- posterior(dtm1_k11[["VEM"]])
summary(dist11$topic[,1])
summary(dist11$topic[,2])


k <- 12
dtm1_k12 <- list(VEM = LDA(dtm1, k = k),
                 VEM_fixed = LDA(dtm1, k = k, control = list(estimate.alpha = FALSE)))

Topic_k12 <- topics(dtm1_k12[["VEM"]], 1)
Terms_k12 <- terms(dtm1_k12[["VEM"]], 30)
write.csv(Terms_k12, file = "terms_k12_dtm1.csv", quote = FALSE, row.names = FALSE)

dist12 <- posterior(dtm1_k12[["VEM"]])
summary(dist12$topic[,1])
summary(dist12$topic[,2])


k <- 13
dtm1_k13 <- list(VEM = LDA(dtm1, k = k),
                 VEM_fixed = LDA(dtm1, k = k, control = list(estimate.alpha = FALSE)))

Topic_k13 <- topics(dtm1_k13[["VEM"]], 1)
Terms_k13 <- terms(dtm1_k13[["VEM"]], 30)
write.csv(Terms_k13, file = "terms_k13_dtm1.csv", quote = FALSE, row.names = FALSE)

dist13 <- posterior(dtm1_k13[["VEM"]])
summary(dist13$topic[,1])
summary(dist13$topic[,2])


k <- 14
dtm1_k14 <- list(VEM = LDA(dtm1, k = k),
                 VEM_fixed = LDA(dtm1, k = k, control = list(estimate.alpha = FALSE)))

Topic_k14 <- topics(dtm1_k14[["VEM"]], 1)
Terms_k14 <- terms(dtm1_k14[["VEM"]], 30)
write.csv(Terms_k14, file = "terms_k14_dtm1.csv", quote = FALSE, row.names = FALSE)

dist14 <- posterior(dtm1_k14[["VEM"]])
summary(dist14$topic[,1])
summary(dist14$topic[,2])


k <- 15
dtm1_k15 <- list(VEM = LDA(dtm1, k = k),
                 VEM_fixed = LDA(dtm1, k = k, control = list(estimate.alpha = FALSE)))

Topic_k15 <- topics(dtm1_k15[["VEM"]], 1)
Terms_k15 <- terms(dtm1_k15[["VEM"]], 30)
write.csv(Terms_k15, file = "terms_k15_dtm1.csv", quote = FALSE, row.names = FALSE)

dist15 <- posterior(dtm1_k15[["VEM"]])
summary(dist15$topic[,1])
summary(dist15$topic[,2])

