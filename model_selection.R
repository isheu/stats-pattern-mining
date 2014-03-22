rm(list=ls(all=TRUE))
library("tm")
library("topicmodels")
library("cvTools")
library("scatterplot3d")

max_topic = 35
num_method = 4
num_fold = 5

# k-means function
k_means <- function(DTM) {
    kmeans_dtm <- list(c1 = kmeans(DTM, 1), c2 = kmeans(DTM, 2), c3 = kmeans(DTM, 3), c4 = kmeans(DTM, 4), c5 = kmeans(DTM, 5),
                       c6 = kmeans(DTM, 6), c7 = kmeans(DTM, 7), c8 = kmeans(DTM, 8), c9 = kmeans(DTM, 9),
                       c10 = kmeans(DTM, 10), c11 = kmeans(DTM, 11), c12 = kmeans(DTM, 12), c13 = kmeans(DTM, 13),
                       c14 = kmeans(DTM, 14), c15 = kmeans(DTM, 15), c16 = kmeans(DTM, 16), c17 = kmeans(DTM, 17),
                       c18 = kmeans(DTM, 18), c19 = kmeans(DTM, 19), c20 = kmeans(DTM, 20), c21 = kmeans(DTM, 21), 
                       c22 = kmeans(DTM, 22), c23 = kmeans(DTM, 23), c24 = kmeans(DTM, 24), c25 = kmeans(DTM, 25), 
                       c26 = kmeans(DTM, 26), c27 = kmeans(DTM, 27), c28 = kmeans(DTM, 28), c29 = kmeans(DTM, 29), 
                       c30 = kmeans(DTM, 30), c31 = kmeans(DTM, 31), c32 = kmeans(DTM, 32), c33 = kmeans(DTM, 33),
                       c34 = kmeans(DTM, 34), c35 = kmeans(DTM, 35))

    within <- c(kmeans_dtm[["c1"]]$tot.withinss, kmeans_dtm[["c2"]]$tot.withinss, kmeans_dtm[["c3"]]$tot.withinss, 
                kmeans_dtm[["c4"]]$tot.withinss, kmeans_dtm[["c5"]]$tot.withinss, kmeans_dtm[["c6"]]$tot.withinss, 
                kmeans_dtm[["c7"]]$tot.withinss, kmeans_dtm[["c8"]]$tot.withinss, kmeans_dtm[["c9"]]$tot.withinss, 
                kmeans_dtm[["c10"]]$tot.withinss, kmeans_dtm[["c11"]]$tot.withinss, kmeans_dtm[["c12"]]$tot.withinss, 
                kmeans_dtm[["c13"]]$tot.withinss, kmeans_dtm[["c14"]]$tot.withinss, kmeans_dtm[["c15"]]$tot.withinss, 
                kmeans_dtm[["c16"]]$tot.withinss, kmeans_dtm[["c17"]]$tot.withinss, kmeans_dtm[["c18"]]$tot.withinss, 
                kmeans_dtm[["c19"]]$tot.withinss, kmeans_dtm[["c20"]]$tot.withinss, kmeans_dtm[["c21"]]$tot.withinss, 
                kmeans_dtm[["c22"]]$tot.withinss, kmeans_dtm[["c23"]]$tot.withinss, kmeans_dtm[["c24"]]$tot.withinss, 
                kmeans_dtm[["c25"]]$tot.withinss, kmeans_dtm[["c26"]]$tot.withinss, kmeans_dtm[["c27"]]$tot.withinss, 
                kmeans_dtm[["c28"]]$tot.withinss, kmeans_dtm[["c29"]]$tot.withinss, kmeans_dtm[["c30"]]$tot.withinss, 
                kmeans_dtm[["c31"]]$tot.withinss, kmeans_dtm[["c32"]]$tot.withinss, kmeans_dtm[["c33"]]$tot.withinss,
                kmeans_dtm[["c34"]]$tot.withinss, kmeans_dtm[["c35"]]$tot.withinss)

    between <- c(kmeans_dtm[["c1"]]$betweenss, kmeans_dtm[["c2"]]$betweenss, kmeans_dtm[["c3"]]$betweenss, 
                 kmeans_dtm[["c4"]]$betweenss, kmeans_dtm[["c5"]]$betweenss, kmeans_dtm[["c6"]]$betweenss, 
                 kmeans_dtm[["c7"]]$betweenss, kmeans_dtm[["c8"]]$betweenss, kmeans_dtm[["c9"]]$betweenss, 
                 kmeans_dtm[["c10"]]$betweenss, kmeans_dtm[["c11"]]$betweenss, kmeans_dtm[["c12"]]$betweenss, 
                 kmeans_dtm[["c13"]]$betweenss, kmeans_dtm[["c14"]]$betweenss, kmeans_dtm[["c15"]]$betweenss, 
                 kmeans_dtm[["c16"]]$betweenss, kmeans_dtm[["c17"]]$betweenss, kmeans_dtm[["c18"]]$betweenss, 
                 kmeans_dtm[["c19"]]$betweenss, kmeans_dtm[["c20"]]$betweenss, kmeans_dtm[["c21"]]$betweenss, 
                 kmeans_dtm[["c22"]]$betweenss, kmeans_dtm[["c23"]]$betweenss, kmeans_dtm[["c24"]]$betweenss, 
                 kmeans_dtm[["c25"]]$betweenss, kmeans_dtm[["c26"]]$betweenss, kmeans_dtm[["c27"]]$betweenss, 
                 kmeans_dtm[["c28"]]$betweenss, kmeans_dtm[["c29"]]$betweenss, kmeans_dtm[["c30"]]$betweenss, 
                 kmeans_dtm[["c31"]]$betweenss, kmeans_dtm[["c32"]]$betweenss, kmeans_dtm[["c33"]]$betweenss,
                 kmeans_dtm[["c34"]]$betweenss, kmeans_dtm[["c35"]]$betweenss)

    # within_ss <- min(within)
    within_ss_idx <- which.min(within)
    # between_ss <- max(between)
    # between_ss_idx <- which.max(between)

    return(list(min(within), which.min(within), max(between), which.max(between), 
                model = kmeans_dtm[[paste("c", within_ss_idx, sep="")]]))
}


# 1-1
text <- readLines("new_othr_rsn_txt_stus11.txt", encoding="UTF-8")
text <- text[-1]    # drop the header
doc <- data.frame(text)
ds <- DataframeSource(doc)
training_txt <- Corpus(ds)

ctrl <- list(wordLengths = c(1, Inf))
dtm1 <- DocumentTermMatrix(training_txt, control = ctrl)
dim(dtm1)

# k-means clustering
kmeans_dtm1_best_1 <- k_means(dtm1)
kmeans_dtm1_best_2 <- k_means(dtm1)
kmeans_dtm1_best_3 <- k_means(dtm1)
kmeans_dtm1_best_4 <- k_means(dtm1)
kmeans_dtm1_best_5 <- k_means(dtm1)
kmeans_dtm1_best_6 <- k_means(dtm1)
kmeans_dtm1_best_7 <- k_means(dtm1)
kmeans_dtm1_best_8 <- k_means(dtm1)
kmeans_dtm1_best_9 <- k_means(dtm1)
kmeans_dtm1_best_10 <- k_means(dtm1)

# k = 35 performs better
write.csv(kmeans_dtm1_best_7[[5]]$cluster, file = "kmeans_dtm1_best.csv", quote = FALSE)

# Count words frequency for each topic
high_prob_words <- list()
for (i in 1:35) {
    m <- as.matrix(dtm1[kmeans_dtm1_best_7[[5]]$cluster == i,])
    if (dim(m)[1] > 1) {
        m_nonzero <- m[, colSums(m) != 0]
        num_words <- dim(as.matrix(m_nonzero))[2]
        if (num_words > 30) {
            words <- names(sort(colSums(m_nonzero)))[(num_words-29):num_words]
        } else words <- names(sort(colSums(m_nonzero)))
    } else words <- colnames(m)[which(m!=0)]
    high_prob_words[[i]] <- rev(words)
}

for (i in 1:35) {
    write.csv(high_prob_words[[i]], file = paste("kmeans_words", i, ".csv", sep = ""), quote = FALSE)
}


x <- as.vector(dtm1$dimnames$Terms)
fileConn <- file("dtm1.txt")
writeLines(x, fileConn)
close(fileConn)

# LDA
dtm1_matrix <- as.matrix(dtm1)
num_doc <- dim(dtm1_matrix)[1]
dtm1_cv <- cvFolds(num_doc, K = num_fold, type = "random") 

# perplexity_dtm1 <- c()
# for (k in 2:max_topic) {
#     for (i in 1:5) {
#         perplexity <- rep(0, 5) 
#         dtm1_training <- dtm1_matrix[dtm1_cv$which != i,]
#         dtm1_test <- dtm1_matrix[dtm1_cv$which == i,]
#         training_tm <- LDA(dtm1_training, k = k)
#         perplexity[i] <- perplexity(training_tm, dtm1_test)
#     }
#     perplexity_dtm1[k-1] <- sum(perplexity) / 5
# }


perplexity_dtm1 <- matrix(0, num_method, max_topic-1)
for (k in 2:max_topic) {
    perplexity <- matrix(0, 4, 5) 
    avg_perplexity <- rep(0, 4)
    for (i in 1:5) {
        dtm1_training <- dtm1_matrix[dtm1_cv$which != i,]
        dtm1_test <- dtm1_matrix[dtm1_cv$which == i,]
        training_tm <- list(VEM = LDA(dtm1_training, k = k),
                            VEM_fixed = LDA(dtm1_training, k = k, control = list(estimate.alpha = FALSE)))
                            # Gibbs = LDA(dtm1_training, k = k, method = "Gibbs", 
                            #   control = list(burnin = 1000), thin = 100, iter = 1000),
                            # CTM = CTM(dtm1_training , k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
        perplexity[1, i] <- perplexity(training_tm[["VEM"]], dtm1_test)
        perplexity[2, i] <- perplexity(training_tm[["VEM_fixed"]], dtm1_test)
        # perplexity[3, i] <- perplexity(training_tm[["Gibbs"]], dtm1_test)
        # perplexity[4, i] <- perplexity(training_tm[["CTM"]], dtm1_test)
    }
    for (j in 1:4) {
        avg_perplexity[j] <- sum(perplexity[j,]) / 5
    }
    perplexity_dtm1[,k-1] <- avg_perplexity
}

rownames(perplexity_dtm1) <- c("VEM", "VEM_fixed", "", "")
# rownames(perplexity_dtm1) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

colnames(perplexity_dtm1) <- rep("k=", max_topic-1)
for (i in 2:max_topic) {
    colnames(perplexity_dtm1)[i-1] <- paste(colnames(perplexity_dtm1)[i-1], i, sep = "")
}

write.csv(perplexity_dtm1, file = "perplexity_dtm1_new.csv", quote = FALSE)

# plot perplexity
plot(2:35, perplexity_dtm1[1,], type = "l", col = "red", lwd = 2, xlab = "# of topics", ylab = "perplexity")
title(main = "Perplexity of 'VEM' Method")

plot(2:35, perplexity_dtm1[2,], type = "l", col = "blue", lwd = 2, xlab = "# of topics", ylab = "perplexity")
title(main = "Perplexity of 'VEM_fixed' Method")


# choose k = 13
k = 15
training_tm <- LDA(dtm1, k = k)
Topic <- topics(training_tm, 5)
Terms <- terms(training_tm, 20)
write.csv(Terms, file = "high_prob_words_15.csv", quote = FALSE, row.names = FALSE)
write.csv(Topic, file = "LDA_topic_15.csv", quote = FALSE, row.names = FALSE)




topic_dist <- posterior(training_tm)$topic

order_dist <- t(apply(topic_dist, 1, sort))



# 3D visualization plot
draw_3D_plot <- function(topic_dist, Topic) {
    # define the topic of each document
    doc_num <- dim(topic_dist)[1]
    topic_num <- dim(topic_dist)[2]

    # reduce dimension using PCA
    if (topic_num > 3) {
        x <- princomp(topic_dist)$scores[,1:3]
    } else x <- princomp(topic_dist)$scores

    # separate points according to assigned topic
    for (i in 1:topic_num) {
        y <- x[, Topic == i]
    }

    plot3d(y[,1], y[,2], y[,3], col = "red")


    # max_prob <- rep(0, doc_num)
    # for (i in 1:doc_num) {
    #     max_prob[i] <- max(topic_dist[i,])
    # }

    # topic <- rep(0, doc_num)
    # tie <- list()
    # for (i in 1:doc_num) {
        # topic[i] <- which(topic_dist[i,] == max_prob[i])
        # test <- c()
        # test <- which(topic_dist[i,] == max_prob[i])
        # if (length(test) != 1) {
        #     tie[[i]] <- test
        # }
    # }

}




















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

num_doc <- dim(m1)[1]
dtmsparse1_cv <- cvFolds(num_doc, K = num_fold, type = "random") 

perplexity_dtmsparse1 <- matrix(0, num_method, max_topic-1)
for (k in 2:max_topic) {
    perplexity <- matrix(0, 4, 5) 
    avg_perplexity <- rep(0, 4)
    for (i in 1:5) {
        training_data <- m1[dtmsparse1_cv$which != i,]
        test_data <- m1[dtmsparse1_cv$which == i,]
        training_tm <- list(VEM = LDA(training_data, k = k),
                            VEM_fixed = LDA(training_data, k = k, control = list(estimate.alpha = FALSE)),
                            Gibbs = LDA(training_data, k = k, method = "Gibbs", 
                              control = list(burnin = 1000), thin = 100, iter = 1000),
                            CTM = CTM(training_data, k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
        perplexity[1, i] <- perplexity(training_tm[["VEM"]], test_data)
        perplexity[2, i] <- perplexity(training_tm[["VEM_fixed"]], test_data)
        perplexity[3, i] <- perplexity(training_tm[["Gibbs"]], test_data)
        perplexity[4, i] <- perplexity(training_tm[["CTM"]], test_data)
    }
    for (j in 1:4) {
        avg_perplexity[j] <- sum(perplexity[j,]) / 5
    }
    perplexity_dtmsparse1[,k-1] <- avg_perplexity
}

rownames(perplexity_dtmsparse1) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

colnames(perplexity_dtmsparse1) <- rep("k=", max_topic-1)
for (i in 2:max_topic) {
    colnames(perplexity_dtmsparse1)[i-1] <- paste(colnames(perplexity_dtmsparse1)[i-1], i, sep = "")
}

write.csv(perplexity_dtmsparse1, file = "perplexity_dtmsparse1.csv", quote = FALSE)




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
# training_txt1 <- tm_map(training_txt1, stripWhitespace)
# training_txt1 <- tm_map(training_txt1, tolower)
training_txt1 <- tm_map(training_txt1, removeWords, stopwords("english"))
# training_txt1 <- tm_map(training_txt1, removePunctuation)
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
which(rowSums(m) == 0)
m1 <- m[rowSums(m) != 0,]

num_doc <- dim(m1)[1]
dtm2_cv <- cvFolds(num_doc, K = num_fold, type = "random") 

perplexity_dtm2 <- matrix(0, num_method, max_topic-1)
for (k in 2:max_topic) {
    perplexity <- matrix(0, 4, 5) 
    avg_perplexity <- rep(0, 4)
    for (i in 1:5) {
        training_data <- m1[dtm2_cv$which != i,]
        test_data <- m1[dtm2_cv$which == i,]
        training_tm <- list(VEM = LDA(training_data, k = k),
                            VEM_fixed = LDA(training_data, k = k, control = list(estimate.alpha = FALSE)),
                            Gibbs = LDA(training_data, k = k, method = "Gibbs", 
                              control = list(burnin = 1000), thin = 100, iter = 1000),
                            CTM = CTM(training_data, k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
        perplexity[1, i] <- perplexity(training_tm[["VEM"]], test_data)
        perplexity[2, i] <- perplexity(training_tm[["VEM_fixed"]], test_data)
        perplexity[3, i] <- perplexity(training_tm[["Gibbs"]], test_data)
        perplexity[4, i] <- perplexity(training_tm[["CTM"]], test_data)
    }
    for (j in 1:4) {
        avg_perplexity[j] <- sum(perplexity[j,]) / 5
    }
    perplexity_dtm2[,k-1] <- avg_perplexity
}

rownames(perplexity_dtm2) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

colnames(perplexity_dtm2) <- rep("k=", max_topic-1)
for (i in 2:max_topic) {
    colnames(perplexity_dtm2)[i-1] <- paste(colnames(perplexity_dtm2)[i-1], i, sep = "")
}

write.csv(perplexity_dtm2, file = "perplexity_dtm2.csv", quote = FALSE)



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

num_doc <- dim(m2)[1]
dtmsparse2_cv <- cvFolds(num_doc, K = num_fold, type = "random") 

perplexity_dtmsparse2 <- matrix(0, num_method, max_topic-1)
for (k in 2:max_topic) {
    perplexity <- matrix(0, 4, 5) 
    avg_perplexity <- rep(0, 4)
    for (i in 1:5) {
        training_data <- m2[dtmsparse2_cv$which != i,]
        test_data <- m2[dtmsparse2_cv$which == i,]
        training_tm <- list(VEM = LDA(training_data, k = k),
                            VEM_fixed = LDA(training_data, k = k, control = list(estimate.alpha = FALSE)),
                            Gibbs = LDA(training_data, k = k, method = "Gibbs", 
                              control = list(burnin = 1000), thin = 100, iter = 1000),
                            CTM = CTM(training_data, k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))
        perplexity[1, i] <- perplexity(training_tm[["VEM"]], test_data)
        perplexity[2, i] <- perplexity(training_tm[["VEM_fixed"]], test_data)
        perplexity[3, i] <- perplexity(training_tm[["Gibbs"]], test_data)
        perplexity[4, i] <- perplexity(training_tm[["CTM"]], test_data)
    }
    for (j in 1:4) {
        avg_perplexity[j] <- sum(perplexity[j,]) / 5
    }
    perplexity_dtmsparse2[,k-1] <- avg_perplexity
}

rownames(perplexity_dtmsparse2) <- c("VEM", "VEM_fixed", "Gibbs", "CTM")

colnames(perplexity_dtmsparse2) <- rep("k=", max_topic-1)
for (i in 2:max_topic) {
    colnames(perplexity_dtmsparse2)[i-1] <- paste(colnames(perplexity_dtmsparse2)[i-1], i, sep = "")
}

write.csv(perplexity_dtmsparse2, file = "perplexity_dtmsparse2.csv", quote = FALSE)




## Experiments
k <- 11
dtm1_k11 <- list(VEM = LDA(dtm1, k = k),
                 VEM_fixed = LDA(dtm1, k = k, control = list(estimate.alpha = FALSE)))

Topic_k11 <- topics(dtm1_k11[["VEM"]], 1)
Terms_k11 <- terms(dtm1_k11[["VEM"]], 30)
write.csv(Terms_k11, file = "terms_k11_dtm1.csv", quote = FALSE, row.names = FALSE)

dist11 <- posterior(dtm1_k11[["VEM"]])
summary(dist11$topic[,1])
summary(dist11$topic[,2])

write.csv(Topic_k11, file = "Topic_k11.csv", quote = FALSE)


k <- 12
dtmsparse1_k12 <- list(VEM = LDA(m1, k = k),
                       CTM = CTM(m1, k = k, control = list(var = list(tol = 10^-4), em = list(tol = 10^-3))))

Topic_k12 <- topics(dtmsparse1_k12[["VEM"]], 1)
Terms_k12 <- terms(dtmsparse1_k12[["VEM"]], 30)
write.csv(Terms_k12, file = "terms_k12_dtmsparse1.csv", quote = FALSE, row.names = FALSE)

dist12 <- posterior(dtmsparse1_k12[["VEM"]])
summary(dist12$topic[,1])
summary(dist12$topic[,2])


Topic_k12_CTM <- topics(dtmsparse1_k12[["CTM"]], 1)
Terms_k12_CTM <- terms(dtmsparse1_k12[["CTM"]], 30)
write.csv(Terms_k12_CTM, file = "terms_k12_dtmsparse1_CTM.csv", quote = FALSE, row.names = FALSE)

dist12_CTM <- posterior(dtmsparse1_k12[["CTM"]])
summary(dist12_CTM$topic[,1])
summary(dist12_CTM$topic[,2])



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

