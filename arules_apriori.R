library(arules)
setwd("G:/ProjectsOnG/Safe_Rx/CPI/Development/Association_Rules/EDB2012_10/stata_ivan/2012_12_20")

data_csv <- read.csv("bene_procedure_transactions.csv", sep = ",")
data_trans <- as(as.matrix(data_csv), "transactions")

rules <- apriori(data_trans, parameter = list(support = 0.05, confidence = 1, target = "rules", minlen = 30, maxlen = 35))

data_csv_t <- t(data_csv[0:1000,])
data_trans <- as(data_csv_t, "transactions")

data_csv <- read.csv("bene_procedure_transactions_ny.csv", sep = ",")
data_trans <- as(as.matrix(data_csv), "transactions")

rules_ny <- apriori(data_trans, parameter = list(support = 0.75, confidence = 1, target = "frequent itemsets", minlen = 2, maxlen = 10))
