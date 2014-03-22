library(arules)
setwd("Projects/Association_Rules/stata_ivan/2012_12_20/data")

## Format of bene_procedure_transactions.csv ##
## Beneficiary ID, Procedure #1, Procedure #2, Procedure #3 (by order of date of service/transaction)

## Clinically-meaningful bins
## Beneficiaries with a minimum # of procedures received
## Extendable to identifying common NPIs, TINs

data_csv <- read.csv("bene_procedure_transactions.csv", sep = ",")
data_trans <- as(as.matrix(data_csv), "transactions")    # Create arules library transactions data format

rules <- apriori(data_trans, parameter = list(support = 0.05, confidence = 1, target = "rules", minlen = 30, maxlen = 35))

data_csv <- read.csv("bene_procedure_transactions_ny.csv", sep = ",")
data_trans <- as(as.matrix(data_csv), "transactions")

rules_ny <- apriori(data_trans, parameter = list(support = 0.75, confidence = 1, target = "frequent itemsets", minlen = 2, maxlen = 10))
