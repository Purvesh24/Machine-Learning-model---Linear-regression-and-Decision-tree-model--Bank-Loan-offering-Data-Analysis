setwd("C:/Users/borka/OneDrive/Desktop/Sayali/loan-data-for-dummy-bank")

library(mlr)
library(mice)
library(VIM)
library(ggplot2)
library(dplyr)


tr <- read.csv('Loan.csv')
str(tr)
View(tr)
summary(tr)

tr <- tr[1:10000,]

tr$application_type_cat <- NULL
tr$interest_payments <- NULL        
tr$income_category <- NULL        
tr$id <- NULL        
tr$year <- NULL
tr$id <- NULL
tr$year <- NULL
tr$issue_d <- NULL
tr$final_d <- NULL
tr$home_ownership <- NULL
tr$term <- NULL
tr$application_type <- NULL
tr$purpose <- NULL
tr$grade <- NULL
tr$region <- NULL

str(tr)


tr$loan_condition <- as.factor(tr$loan_condition)

# missing data Analysis
sapply(tr, function(x) sum(is.na(x)))

mice_plot <- aggr(tr, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(tr), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"));

par(mfrow=c(2,2))

hist(tr$loan_amount, 
     main="Histogram for Loan Amount", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,700))

;

barplot(table(tr$loan_condition))

barplot(table(tr$loan_amount))

summarizeColumns(tr)


boxplot(tr$loan_amount, col='maroon',xlab = 'LoanAmount', main = 'Box Plot for Loan Amount')


print(ggplot(tr, aes(x=loan_condition))+geom_bar()+facet_grid(.~home_ownership)+ggtitle("Loan Status by property Type"))


print(ggplot(tr, aes(x=loan_condition,y=annual_inc))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))

print(ggplot(tr, aes(x=loan_condition))+geom_bar()+facet_grid(.~purpose)+ggtitle("Loan Status by Requirement Type"))


set.seed(111)
sample <- sample.int(n = nrow(tr), size = floor(.80*nrow(tr)), replace = F)
trainnew <- tr[sample, ]
testnew  <- tr[-sample, ]





trainnew[,-9] <- scale(trainnew[,-9])
testnew[,-9] <- scale(testnew[,-9])

View(trainnew)

log.model <- glm(loan_condition ~., data = trainnew, family = binomial(link = "logit"),maxit = 100)

summary(log.model)


log.predictions <- predict(log.model, testnew, type="response")
head(log.predictions, 9)
        
log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
head(log.prediction.rd, 9)

table(log.prediction.rd, testnew[,9])
accuracy <- table(log.prediction.rd, testnew[,12])
sum(diag(accuracy))/sum(accuracy)


