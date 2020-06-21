#########################################Decission Tree#################################################


setwd("C:/Users/borka/OneDrive/Desktop/Sayali/loan-data-for-dummy-bank")

library(rpart)
library(rpart.plot)

# grow tree 
trainnew11 <- trainnew
testnew11 <- testnew

trainnew11$loan_condition_cat <- NULL
testnew11$loan_condition_cat <- NULL


dtree <- rpart(loan_condition ~.,method="class", data=trainnew11,parms=list(split="information"))
dtree$cptable

plotcp(dtree)


#dtree.pruned <- prune(dtree, cp=.02290076) #no need to run




prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
dtree.pred <- predict(dtree.pruned, trainnew11, type="class")

dtree.perf <- table(trainnew11$loan_condition, dtree.pred,
                    dnn=c("Actual", "Predicted"))

dtree.perf




#Accuracy: The number of correct predictions made divided by the total number of predictions made.
#Accuracy = (128+660)/(128+660+12) = 98.5
