library(caret,quietly = TRUE)
library(rpart,quietly = TRUE)
library(pROC,quietly = TRUE)

build_decision_tree <- function(X,y,test){
    set.seed(12345)
    tctrl2 <- trainControl(method = "adaptive_cv",repeats = 5,
                           classProbs = TRUE, summaryFunction = twoClassSummary)
    #
    #Build and optimize tree based on maxdepth of tree
    #
    dtree_fit <- train(X,y, method = "rpart2",
                       parms = list(split = "information"),
                       trControl=tctrl2,
                       metric="ROC",tuneLength = 10)
    #
    #Calculate performace metrics
    #
    pred.dtree<-predict(dtree_fit, newdata=test[,-ncol(test)],type="prob")
    pred.dtree.res <- predict(dtree_fit, newdata=test[,-ncol(test)])
    dtree.roc = pROC::roc( response=test[,ncol(test)], predictor=pred.dtree[,1] )
    dtree.auc = dtree.roc$auc[1]
    dtree=list( classifier=dtree_fit, pred.prob=pred.dtree,
                pred.result=pred.dtree.res, roc=dtree.roc, auc=dtree.auc )
    return(dtree)
}
