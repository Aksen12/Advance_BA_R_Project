library(caret,quietly = TRUE)
library(pROC,quietly = TRUE)
library(rpart,quietly = TRUE)
library(randomForest,quietly = TRUE)
library(ipred,quietly = TRUE)
library(plyr,quietly = TRUE)
#
#Build a function to test multiple models
build_AUC_models <- function(X,y,test){
#
#Setting train re-sampling method in trainControl function
#
tctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                      classProbs = TRUE, summaryFunction = twoClassSummary)
#
# Logistic Regression
#
glm.fit <- train(X,y,method="glm",trControl = tctrl,metric="ROC")
pred.glm<-predict(glm.fit, newdata=test[,-ncol(test)],type="prob")
glm.rocCurve = pROC::roc( response=test[,ncol(test)], predictor=pred.glm[,1] )
glm.auc = glm.rocCurve$auc[1]
glm=list( classifier=glm.fit, predictions=pred.glm, roc=glm.rocCurve, auc=glm.auc )
#
# Decision Trees
#
dt.fit <- train(X,y,method="rpart",trControl = tctrl,metric="ROC")
pred.dt<-predict(dt.fit, newdata=test[,-ncol(test)],type="prob")
dt.rocCurve = pROC::roc( response=test[,ncol(test)], predictor=pred.dt[,1] )
dt.auc = dt.rocCurve$auc[1]
dt=list( classifier=dt.fit, predictions=pred.dt, roc=dt.rocCurve, auc=dt.auc )
#
# Random Forest
#
rf.fit <- train(X,y,method="rf",trControl = tctrl,metric="ROC")
pred.rf<-predict(rf.fit, newdata=test[,-ncol(test)],type="prob")
rf.rocCurve = pROC::roc( response=test[,ncol(test)], predictor=pred.rf[,1] )
rf.auc = rf.rocCurve$auc[1]
rf=list( classifier=rf.fit, predictions=pred.rf, roc=rf.rocCurve, auc=rf.auc )
#
# Bagged Trees
#
bag.fit <- train(X,y,method="treebag",trControl = tctrl,metric="ROC")
pred.bag<-predict(bag.fit, newdata=test[,-ncol(test)],type="prob")
bag.rocCurve = pROC::roc( response=test[,ncol(test)], predictor=pred.bag[,1] )
bag.auc = bag.rocCurve$auc[1]
bag=list( classifier=bag.fit, predictions=pred.bag, roc=bag.rocCurve, auc=bag.auc )

result = list( glm=glm, dt=dt, rf=rf, bag=bag )
return( result )
}
