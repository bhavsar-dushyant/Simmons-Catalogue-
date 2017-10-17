library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(stringr)
library(car)
library(ROCR)
library(MASS)
library(gmodels)
library(dummies)
library(Hmisc)


#import data
logit <- read.csv("Logit.csv")

# summary & structure of data
str(logit)
summary(logit)


#logit$Spending <- logit$Spending*1000

#Check For Missing Values in the Data set and replace with mode
sum(is.na(logit))

#logit[which(logit$XXX == "unknown"),]$XXX <- "ABC"

# Outlier Treatment
quantile(logit$Spending, c(0.05,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))
#logit$Spending[which(logit$Spending>6800)]<- 6737
#CrossTable(logit$Spending)

par(mfrow=c(1,1))

# Correlation Plot
library(corrplot)
library(cran)
install.packages("cran")
# calculate correlations
correlations <- cor(logit)
# create correlation plot
corrplot(correlations, method="number")
corrplot(correlations, method="circle")

ft <- lm(Purchase ~ ., data = logit[, -c(1,5)])

summary(ft)
## Creating Development(train) and Validation(test) Sample
set.seed(100)
logit$random <- runif(nrow(logit), 0, 1);
logit <- logit[order(logit$random),]
train <- logit[which(logit$random <= 0.7),]
test <- logit[which(logit$random > 0.7),]
c(nrow(train), nrow(test))


sum(logit$Purchase)/nrow(logit)
sum(train$Purchase)/nrow(train)
sum(train$Purchase)
sum(test$Purchase)/nrow(test)
sum(test$Purchase)
# Model with variables having high corelation
model1 <- glm(Purchase ~ Spending+Card, data = train, family = binomial)
summary(model1)

# Stepwise selection of variables
best_model = step(model1,direction = "both")
summary(best_model)

vif(model1)

#pred <- predict(model1,newdata=train) #gives you b0 + b1x1 + b2x2 + b3x3
#predclass <- predict(model1,newdata=train, type = "terms") #gives you b0 + b1x1 + b2x2 + b3x3
#probs <- exp(pred)/(1+exp(pred)) 

predTrain <- predict(model1,newdata = train, type = "response") # in-sample accuracy
train$predTrain <- predTrain
#train$probs <- probs
#train$pred <- pred
#train$predclass <- predclass
hist(predTrain)
table(train$Purchase, predTrain >= 0.6)

predTest <- predict(model1, newdata = test, type = "response") # out-sample accuracy
test$predTest <- predTest
hist(predTest)
table(test$Purchase, predTest >= 0.6)

# ROC Curve
library(ROCR)
ROCRpred <- prediction(predTrain, train$Purchase)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

# AUC calculation
auc.tmp <- performance(ROCRpred,"auc"); 
auc <- as.numeric(auc.tmp@y.values)
auc


## C-statistic
library(Hmisc)
train$predicted_prob = predict(model1,  type = "response")
rcorr.cens(train$predicted_prob,train$Purchase) 

test$predicted_prob = predict(model1, newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$Purchase)

#KS-statistic
model_score <- prediction(train$predicted_prob,train$Purchase)

model_perf <- performance(model_score, "tpr", "fpr")

ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)

which(ks_table == ks)

ks
auc
table(train$Purchase, predTrain >= 0.6)

model_score_test <- prediction(test$predicted_prob,test$Purchase)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks1=max(ks_table_test)

which(ks_table_test == ks1)


ks1

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

class(train$predTrain)

## deciling
train$deciles <- decile(train$predTrain)
test$deciles <- decile(test$predTest)
#View(train)
#View(test)


## Ranking code for Train data
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(train)
rank <- tmp_DT[, list(
  cnt = length(Purchase), 
  cnt_resp = sum(Purchase), 
  cnt_non_resp = sum(Purchase == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);

View(rank)


## Ranking code for Train data
#library(data.table)
tmp_DT = data.table(test)
h_rank <- tmp_DT[, list(
  cnt = length(Purchase), 
  cnt_resp = sum(Purchase), 
  cnt_non_resp = sum(Purchase == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_perct_resp <- round(h_rank$cum_resp * 100 / sum(h_rank$cnt_resp),2);
h_rank$cum_perct_non_resp <- round(h_rank$cum_non_resp * 100 / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_perct_resp - h_rank$cum_perct_non_resp);

View(h_rank)








