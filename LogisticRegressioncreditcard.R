credit_card <- read.csv(file.choose()) 
View(credit_card[-1]) #except first column selecting all
str(credit_card)
install.packages("caret")
library(caret)
dmy <- dummyVars(" ~ .", data = credit_card,fullRank = T)
model <- glm(card~.,data = credit_card,family = "binomial")
summary(model)
prob <- predict(model,type = c("response"),credit_card)
prob
confusion <- table(prob>0.5,credit_card$card)
confusion
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy #0.901838

#Roc Curve
install.packages("ROCR")
library(ROCR)
rocpred <- prediction(prob,credit_card$card)
rocperf <- performance(rocpred,'tpr','fpr')
plot(rocperf,colorize=T,text.adj=c(-0.2,4))

#####
pred_values <- NULL
yes_no <- NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

bank_data[,"prob"] <- prob
bank_data[,"pred_values"] <- pred_values
bank_data[,"yes_no"] <- yes_no
View(bank_data)

View(bank_data[,c(17,18,19,20)])

# Accuracy 
acc <- table(bank_data$term.deposit,pred_values)
acc
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy

