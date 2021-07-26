library(caret)
library(dplyr)
library(glmnet)
library(ROCR)
library(pROC)
library(rpart.plot)

###going as per IV
##reading main file 
IV<-read.csv(file.choose(),header=TRUE,na.strings = "")
IV<-IV[,-1]
IV$Target_team1<-as.factor(IV$Target_team1)


##SPLITTING INTO Train and oot
IV_oot<-subset(IV,IV$series_name=="Pro Kabaddi League Season 7, 2019")
IV_Model<-subset(IV,IV$series_name != "Pro Kabaddi League Season 7, 2019")


# splitting data into 70% train and 30% test data

set.seed(123)
AB <- IV_Model$Target_team1 %>%
  createDataPartition(p=0.70, list = FALSE)

train <- IV_Model[AB, ]
test <- IV_Model[-AB, ]

write.csv(train,"train.csv")
write.csv(test,"test.csv")

##Fitting glm

model1 <- glm( Target_team1~Team2_stats.points.raid_points.raid_bonus + 
                 Team1_raids_successful_rate + Team2_stats.points.extras +
                 Team1_stats.raids.total
               +Team1_Tackle_successful_rate +Team2_stats.all_outs,
               data = train, family = binomial("logit"),maxit=200)
summary(model1)


# taking prediction > 0.5 as positive and otherwise as negative
train$prob<-predict(model1,train,type = 'response')
train$Predict <- ifelse(model1$fitted.values >0.5,"1","0")


# Analysing Results


# Confusion Matrix and comparing the observed value V/S Predicted Value
table1 <- table(train$Target_team1,train$Predict)
confusionMatrix(as.factor(train$Predict),as.factor(train$Target_team1))
table1
# Accuracy : 0.8558          
# 95% CI : (0.8129, 0.8921)
# No Information Rate : 0.5031          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.7117          
# 
# Mcnemars Test P-Value : 1               
#                                           
#             Sensitivity : 0.8580          
#             Specificity : 0.8537    


##deciling to check performance
train$decile <- ntile(-train$prob, 10)
table(train$prob,train$decile)

write.csv(train,"LR2_TRAIN.csv")

##ROC curve and Auc

roccurve=roc(train$Target_team1,as.numeric(train$Predict))
plot(roccurve, col="red", lwd=3, main="ROC curve train")
auc_train<-auc(roccurve)
auc_train ##0.8558

rm(model)
##checking the model on test
test$probabilities <- model1 %>% predict(test, type = "response")
test$predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
confusionMatrix(as.factor(predicted.classes),as.factor(test$Target_team1))
# Accuracy : 0.8333          
# 95% CI : (0.7605, 0.8913)
# No Information Rate : 0.5             
# P-Value [Acc > NIR] : 3.265e-16       
# 
# Kappa : 0.6667          
# 
# Mcnemars Test P-Value : 0.4042          
#                                           
#             Sensitivity : 0.8696          
#             Specificity : 0.7971

test$decile <- ntile(-test$probabilities, 10)
write.csv(test,"LR2_Test.csv")
##ROC curve and Auc

roccurve=roc(test$Target_team1,as.numeric(test$predicted.classes))
plot(roccurve, col="red", lwd=3, main="ROC curve test")
auc_test<-auc(roccurve)
auc_test ##0.8333


###checking on oot
IV_oot$probabilities1 <- model1 %>% predict(IV_oot, type = "response")
IV_oot$predicted.classes1 <- ifelse(probabilities1 > 0.5, "1", "0")
confusionMatrix(as.factor(predicted.classes1),as.factor(IV_oot$Target_team1))
# Accuracy : 0.8481         
# 95% CI : (0.7497, 0.919)
# No Information Rate : 0.5696         
# P-Value [Acc > NIR] : 1.126e-07      
# 
# Kappa : 0.6967         
# 
# Mcnemars Test P-Value : 0.1489         
#                                          
#             Sensitivity : 0.8000         
#             Specificity : 0.9118 

IV_oot$decile <- ntile(-IV_oot$probabilities1, 10)
write.csv(IV_oot,"LR2_oot.csv")

##ROC curve and Auc

roccurve=roc(IV_oot$Target_team1,as.numeric(IV_oot$predicted.classes1))
plot(roccurve, col="red", lwd=3, main="ROC curve OOT")
auc_OOT<-auc(roccurve)
auc_OOT ##0.8559
###########DECISION TREE###########
colnames(train)
train<-train[,-c(26:28)]
test<-test[,-c(26:28)]
IV_oot<-IV_oot[,-c(26:28)]

library(rpart)

# grow tree 
control<-rpart.control(minsplit = 40, minbucket = 20, maxdepth = 5)
fit <- rpart(Target_team1 ~Team2_stats.points.raid_points.raid_bonus + Team2_Raid_successful_rate + 
               Team1_raids_successful_rate + Team2_stats.points.extras + 
               venue_id + Team1_Green_cards + Type_of_Match + 
               Team1_stats.raids.total + toss_selection
             +Team1_Tackle_successful_rate +Team2_stats.all_outs, data = train,method="class",
             control = control)

summary(fit)
fit
rpart.plot(fit,fallen.leaves=FALSE,tweak=1.1,varlen=8,faclen=8)

#Predict Output on train
train$predicted.classes= predict(fit,train,type = 'class')
confusionMatrix(as.factor(train$predicted.classes),as.factor(train$Target_team1))
# Accuracy : 0.8405          
# 95% CI : (0.7961, 0.8785)
# No Information Rate : 0.5031          
# P-Value [Acc > NIR] : <2e-16
# Kappa : 0.6809          
# 
# Mcnemars Test P-Value : 0.4881          
#                                           
#             Sensitivity : 0.8210          
#             Specificity : 0.8598 


##ROC curve and Auc

roccurve=roc(train$Target_team1,as.numeric(train$predicted.classes))
plot(roccurve, col="red", lwd=3, main="ROC curve train")
auc_train<-auc(roccurve)
auc_train ##0.8404

#Predict Output on test
test$predicted.classes= predict(fit,test,type = 'class')
confusionMatrix(as.factor(test$predicted.classes),as.factor(test$Target_team1))

# Accuracy : 0.8188          
# 95% CI : (0.7443, 0.8792)
# No Information Rate : 0.5             
# P-Value [Acc > NIR] : 7.321e-15         
# Kappa : 0.6377          
# 
# Mcnemars Test P-Value : 0.4237          
#                                           
#             Sensitivity : 0.8551          
#             Specificity : 0.7826 

##ROC curve and Auc

roccurve=roc(test$Target_team1,as.numeric(test$predicted.classes))
plot(roccurve, col="red", lwd=3, main="ROC curve test")
auc_test<-auc(roccurve)
auc_test ##0.8188


#Predict Output on oot
IV_oot$predicted1= predict(fit,IV_oot,type = 'class')
confusionMatrix(as.factor(IV_oot$predicted1),as.factor(IV_oot$Target_team1))

# Accuracy : 0.9114          
# 95% CI : (0.8259, 0.9636)
# No Information Rate : 0.5696          
# P-Value [Acc > NIR] : 2.285e-11   
# Kappa : 0.8212          
# 
# Mcnemars Test P-Value : 0.4497          
#                                           
#             Sensitivity : 0.8889          
#             Specificity : 0.9412     

##ROC curve and Auc

roccurve=roc(IV_oot$Target_team1,as.numeric(IV_oot$predicted1))
plot(roccurve, col="red", lwd=3, main="ROC curve oot")
auc_oot<-auc(roccurve)
auc_oot ##0.915
colnames(IV_oot)
train<-train[,-c(26,27)]
test<-test[,-26]
IV_oot<-IV_oot[,-26]


###fitting random forest
library(randomForest)
rf <- randomForest(Target_team1~.,data=train)
rf
predTrain <- predict(rf, train, type = "class")
table(predTrain, train$Target_team1)  

