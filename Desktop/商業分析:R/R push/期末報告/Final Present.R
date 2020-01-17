school <- read.csv("Admission_Predict_Ver1.1.csv")

#資料處理
library(dummies)
school$University.Rating <- factor(school$University.Rating,
                                   levels = c(unique(school$University.Rating)))
school <- dummy.data.frame(school)

#轉換資料符合假設
school$Chance.of.Admit <- ifelse(school$Chance.of.Admit >= 0.9,1,0)


#分訓練集與測試集
library(tidyverse)
set.seed(200)
train <- school %>% group_by(Chance.of.Admit) %>% sample_frac(0.8)
test <- anti_join(school, train, by ='Serial.No.')

#決策樹
library(rpart)
tree <- rpart(Chance.of.Admit ~ . ,data=train, method="class")
predicted_DT <- predict(tree, newdata=test, type="class")
Real = test$Chance.of.Admit
Predict = predicted_DT
DT_matrix <- table(Real, Predict) #confusion table
sum(diag(DT_matrix))/sum(DT_matrix) #預測正確率=0.94


#視覺化混淆矩陣
library(ggplot2)
ggplot(data =  as.data.frame(DT_matrix), mapping = aes(x = Real, y = Predict)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Accuracy Score = 0.94") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

#視覺化決策樹
library(rpart.plot) 
rpart.plot(tree) 

#隨機森林
library(randomForest)
train$Chance.of.Admit <- as.character(train$Chance.of.Admit)
train$Chance.of.Admit <- as.factor(train$Chance.of.Admit)
rf <- randomForest(Chance.of.Admit ~ ., data = train, importance=TRUE)

#看大概需要幾棵樹
plot(rf)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

#看變數重要性
importance(rf)
varImpPlot(rf)

#RF預測
predicted_RF <- predict(rf, newdata = test, type = "class")
Real1 = test$Chance.of.Admit
Predict1 = predicted_RF
RF_matrix <- table(Real1, Predict1)
sum(diag(RF_matrix))/sum(RF_matrix) #預測正確率= 0.99

#視覺化混淆矩陣
ggplot(data =  as.data.frame(RF_matrix), mapping = aes(x = Real1, y = Predict1)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Accuracy Score = 0.99") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

