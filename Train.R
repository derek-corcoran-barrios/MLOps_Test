library(caret)
library(gbm)

Titanic <- read.csv("data/Titanic.csv") 
Titanic$Survived <- ifelse(Titanic$Survived == 1, "Yes", "No")
Titanic <-  Titanic[,-c(1,4,9, 11)]

Titanic <- Titanic[complete.cases(Titanic),]

set.seed(2020)


Index <- createDataPartition(Titanic$Survived, list = F, p =0.8)
  
Train <- Titanic[Index,]

Test <- Titanic[-Index,]

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5)

gbmGrid <- expand.grid(shrinkage = 0.1, 
                       interaction.depth = c(1,2,3),
                       n.minobsinnode = c(10),
                       n.trees = c(50, 100, 150))

Fit1 <- train(Survived~., method = "gbm", 
              data = Train,
            #tunegrid = gbmGrid)#,
             trControl = ctrl)


Table_Results <- data.frame(Accuracy_Train = confusionMatrix(predict(Fit1, Train), reference = as.factor(Train$Survived))$overall[1], Accuracy_Test = confusionMatrix(predict(Fit1, Test), reference = as.factor(Test$Survived))$overall[1])

write.table(Table_Results, file = 'metrics.txt', col.names = FALSE, row.names = FALSE)
