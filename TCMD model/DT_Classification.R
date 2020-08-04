library(caret) 
library(rpart) 
library(e1071)
library(DMwR)


rm(list=ls())

setwd("../Desktop/TCMD/Datasets/clustered_Dataset")
clusteredData = read.csv("satellite-unsupervised-labelled.csv", header=T)

table(clusteredData$ClusterLabel) ## "1" = "anomaly" class And "2" = "normal"

#remove the id
clusteredData <- clusteredData[,-1]

clusteredData$ClusterLabel <- factor(clusteredData$ClusterLabel)
set.seed(10)
validation_indexSAT <- createDataPartition(clusteredData$ClusterLabel, p = 0.7, list = FALSE)
trainReadSAT_C <- clusteredData[validation_indexSAT,]
testReadSAT_C <- clusteredData[-validation_indexSAT,]

table(trainReadSAT_C$ClusterLabel)
table(testReadSAT_C$ClusterLabel)


#--------------- DT ----------------------
set.seed(10)
# Grow tree
# rpart 递归分区和回归树
fit_clust <- rpart(ClusterLabel ~ .,trainReadSAT_C,  method="class")
print(fit_clust)

# predict fot the label for test data.
test_clust <-testReadSAT_C[,1:36]
head(test_clust)

# predict calss label
predicted_clust <- predict(fit_clust,test_clust,type="class") 
predicted_clust

classification_Clust <- table(Predicted_class = predicted_clust,
                              Actual_class = testReadSAT_C$ClusterLabel) 
classification_Clust 

# Confusion Matrix，混淆矩阵，用于展示模型预测精度的一种较为常见的方法
confusionMatrix(classification_Clust)

FPR <- 1- specificity(classification_Clust)
FPR
