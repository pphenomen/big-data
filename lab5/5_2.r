setwd("C:/Users/Administrator/Desktop/study/big-data/lab5")
library(e1071)
library(party)
library(randomForest)
set.seed(42)

# загрузка данных
clustered_data <- read.csv("clustered_data.csv")
clustered_data$cluster <- as.factor(clustered_data$cluster)

# обучение наивного байесовского классификатора
ind <- sample(2, nrow(clustered_data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- clustered_data[ind == 1, ]
testData <- clustered_data[ind == 2, ]
nrow(trainData)
nrow(testData)
nrow(clustered_data)

bayes_model <- naiveBayes(cluster ~ ., data = trainData)
bayes_pred <- predict(bayes_model, newdata = testData)

cat("\nБайес: Точность =", round(mean(bayes_pred == testData$cluster) * 100, 2), "%\n")
cat("Таблица ошибок:\n")
print(table(Факт = testData$cluster, Прогноз = bayes_pred))
# testData[testData$cluster == 1, ]

# дерево решений
tree_model <- ctree(cluster ~ ., data = trainData)
tree_pred <- predict(tree_model, newdata = testData)

cat("\nДерево решений: Точность =", round(mean(tree_pred == testData$cluster) * 100, 2), "%\n")
cat("Таблица ошибок:\n")
print(table(Факт = testData$cluster, Прогноз = tree_pred))

plot(tree_model, main = "Дерево решений по кластерам")

# случайный лес
rf_model <- randomForest(cluster ~ ., data = trainData, ntree = 100, proximity = TRUE)
rf_pred <- predict(rf_model, newdata = testData)

cat("\nСлучайный лес: Точность =", round(mean(rf_pred == testData$cluster) * 100, 2), "%\n")
cat("Таблица ошибок:\n")
print(table(Факт = testData$cluster, Прогноз = rf_pred))