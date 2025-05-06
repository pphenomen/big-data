setwd("C:/Users/Administrator/Desktop/study/big-data/lab5")

library(factoextra)
library(cluster)
library(parameters)
library(scatterplot3d)

# 1. чтение данных
dataset <- read.csv("European Jobs_data.csv", header=TRUE, sep=";")
clean_dataset <- dataset[, -1] 
# View(dataset)

sector_names <- c(
  Agr = "Сельское хозяйство",
  Min = "Добыча",
  Man = "Промышленность",
  PS  = "Энергетика",
  Con = "Строительство",
  SI  = "Сфера услуг",
  Fin = "Финансы",
  SPS = "Социальные службы",
  TC  = "Транспорт и связь"
)

# 2. дескриптивный анализ
descr_analysis = summary(clean_dataset)
cat("Дескриптивный анализ по отраслям занятости: \n")
print(descr_analysis)

cat("\nСтандартное отклонение:\n")
print(sapply(clean_dataset, sd))

cat("\nДисперсия:\n")
print(sapply(clean_dataset, var))

columns <- colnames(clean_dataset)
for (col in columns) {
  hist(clean_dataset[[col]], main = sprintf('Распределение занятости "%s"', sector_names[[col]]), xlab = "Процент занятых", ylab = "Частота",col = "lightblue", border = "black")
}

boxplot(clean_dataset, main="Boxplot: Занятость по отраслям", col=rainbow(ncol(clean_dataset)), ylab = "Процент занятых")

# 3. нормализация и оценка оптимального числа кластеров: построение диаграмм "Метод силуэта", “Метод локтя”,  "Статистику разрыва" и Алгоритм консенсуса.
maxs <- apply(clean_dataset, 2, max)
mins <- apply(clean_dataset, 2, min)
scaled_data <- scale(clean_dataset, center = mins, scale = maxs - mins)
scaled_data <- as.data.frame(scaled_data)

fviz_nbclust(scaled_data, kmeans, method = "wss") + labs(title = "Метод локтя")
fviz_nbclust(scaled_data, kmeans, method = "silhouette") + labs(title = "Метод силуэта")

set.seed(123)
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 6, B = 50)
fviz_gap_stat(gap_stat) + labs(title = "Статистика разрыва")

set.seed(123)
n_clust <- n_clusters(scaled_data, package = c("easystats", "NbClust", "mclust"), standardize = FALSE)
plot(n_clust)

# 4. дендограмма
dist_matrix <- dist(scaled_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, labels = dataset$Country, main = "Дендрограмма: иерархическая кластеризация", xlab = "Страны", ylab = "Расстояние", sub = "", cex = 0.8)
rect.hclust(hc, k = 3, border = "red")

# 5, 6. кластеризация методом k-means и столбчатая диаграмма с боксплотом групп
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

clustered_data <- clean_dataset
clustered_data$cluster <- as.factor(kmeans_result$cluster)
# View(clustered_data)
# write.csv(clustered_data, "clustered_data.csv", row.names = FALSE)
fviz_cluster(kmeans_result, data = scaled_data, 
             geom = "point", ellipse.type = "norm",
             palette = "Set2", ggtheme = theme_minimal(),
             main = "Визуализация кластеров (k-means)")

g1 <- colMeans(clean_dataset[clustered_data$cluster == 1, ])
g2 <- colMeans(clean_dataset[clustered_data$cluster == 2, ])
g3 <- colMeans(clean_dataset[clustered_data$cluster == 3, ])

df <- data.frame(g1, g2, g3)
df_t <- t(df)
barplot(df_t, beside = TRUE, ylim = c(0, max(df_t) + 5), col = c("red", "green", "blue"), main = "Средние значения по кластерам", axes = FALSE, las = 2)
axis(2)
legend("top", legend = rownames(df_t), col = c("red", "green", "blue"), lwd = 10, bty = "n")

par(mfrow = c(3, 3))
for (col in names(clean_dataset)[-ncol(clean_dataset)]) {
  boxplot(clustered_data[[col]] ~ clustered_data$cluster, main = paste("Boxplot по", col), xlab = "Кластер", ylab = "Процент занятых", col = c("red", "green", "blue"))
}
par(mfrow = c(1, 1))

# 7. построение scatterplot
selected_vars <- c("Agr", "Man", "SPS", "SI")
colors <- c("red", "green", "blue")[clustered_data$cluster]
pairs(clean_dataset[, selected_vars], col = colors, pch = 19, main = "Scatterplot: распределение по кластерам")

# 8. трехмерная кластеризация по scatterplot3d
x <- clean_dataset$Agr
y <- clean_dataset$Man
z <- clean_dataset$SPS
colors <- c("red", "green", "blue")[clustered_data$cluster]
scatterplot3d(x, y, z, color = colors, pch = 19, main = "3D scatterplot по кластерам", xlab = "Agr (Сельское х-во)", ylab = "Man (Промышленность)", zlab = "SPS (Социальные службы)")

