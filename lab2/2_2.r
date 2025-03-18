library(readxl)
library(dplyr) # анализ данных
library(ggplot2) # визуализация данных
library(tidyr) # манипуляции с данными
library(openxlsx)

setwd("C:/Users/Administrator/Desktop/study/big-data/lab2")

# 1. Импорт данных из csv-файла и xlsx-таблицы.
df_csv <- read.csv("ответы.csv", header=TRUE, sep=",")
df_xls <- read_excel("ответы.xlsx", sheet = 1)

print("Данные из CSV:")
print(head(df_csv))
print("Данные из Excel:")
print(head(df_xls))

# 2. Дескриптивный анализ
df_cleaned <- df_xls[, -c(1,2)]

summary_stats <- summary(df_cleaned)
print("Дескриптивная статистика:")
print(summary_stats)

std_dev <- apply(df_cleaned, 2, sd, na.rm=TRUE)
variance <- apply(df_cleaned, 2, var, na.rm=TRUE)

print("Стандартное отклонение:")
print(std_dev)
print("Дисперсия:")
print(variance)

hist(df_cleaned$`Red Bull`, breaks=10, col="blue", main="Гистограмма оценок Red Bull", xlab="Оценка", ylab="Частота")
boxplot(df_cleaned, main="Boxplot оценок напитков", col=rainbow(ncol(df_cleaned)))

# 3. Выполнить сортировку наборов данных по выбранному признаку.
df_sorted <- df_cleaned %>% arrange(`Adrenaline`) %>% filter(`Flash` > 5)
print(df_sorted)

# 4. Дескриптивный анализ на новом наборе данных
df_subset <- df_cleaned %>% filter(`Burn` > 8)
print(df_subset)
print(dim(df_subset))

print("Дескриптивная статистика поднабора данных:")
print(summary(df_subset))

hist(df_subset$`Red Bull`, breaks=5, col="blue", main="Гистограмма оценок Red Bull в новом наборе данных", xlab="Оценка", ylab="Частота")
boxplot(df_subset, main="Boxplot оценок в новом наборе данных", col=rainbow(ncol(df_subset)))

# 5. Cлияние таблиц, добавление строк, исключение переменных, формирование подмножества, умение загрузить данные их внешнего файла.
df_copy <- df_cleaned
df_copy$ID <- 1:nrow(df_copy)
df_new <- data.frame(
  ID = 1:nrow(df_copy),
  Genesis = sample(1:10, nrow(df_copy), replace=TRUE)
)
df_merged <- merge(df_copy, df_new, by="ID", all=TRUE)
print("Объединенная таблица:")
print(df_merged)

new_row <- data.frame(matrix(ncol = ncol(df_copy), nrow = 1))
colnames(new_row) <- colnames(df_copy)
new_row[1,] <- c(55,55,55,55,55,55,55,55,55,55,55)
df_copy <- rbind(df_copy, new_row)
print(df_copy)

df_copy <- df_copy %>% select(-'Adrenaline')
print(df_copy)

df_subset <- df_copy %>% filter(`Monster` > 7)
print(df_subset)

write.xlsx(df_merged, "merged_data.xlsx")
xlsx_merged <- read_excel("merged_data.xlsx", sheet=1)
print("Данные из merged_data.xlsx:")
print(xlsx_merged)

