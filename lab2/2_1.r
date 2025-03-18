library(readxl)
library(dplyr)
library(tidyr)

setwd("C:/Users/Administrator/Desktop/study/big-data/lab2")

df <- read_excel("ответы.xlsx", sheet = 1)
print("Названия колонок:")
print(colnames(df))

df_cleaned <- df[, -c(1,2)]  # фио и время
head(df_cleaned)

# 1. Вычислить max, min, mean по каждому столбцу
stats <- df_cleaned %>% summarise_all(list(min = min, max = max, mean = mean), na.rm = TRUE)
print(as.data.frame(stats))

# 2. Подсчитать количество людей, отдавших предпочтение выбранному элементу >0.7 и <0.3 (составить вектор);
above_07 <- colSums(df_cleaned > 7, na.rm=TRUE)
below_03 <- colSums(df_cleaned < 3, na.rm=TRUE)

threshold_counts <- data.frame(
  Оценка_более_7 = above_07,
  Оценка_менее_3 = below_03
)
print("Количество людей по оценкам:")
print(threshold_counts)

# 3. Вывод рейтинга напитков в порядке убывания среднего значения
ranking <- df_cleaned %>%
  summarise_all(mean, na.rm=TRUE) %>%
  pivot_longer(cols = everything(), names_to = "Напиток", values_to = "Средний_рейтинг") %>%
  arrange(desc(Средний_рейтинг))
print("Рейтинг напитков:")
print(ranking)

# 4. Работа с пропущенными данными
df_no_na <- na.omit(df_cleaned) # NA
df_filled_na <- df_cleaned %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm=TRUE), .))

print("Таблица без пропущенных значений:")
print(df_no_na)

print("Таблица с замененными NA на средние:")
print(df_filled_na)

# 5. Выбор строк (Red Bull > 8)
filtered_df <- df_cleaned %>% filter(`Red Bull` > 8)
print("Фильтрованные данные (Red Bull > 8):")
print(filtered_df)

# 6. Построение столбчатой диаграммы
barplot(colMeans(df_cleaned, na.rm=TRUE), 
        main="Средние оценки напитков (1-10)", 
        col=rainbow(ncol(df_cleaned)), 
        names.arg=names(df_cleaned), 
        las=2)
