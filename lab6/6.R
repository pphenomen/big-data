# 1. дескриптивный анализ
data <- read.csv("athlete_events.csv")

basketball_usa <- subset(data, Sport == "Basketball" & Team == "United States")

cat("\nУникальных спортсменов:")
print(length(unique(basketball_usa$ID)))

cat("\nРаспределение по полу:")
print(table(basketball_usa$Sex))

cat("\nРаспределение по медалям (включая NA):")
print(table(basketball_usa$Medal, useNA = "ifany"))

cat("\nСтатистика по возрасту:\n")
print(summary(basketball_usa$Age))

cat("\nСтатистика по росту:\n")
print(summary(basketball_usa$Height))

cat("\nСтатистика по весу:\n")
print(summary(basketball_usa$Weight))

hist(basketball_usa$Weight, main = "Распределение веса баскетболистов США", xlab = "Вес (кг)", col = "lightblue", breaks = 20)
boxplot(basketball_usa$Height, main = "Boxplot: Рост", col = "lightgreen")

# 2. проверка на нормальность и дисперсию
weight <- na.omit(basketball_usa$Weight)

cat("\nТест Шапиро–Уилка для веса:\n")
print(shapiro.test(weight))

qqnorm(weight, main = "Q-Q график: Вес баскетболистов")
qqline(weight, col = "red")

cat("\nПроверка равенства дисперсий (мужчины и женщины):\n")
print(bartlett.test(Weight ~ Sex, data = basketball_usa))

# 3. проверка гипотезы о среднем весе спортсменов
cat("\nРезультат теста Манна–Уитни (Wilcoxon test):\n")
print(wilcox.test(basketball_usa$Weight, mu = 90, conf.int = TRUE, exact = FALSE))

# 4. проверка гипотезы о равенстве среднего веса женщин (мужчин) в баскетбол и волейбол
sport1 <- subset(data, Sport == "Basketball" & Sex == "F")
sport2 <- subset(data, Sport == "Volleyball" & Sex == "F")

w1 <- na.omit(sport1$Weight)
w2 <- na.omit(sport2$Weight)

cat("\nПроверка нормальности (Shapiro-Wilk):\n")
cat("Basketball (женщины):\n")
print(shapiro.test(w1))
cat("Volleyball (женщины):\n")
print(shapiro.test(w2))

cat("\nПроверка равенства дисперсий (Bartlett test):\n")
print(bartlett.test(list(w1, w2)))

cat("\nРезультат теста Манна–Уитни (Wilcoxon test):\n")
print(wilcox.test(w1, w2, paired = FALSE, conf.int = TRUE))