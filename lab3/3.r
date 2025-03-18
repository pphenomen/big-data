library(readxl)
library(openxlsx)
setwd("C:/Users/Administrator/Desktop/study/big-data/lab3")

# 1
men <- read_excel("men.xlsx", sheet = 1)
women <- read_excel("women.xlsx", sheet = 1)

# 2
# столбчатая диаграмма
stat_men <- colSums(men[,-1])
stat_women <- colSums(women[,-1])
men_women <- rbind(stat_men, stat_women)

barplot(height = men_women, beside = TRUE, xlab = "Место", ylab = "Количество", names.arg = 1:8, col = c("navyblue", "red"), main = "Мужчины и женщины (за все время)")
legend("topright", legend = c("Мужчины", "Женщины"), fill = c("navyblue", "red"))

# круговая диаграмма
first_men <- men[, c(1, 2)][men$`1` > 0, ]
first_women <- women[, c(1, 2)][women$`1` > 0, ]

gold_men <- sum(first_men$`1`)
gold_women <- sum(first_women$`1`)
gold_sum <- c(gold_men, gold_women)

pie(gold_sum, labels = gold_sum, col = c("navyblue", "red"), cex = 1, main = "Количество золотых медалей у мужчин и женщин\nза все время")
legend("topright", cex=0.7, legend = c("Мужчины", "Женщины"), fill = c("navyblue", "red"))

# функциональный график
start_year <- max(men$year) - 30
men_30 <- men[men$year >= start_year, ]
women_30 <- women[women$year >= start_year, ]

prize_men <- data.frame(Год=men_30$year, Призовые=rowSums(men_30[, 2:4]))
prize_women <- data.frame(Год=women_30$year, Призовые=rowSums(women_30[, 2:4]))

par(mfrow=c(1,1))
plot(prize_men$Год, prize_men$Призовые, type="b", pch=19, col="blue", 
     xaxt="n", ylim=c(0.5, max(c(prize_men$Призовые, prize_women$Призовые))),
     main="Призовые места США по баскетболу за 30 лет", xlab="Год", ylab="Призовые места")
lines(prize_women$Год, prize_women$Призовые, type="o", pch=19, col="red", lty=2)
legend("topright", legend=c("Мужчины", "Женщины"), col=c("blue", "red"), lty=1, pch=19)
axis(side=1, at=prize_men$Год)

# 3
# графики изменения
gold_medals <- read_excel("gold_medals.xlsx")
bronze_medals <- read_excel("bronze_medals.xlsx")

# золото
plot(gold_medals$Год, gold_medals$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,70), 
     xlab="Год", ylab="Золотых медалей", main="Золотые медали за 6 последних олимпиад")
lines(gold_medals$Год, gold_medals$Китай, type="o", pch=19, col="#1aafd0")
lines(gold_medals$Год, gold_medals$Япония, type="o", pch=19, col="#6a67ce")
lines(gold_medals$Год, gold_medals$Великобритания, type="o", pch=19, col="#ffb900")
lines(gold_medals$Год, gold_medals$Германия, type="o", pch=19, col="gray70")
lines(gold_medals$Год, gold_medals$Австралия, type="o", pch=19, col="#2e3c54")
lines(gold_medals$Год, gold_medals$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=gold_medals$Год)
legend("topright", legend=c("США", "Китай", "Япония", "Великобритания", "Германия", "Австралия", "Нидерланды"), 
       fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))

# бронза
plot(bronze_medals$Год, bronze_medals$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,70),
     xlab="Год", ylab="Бронзовых медалей", main="Бронзовые медали за 6 последних олимпиад")
lines(bronze_medals$Год, bronze_medals$Китай, type="o", pch=19, col="#1aafd0")
lines(bronze_medals$Год, bronze_medals$Япония, type="o", pch=19, col="#6a67ce")
lines(bronze_medals$Год, bronze_medals$Великобритания, type="o", pch=19, col="#ffb900")
lines(bronze_medals$Год, bronze_medals$Германия, type="o", pch=19, col="gray70")
lines(bronze_medals$Год, bronze_medals$Австралия, type="o", pch=19, col="#2e3c54")
lines(bronze_medals$Год, bronze_medals$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=bronze_medals$Год)
legend("top", legend=c("США", "Китай", "Япония", "Великобритания", "Германия", "Австралия", "Нидерланды"),
       fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))

# 4 
# последние 6 олимпиад
latest_years <- tail(sort(unique(men$year)), 6)
men_latest <- subset(men, year %in% latest_years)
women_latest <- subset(women, year %in% latest_years)

# общее количество призовых мест
men_latest$Призовые <- rowSums(men_latest[,2:4])
women_latest$Призовые <- rowSums(women_latest[,2:4])

# итоговый датасет
prize_data <- data.frame(
  Год = men_latest$year,
  Мужчины = men_latest$Призовые,
  Женщины = women_latest$Призовые
)
write.xlsx(prize_data, "prize_data.xlsx")

par(mfrow=c(1,3))
# функциональный график
plot(prize_data$Год, prize_data$Мужчины, type="b", pch=19, col="navyblue", 
     xaxt="n", ylim=c(min(c(prize_data$Мужчины, prize_data$Женщины)), max(c(prize_data$Мужчины, prize_data$Женщины))),
     xlab="Год", ylab="Призовые места", main="Динамика призовых мест США по баскетболу\nза последние 6 Олимпиад")
lines(prize_data$Год, prize_data$Женщины, type="o", pch=19, col="red", lty=2)
axis(side=1, at=prize_data$Год)
legend("topright", legend=c("Мужчины", "Женщины"), col=c("navyblue", "red"), lty=c(1,2), pch=19)

# столбчатая диаграмма
barplot(height = t(as.matrix(prize_data[,-1])), beside = TRUE, xlab = "Год", ylab = "Количество призовых мест", 
        names.arg = prize_data$Год, col = c("navyblue", "red"), main = "Количество призовых мест США по баскетболу\n за последние 6 ОИ"
)
legend("topright", legend = c("Мужчины", "Женщины"), fill = c("navyblue", "red"))

# Круговая диаграмма
prize_6_sum <- colSums(prize_data[,-1])
pie(prize_6_sum, labels = prize_6_sum,  col = c("navyblue", "red"), main = "Всего призовых мест у мужчин и женщин\nпо баскетболу за последние 6 ОИ")
legend("topright", legend = c("Мужчины", "Женщины"), fill = c("navyblue", "red"))