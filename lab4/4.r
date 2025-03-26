library(rvest)

# уровень жизни стран мира по годам
url_21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

# функция выбирает все элементы таблицы на странице по селектору 'table'
nodes_21<-html_nodes(url_21, 'table')
nodes_20<-html_nodes(url_20, 'table')
nodes_19<-html_nodes(url_19, 'table')
nodes_18<-html_nodes(url_18, 'table')
nodes_17<-html_nodes(url_17, 'table')
nodes_16<-html_nodes(url_16, 'table')
nodes_15<-html_nodes(url_15, 'table')
nodes_14<-html_nodes(url_14, 'table')

# преобразования HTML-таблицы в датафрейм
df_21<-html_table(nodes_21[[2]])%>%as.data.frame()
df_20<-html_table(nodes_20[[2]])%>%as.data.frame()
df_19<-html_table(nodes_19[[2]])%>%as.data.frame()
df_18<-html_table(nodes_18[[2]])%>%as.data.frame()
df_17<-html_table(nodes_17[[2]])%>%as.data.frame()
df_16<-html_table(nodes_16[[2]])%>%as.data.frame()
df_15<-html_table(nodes_15[[2]])%>%as.data.frame()
df_14<-html_table(nodes_14[[2]])%>%as.data.frame()

rownames(df_21)<-df_21[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_14)<-df_14[, 2]

# выбор столбцов в датафрейме с оценками общего качетсва жизни
df_21<-df_21[, 3:11]
df_20<-df_20[, 3:11]
df_19<-df_19[, 3:11]
df_18<-df_18[, 3:11]
df_17<-df_17[, 3:11]
df_16<-df_16[, 3:11]
df_15<-df_15[, 3:11]
df_14<-df_14[, 3:11]

country<-c('United States', 'Canada', 'United Kingdom', 'Austria', 'Denmark')

# оценка индекса качества жизни
evaluation_of<-'Quality of Life Index'
QLI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(QLI)<-country
View(QLI)

mn<-min(QLI, na.rm=TRUE)
mx<-max(QLI, na.rm=TRUE)

plot(2014:2021, QLI$'United States', xlab='Года', ylab='Индекс качества жизни', ylim=c(mn-13,mx+13),
     main='Оценка индекса качества жизни',col='blue',type='b',lty=1,pch=1, lwd=2)

lines(2014:2021, QLI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, QLI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, QLI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, QLI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)
legend('bottomright', cex=0.6,country, fill= c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекс покупательной способности
evaluation_of<-'Purchasing Power Index'
PPI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(PPI)<-country
View(PPI)

mn<-min(PPI, na.rm=TRUE)
mx<-max(PPI, na.rm=TRUE)

plot( 2014:2021, PPI$'United States', xlab='Года', ylab='Индекс покупательной способности', ylim=c(mn-13,mx+13), 
      main='Оценка индекса покупательной способности', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PPI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PPI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PPI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PPI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекса безопасности
evaluation_of<-'Safety Index'
SI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(SI)<-country
View(SI)

mn<-min(SI, na.rm=TRUE)
mx<-max(SI, na.rm=TRUE)

plot( 2014:2021, SI$'United States', xlab='Года', ylab='Индекс безопасности', ylim=c(mn-13,mx+13),
      main='Оценка индекса безопасности', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, SI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка медицинского обслуживания
evaluation_of<-'Health Care Index'
HCI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(HCI)<-country
View(HCI)

mn<-min(HCI, na.rm=TRUE)
mx<-max(HCI, na.rm=TRUE)

plot(2014:2021, HCI$'United States', xlab='Года', ylab='Индекс медицинского обслуживания', ylim=c(mn-13,mx+13),
     main='Оценка индекс медицинского обслуживания ', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, HCI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, HCI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, HCI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, HCI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('bottomright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекса прожиточного минимума
evaluation_of<-'Cost of Living Index'
CLI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(CLI)<-country
View(CLI)

mn<-min(CLI, na.rm=TRUE)
mx<-max(CLI, na.rm=TRUE)

plot(2014:2021, CLI$'United States', xlab='Года', ylab='Индекс прожиточного минимума', ylim=c(mn-13,mx+13),
     main='Оценка индекса прожиточного минимума', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, CLI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, CLI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, CLI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, CLI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка отношения цены на жилье к доходу
evaluation_of<-'Property Price to Income Ratio'
PPIR<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(PPIR)<-country
View(PPIR)

mn<-min(PPIR, na.rm=TRUE)
mx<-max(PPIR, na.rm=TRUE)
plot( 2014:2021, PPIR$'United States', xlab='Года', ylab='Отношение цены на жилье к доходу', ylim=c(mn-13,mx+13),
      main='Оценка отношения цены на жилье к доходу', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PPIR$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PPIR$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PPIR$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PPIR$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекс времени движения на дороге
evaluation_of<-'Traffic Commute Time Index'
TCTI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(TCTI)<-country
View(TCTI)

mn<-min(TCTI, na.rm=TRUE)
mx<-max(TCTI, na.rm=TRUE)

plot( 2014:2021, TCTI$'United States', xlab='Года', ylab='Индекс времени движения на дороге', ylim=c(mn-13,mx+13), 
      main='Оценка индекса времени движения на дороге', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, TCTI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекса загрязнения
evaluation_of<-'Pollution Index'
PI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(PI)<-country
View(PI)

mn<-min(PI, na.rm=TRUE)
mx<-max(PI, na.rm=TRUE)

plot( 2014:2021, PI$'United States', xlab='Года', ylab='Индекс загрязнения', ylim=c(mn-13,mx+13),
      main='Оценка индекса загрязнения', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка климатического индекса
# за 2014 и 2015 года данные отсутствуют
evaluation_of<-'Climate Index'
CI<-as.data.frame(
  rbind(
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2016:2021
)
colnames(CI)<-country
View(CI)

mn<-min(CI, na.rm=TRUE)
mx<-max(CI, na.rm=TRUE)

plot( 2016:2021, CI$'United States', xlab='Года', ylab='Климатический индекс', ylim=c(mn-13,mx+13),
      main='Оценка климатического индекса', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2016:2021, CI$'Canada', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'United Kingdom', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Austria', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Denmark', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('bottomright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# WEB скрэпинг
library(rvest)

page <- read_html("https://www.sputnik8.com/ru/moscow")
cards <- html_nodes(page, ".activity-card_n0wr")

titles <- c()
links <- c()
description <- c()
ratings <- c()

for (card in cards) {
  titles <- c(titles, html_text(html_node(card, ".title_xIA3"), trim = TRUE))
  links <- c(links, paste0("https://www.sputnik8.com", html_attr(html_node(card, "a[role='link']"), "href")))
  ratings <- c(ratings, html_text(html_node(card, ".value-text_1vwc"), trim = TRUE))
  description <- c(description, html_text(html_node(card, ".description_phiK"), trim = TRUE))
}

df <- data.frame(
  Название = titles,
  Ссылка = links,
  Рейтинг = ratings,
  Описание = description,
  stringsAsFactors = FALSE
)

View(df)

