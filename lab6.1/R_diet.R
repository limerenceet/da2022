#Дисперсионный анализ. Пример
setwd('D:/R')
getwd()
#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
data = read.csv("data/diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#data/Diet_data_description.docx
#data/diet.csv
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)


#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

#попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

#Задание
#Добавить проверку на выборы и избавиться от них
data <- data %>% drop_na()

list_quantiles <- tapply(data$weight.loss, data$diet.type, quantile)

Q1s <- sapply(1:3, function(i) list_quantiles[[i]][2])
Q3s <- sapply(1:3, function(i) list_quantiles[[i]][4])

IQRs <- tapply(data$weight.loss, data$diet.type, IQR)

Lowers <- Q1s - 1.5*IQRs
Uppers <- Q3s + 1.5*IQRs
datas <- split(data, data$diet.type)
data_no_outlier <- NULL
for (i in 1:3){
  out <- subset(datas[[i]], datas[[i]]$weight.loss > Lowers[i] & datas[[i]]$weight.loss < Uppers[i])
  data_no_outlier <- rbind(data_no_outlier, out)
}

dim(data)
dim(data_no_outlier)

boxplot(weight.loss~diet.type,data=data_no_outlier,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

library(gplots)
plotmeans(weight.loss ~ diet.type, data=data_no_outlier)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

fit <- aov(weight.loss ~ diet.type, data=data_no_outlier)
summary(fit)

TukeyHSD(fit)

#Tukey honest significant differences test)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
#повторно проверсти все тесты и сравнить результаты с выбросами и без
#Открыть документ https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#и попытаться выполнить задания из него