library(readxl)
library(caTools)
library(tidyverse)
library(caret)
library(ggplot2)

data<-read_xlsx("C:\\Users\\Jyothi Nadig\\Desktop\\SIP - Task III\\grips.xlsx")
View(data)
cor.test(data$hours, data$scores)
plot(data$hours, data$scores)

reg_model<-lm(data$scores~data$hours, data = data)
summary(reg_model)
target<- (2.4837 + (9.7758 * 9.25))
target

# 92.90 == 93
