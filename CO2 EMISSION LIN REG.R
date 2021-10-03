data1<-read.csv("C:\\Users\\Jyothi Nadig\\Desktop\\BA Trim 2\\MLA-1\\Cia\\CO2 Emissions_Canada.csv", stringsAsFactors = TRUE)

View(data1)
str(data1)
dim(data1)

library(caTools)
set.seed(100)
split<-sample.split(data1$CO2.Emissions.g.km., SplitRatio = 0.7)
summary(split)
train_data1<-subset(data1, split == TRUE)
test_data1<-subset(data1, split == FALSE)

cor.test(data1$CO2.Emissions.g.km., data1$Engine.Size.L.)
cor.test(data1$CO2.Emissions.g.km., data1$Cylinders)
cor.test(data1$CO2.Emissions.g.km., data1$Fuel_City)
cor.test(data1$CO2.Emissions.g.km., data1$Fuel_Highway)
cor.test(data1$CO2.Emissions.g.km., data1$Fuel_comb)
cor.test(data1$CO2.Emissions.g.km., data1$Fuel_Comb.mpg.)

library(ggplot2)
ggplot(data1, aes(data1$CO2.Emissions.g.km., data1$Engine.Size.L.))+geom_point()
ggplot(data1, aes(data1$CO2.Emissions.g.km., data1$Cylinders))+geom_point()
ggplot(data1, aes(data1$CO2.Emissions.g.km., data1$Fuel_City))+geom_point()
ggplot(data1, aes(data1$CO2.Emissions.g.km., data1$Fuel_Highway))+geom_point()
ggplot(data1, aes(data1$CO2.Emissions.g.km., data1$Fuel_comb))+geom_point()
ggplot(data1, aes(data1$CO2.Emissions.g.km., data1$Fuel_Comb.mpg.))+geom_point()

levels(data1$Fuel.Type)
ggplot(data1, aes(data1$Fuel.Type, data1$CO2.Emissions.g.km.))+geom_line()

reg1<-lm(CO2.Emissions.g.km.~
           Engine.Size.L.+ # not sig
           Cylinders+
           Fuel_City+ # high vif
           Fuel_Highway+ # high vif
           Fuel_Comb.mpg.+
           Fuel_comb+ # high vif
           Fuel.Type+
           Vehicle.Class, data = train_data1) # 7 not sig
summary(reg1)

levels(data1$Transmission)

library(car)
vif(reg1)

library(lmtest)
dwtest(reg1)

cor_df<-data1[, c(8,9,10)]
head(cor_df)
cor_mat<-cor(cor_df)
round(cor_mat,2)

reg2<-lm(CO2.Emissions.g.km.~
           Engine.Size.L.+
           Cylinders+
           Fuel_City+
           Fuel_Comb.mpg.+
           Fuel.Type+
           Vehicle.Class, data = train_data1)
summary(reg2)


dwtest(reg2)
vif(reg2)

library(moments)
skewness(reg2$residuals)
kurtosis(reg2$residuals)

plot(reg2$residuals, reg2$fitted.values)
plot(reg2$residuals, c(1:length(reg2$residuals)))

x<-reg2$residuals
hist(x)

influenceIndexPlot(reg2)

train_data2 = train_data1[-c(1326,1330,2440,5162),]

reg2<-lm(CO2.Emissions.g.km.~
           Engine.Size.L.+
           Cylinders+
           Fuel_City+
           Fuel_Comb.mpg.+
           Fuel.Type+
           Vehicle.Class, data = train_data2)
summary(reg2)

library(lmtest)
dwtest(reg2)
vif(reg2)

pred_train<-predict(reg2, train_data2)

pred_co2<-predict(reg2, test_data1)
new_data<-data.frame(test_data1, pred_co2)

library(caret)

RMSE(test_data1$CO2.Emissions.g.km., pred_co2)

pred
