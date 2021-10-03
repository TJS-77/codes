data<-read.csv("C:\\Users\\Jyothi Nadig\\Desktop\\BA Trim 2\\MLA-1\\Cia\\CIA 2\\Co2_final.csv",stringsAsFactors = TRUE)

str(data)
View(data)

data1<-data[,-c(1,2,3,4)]
str(data1)

levels(data1$Vehicle.Size)

library(caTools)
set.seed(100)
split1<-sample.split(data1$Vehicle.Size, SplitRatio = 0.70)
train_data<-subset(data1,split1 == TRUE)
test_data<-subset(data1,split1 == FALSE)

library(caret)
train_model0<-train(Vehicle.Size~.,
                   method = "glm",
                   family = "binomial",
                   data = train_data)
summary(train_model0)

#logreg_frwd<-blr_step_aic_forward(train_model$finalModel,details = TRUE)

train_model<-train(Vehicle.Size~Engine.Size.L.+
                     Cylinders+
                     Fuel.Type+
                     Fuel_City+
                     Fuel_comb+
                     Fuel_Comb.mpg.,
                     method = "glm",
                     family = "binomial",
                     data = train_data)
train_model
summary(train_model)
train_model1$finalModel

library(blorr)
blr_model_fit_stats(train_model$finalModel)
blr_test_hosmer_lemeshow(train_model$finalModel)

gt<-blr_gains_table(train_model$finalModel)
gt
blr_roc_curve(gt)

prediction_model<-predict(train_model, test_data)
summary(prediction_model)
confusionMatrix(prediction_model, test_data$Vehicle.Size)



