data<-read.csv("C:\\Users\\Jyothi Nadig\\Desktop\\BA Trim 2\\MLA-1\\Cia\\CIA 3\\Co2_final.csv", stringsAsFactors = TRUE)

str(data)
#View(data)

# sub-setting all scale X's
subdata<-data[,c(5,6,9:14)]
str(subdata)
#View(subdata)

library(psych)
pairs.panels(subdata[1:7],
             gap=0,
             pch=21)

library(MVN)
multi_norm<-mvn(subdata, subset = "Vehicle.Size", mvnTest = "hz", showOutliers = TRUE)
multi_norm # first attempt with no transformation

# log transforming the predictor variables 
subdata_2<-log(subdata[1:5]) 
Vehicle.Size=subdata$Vehicle.Size
sub_log<-cbind(subdata_2, Vehicle.Size)

multi_norm2<-mvn(sub_log, subset = 'Vehicle.Size', mvnTest = "hz", showOutliers = TRUE)
multi_norm2 # second attempt

library(LambertW)
#?Gaussianize

# gausiannizing the predictors
gausian<-Gaussianize(subdata_2)
sub_gausian<-cbind(gausian, Vehicle.Size)

multi_norm3<-mvn(sub_gausian, subset = 'Vehicle.Size', mvnTest = "hz", showOutliers = TRUE)
multi_norm3 # third attempt

# checking for outliers
multi_norm2$multivariateOutliers

# BOX-M TEST -- HOMOGENITY OF VARIANCE
library(biotools)
boxM(subdata[1:7],subdata$Vehicle.Size)

library(MASS)
# discriminant analysis model
lda_model<-lda(Vehicle.Size~.,data = subdata)
summary(lda_model)
lda_model

library(DiscriMiner)
lda_model2<-linDA(subdata[1:7],subdata$Vehicle.Size)
lda_model2

#?discPower
discPower(subdata[1:7],subdata$Vehicle.Size)

# diagnostics with canonical correlation
library(candisc)
reg3<-lm(as.matrix(subdata[1:7])~subdata$Vehicle.Size)
regcanno<-candisc(reg3)
regcanno

# model evaluation
p<-predict(lda_model, subdata)
ldahist(data = p$x[,1], subdata$Vehicle.Size)

#################################################################################

# DECISION TREE AND RANDOM FOREST 

library(caret)
library(tree)
library(partykit)
library(rpart.plot)
library(rpart)

tree_data<-data[,-c(1:4)] # removing unique and parent column
#View(data1)

# data partition
set.seed(100)
split1<-createDataPartition(tree_data$Vehicle.Size,
                            times = 1,
                            p = 0.70,
                            list = FALSE,
                            groups = min(50,length(tree_data$Transmission))) 
train_data<-tree_data[split1,]
test_data<-tree_data[-split1,]

# first tree model
#?rpart
tree_1<-rpart(Vehicle.Size~.,
              minsplit=100,
              xval = 5,
              maxdepth=10,
              method = 'class',
              data = train_data)


tree_1
printcp(tree_1)
plotcp(tree_1)

# final tree model after tuning
tree_2<-rpart(Vehicle.Size~
                CO2.Emissions.g.km.+
                Transmission+
                Fuel_City+
                Fuel_Highway+
                Engine.Size.L.,
              minsplit=100,
              xval=5,
              maxdepth=5,
              cp=0.011,
              method = 'class',
              data = train_data)

summary(tree_2)
printcp(tree_2)
plotcp(tree_2)
varImp(tree_2)

rpart.plot(tree_2)

#testing and evaluation 
tree2_accu<-predict(tree_2, newdata = train_data, type = 'class')

confusionMatrix(tree2_accu, train_data$Vehicle.Size)

tree_pred2<-predict(tree_2, newdata = test_data, type = 'class')
summary(tree_pred2)
confusionMatrix(tree_pred2, test_data$Vehicle.Size)

######################################################################################

library(randomForest)

set.seed(123)

# preliminary model
rf <- randomForest(Vehicle.Size~., data = train_data)

print(rf)
rf$importance

# preliminary model evaluation
rf_p1 <- predict(rf, newdata = train_data)
confusionMatrix(rf_p1, train_data$Vehicle.Size)

rf_p2 <- predict(rf, newdata = test_data)
confusionMatrix(rf_p2, test_data$Vehicle.Size)

# tuning the model
plot(rf)

rf_val <- tuneRF(train_data[,-10],train_data[,10],
                 stepFactor = 0.1,
                 plot = TRUE,
                 ntreeTry = 250,
                 trace = TRUE,
                 improve = 0.01)

# final model
rf2 <- randomForest(Vehicle.Size~., data = train_data,
                    ntree = 300,
                    mtry = 3,
                    importance = TRUE,
                    proximity = TRUE)

print(rf2)

# final model evaluation
rf_p3 <- predict(rf2, newdata = test_data)
confusionMatrix(rf_p3, test_data$Vehicle.Size)

varImpPlot(rf)

varUsed(rf)

######################################################################################

#KNN

# sub-setting data
knn_data<-data[,-c(1:4,7,8)]
str(knn_data)

set.seed(100)
split2<-createDataPartition(knn_data$Vehicle.Size,
                            times = 1,
                            p = 0.70,
                            list = FALSE)
train_knn<-knn_data[split2,]
test_knn<-knn_data[-split2,]

# cross validation parameter
fitcontrol<-trainControl(method = "repeatedcv", number =10, repeats = 3)

set.seed(123)
knn_model<-train(Vehicle.Size~
                   CO2.Emissions.g.km.+
                   Fuel_City+
                   Fuel_Highway+
                   Fuel_comb, 
                data = train_knn, 
                method= "knn", 
                trControl= fitcontrol, 
                preProcess= c("center", "scale"), 
                tuneLength= 20)
print(knn_model)
plot(knn_model)

varImp(knn_model)

# testing and model evaluation 
knn_pred <- predict(knn_model, newdata = test_knn)
summary(knn_pred)
confusionMatrix(knn_pred, test_knn$Vehicle.Size)
