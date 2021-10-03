data <- read.csv("C:\\Users\\Jyothi Nadig\\Desktop\\BA Trim 2\\MLA-1\\Cia\\Co2_final.csv")

# REMOVING UNWANTED COLUMNS
subdata<-data[,-c(1,2,3,4)]
str(subdata)

# CONVERTING DEPENDENT VARIABLE TO FACTOR
subdata$Vehicle.Size <- as.factor(subdata$Vehicle.Size)

# DATA PARTITION
library(caret)
set.seed(100)
split1<-createDataPartition(subdata$Vehicle.Size,
                            times = 1,
                            p = 0.70,
                            list = FALSE,
                            groups = min(50,length(subdata$Transmission))) 
train_data<-subdata[split1,]
test_data<-subdata[-split1,]

library(e1071)

# LINEAR METHOD
model_1<-svm(Vehicle.Size~.,
             type = 'C',
             cross = 5,
             data = train_data,
             kernel = "linear",
             scale = TRUE)
summary(model_1)

# POLYNOMIAL MODEL
model_2<-svm(Vehicle.Size~.,
             type = 'C',
             cross = 5,
             data = train_data,
             kernel = "polynomial",
             scale = TRUE)
summary(model_2)

# RADIAL METHOD
model_3<-svm(Vehicle.Size~.,
             type = 'C',
             cross = 5,
             data = train_data,
             kernel = "radial",
             scale = TRUE)
summary(model_3)


# TRYING DIFFERENT COST PARAMETERS FOR RADIAL KERNEL
model_final <- svm(Vehicle.Size~.,
                   scale = TRUE,
                   type = 'C',
                   kernel = 'radial',
                   data = train_data,
                   cross = 10,
                   cost = 0.1)
summary(model_final)

model_final2 <- svm(Vehicle.Size~.,
                   scale = TRUE,
                   type = 'C',
                   kernel = 'radial',
                   data = train_data,
                   cross = 10,
                   cost = 0.25)
summary(model_final2)

model_final3 <- svm(Vehicle.Size~.,
                   scale = TRUE,
                   type = 'C',
                   kernel = 'radial',
                   data = train_data,
                   cross = 10,
                   cost = 0.5)
summary(model_final3)

model_final4 <- svm(Vehicle.Size~.,
                   scale = TRUE,
                   type = 'C',
                   kernel = 'radial',
                   data = train_data,
                   cross = 10,
                   cost = 0.75)
summary(model_final4) # SELECTED FINAL MODEL

model_final5 <- svm(Vehicle.Size~.,
                    scale = TRUE,
                    type = 'C',
                    kernel = 'radial',
                    data = train_data,
                    cross = 10,
                    cost = 0.9)
summary(model_final4)

# PREDICTION AND EVALUATION
pred <- predict(model_final4, newdata = test_data)
confusionMatrix(pred, test_data$Vehicle.Size)
