setwd("~/PTF_EC")
<<<<<<< HEAD
library(readxl)
library(magrittr)
library(corrplot)
library(Hmisc)
#install.packages("reticulate")
library(reticulate)
library(caret)

# devtools::install_github("rstudio/keras",force = T)
#install_keras()
library(keras)


dat <- read_excel("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\BASES\\BASE_NAL_2020.xlsx",
                  sheet = "PTF_V0") %>% data.frame

names(dat)
dat <- dat[,-c(1,2,3,4,5,9,10,13,15,17,22,23,25,26)]
names(dat)
summary(dat)

dat1 <- dat[complete.cases(dat),]
dim(dat1)
summary(dat1)
cor(dat1,method = "spearman")
cor(dat1)
dat1 %>% as.matrix() %>% rcorr(type="pearson")
dat1 %>% as.matrix() %>% rcorr(type="spearman")

dat2 <- dat[,-4]
dat2 <- dat2[complete.cases(dat2),]
dim(dat2)
summary(dat2)
cor(dat2,method = "spearman")
cor(dat2)
dat2 %>% as.matrix() %>% rcorr(type="pearson")
dat2 %>% as.matrix() %>% rcorr(type="spearman")
x11()
corrplot(cor(dat2), metod="circle")

sc <- scale(dat2)
dat2 <- data.frame(elco=dat2$elco,sc[,-5])
head(dat2)
dim(dat2)

set.seed(0324)
inTrain <- createDataPartition(y = dat2$elco, p = .80, list = FALSE)
train_data <- as.matrix(dat2[ inTrain,-1])
train_labels <- as.matrix(dat2[ inTrain,1])

dim(train_data)
#528  10

test_data <- dat2[-inTrain,-1] %>% as.matrix()
test_labels <- dat2[-inTrain,1] %>% as.matrix()
dim(test_data)
#132  10


train_data[1, ] 
#==================================================
#====DEEP LEARNING-NEURAL NETWORK-KERAS PACKAGE====
#==================================================
# First training sample, normalized
######optimizer_rmsprop(lr=0.01)
######optimizer_adagrad(lr=0.01)
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adagrad(lr=0.01),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat("_")
  }
)

epochs <- 250

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

# library(ggplot2)
# #install.packages("digest",dep=T)
# library(digest)
plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 1))



# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)


plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 1))


c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))



test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]
(rmse <- sqrt(loss))

test_labels[,1]
test_predictions[ , 1]

#install.packages("hydroGOF")
library(hydroGOF)
hydroGOF::rmse(test_predictions[ , 1],test_labels[,1])
hydroGOF::mse(test_predictions[ , 1],test_labels[,1])
(cor(test_predictions[ , 1],test_labels[,1]))^2




#==================================================
#====MACHINE LEARNING - RANDOM FOREST==============
#==================================================
library(readxl)
library(rpart.plot)
library(rpart)
library(caret)
library(hydroGOF)
library(readxl)
library(biotools)
library(magrittr)
library(plyr)
library(dplyr)

dat <- read_excel("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\BASES\\BASE_NAL_2020.xlsx",
                  sheet = "PTF_V0") %>% data.frame

names(dat)
dat <- dat[,-c(1,2,3,4,5,9,10,13,15,17,22,23,25,26)]
names(dat)
summary(dat)

dat1 <- dat[complete.cases(dat),]
dim(dat1)

names(dat1)
str(dat1)

dim(dat1)
#790  10

set.seed(922)
inTrain <- createDataPartition(y = dat1$elco, p = .80, list = FALSE)
data.training <- dat1[ inTrain,] 

dim(data.training)
#304  12

data.validation <- dat1[-inTrain,]
dim(data.validation)
#76 12

library(raster)
library(caret)
library(quantregForest)
require(randomForest)
library(doParallel)
library(randomForest)

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
(rfmodel <- rfe(x=dat1[,c(-6)], y=dat1[,6], sizes=c(1:6), rfeControl=control2))
x11()
plot(rfmodel, type=c("g", "o"))
predictors(rfmodel)[1:5]
stopCluster(cl = cl)
endCluster()
names(dat1)

ctrl <- trainControl(method = "cv",savePredictions = T)
# Search for the best mtry parameter 
model_rf <- caret::train(y=dat1[,6],
                         x=dat1[,c(8,4,5,9,7)],
                         metric="RMSE",
                         data=data,
                         trControl = ctrl,
                         method = "rf"
)
x11()
varImpPlot(model_rf[11][[1]])
x11()
plot(model_rf[11][[1]])

str(model_rf)

pred <- predict(model_rf, data.validation)
(cor(pred,data.validation$elco))^2


rmse(pred,data.validation$elco)
sqrt(model_rf$finalModel$mse)

str(model_rf)
model_rf$bestTune

saveRDS(model_rf, "Model_elco_23042020.rds")
##super_model <- readRDS("finalmodel.rds")

library(randomForest)
rf <- randomForest(x = dat1[,predictors(model_rf)], y =dat1[,6],ntree=500,mtry=2)
head(getTree(rf, k=50, labelVar = TRUE))
#x11()
varImpPlot(rf, main = "Clase por pedregosidad superficial")
pred <- predict(rf, data.validation)
(cor(pred,data.validation$elco))^2
