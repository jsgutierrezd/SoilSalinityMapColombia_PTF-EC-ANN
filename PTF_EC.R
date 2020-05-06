setwd("~/PTF_EC")

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
                  sheet = "PTF_V1") %>% data.frame

names(dat)
dat <- dat[,c(11,12,14,15,18:21)]
#dat <- dat[,-c(1:10,13,15:17,22:26)]
names(dat)
summary(dat)

dat1 <- dat[complete.cases(dat),]
dim(dat1)
summary(dat1)
cor(dat1,method = "spearman")
cor(dat1)
dat1 %>% as.matrix() %>% rcorr(type="pearson")
dat1 %>% as.matrix() %>% rcorr(type="spearman")

x11()
corrplot(cor(dat1), method="number")
pairs(dat1)

sc <- scale(dat1)
dat1 <- data.frame(elco=dat1$elco,sc[,-3])
head(dat1)
dim(dat1)

set.seed(0324)
inTrain <- createDataPartition(y = dat1$elco, p = .80, list = FALSE)
train_data <- as.matrix(dat1[ inTrain,-1])
train_labels <- as.matrix(dat1[ inTrain,1])

dim(train_data)
#528  10

test_data <- dat1[-inTrain,-1] %>% as.matrix()
test_labels <- dat1[-inTrain,1] %>% as.matrix()
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
    #layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.01),
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
  coord_cartesian(ylim = c(0, 4))

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
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 4))


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
#View(dat)
dat <- dat[,-c(1:10,13,15:17,22:26)]
names(dat)
summary(dat)

dat1 <- dat[complete.cases(dat),]
dim(dat1)

names(dat1)
str(dat1)

dim(dat1)
#1514  7

set.seed(720)
inTrain <- createDataPartition(y = dat1$elco, p = .70, list = FALSE)
data.training <- dat1[ inTrain,] 

dim(data.training)
#1060   7

data.validation <- dat1[-inTrain,]
dim(data.validation)
#452   7

library(raster)
library(caret)
library(quantregForest)
require(randomForest)
library(doParallel)
library(randomForest)
library(quantregForest)
library(quantreg)
names(dat1)

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
(rfmodel <- rfe(x=dat1[,-c(3,7)], y=dat1[,3], sizes=c(1:6), rfeControl=control2))
x11()
plot(rfmodel, type=c("g", "o"))
predictors(rfmodel)[1:5]
stopCluster(cl = cl)
endCluster()
names(dat1)
hist(dat1)
plot(dat1$Na,dat1$elco)


model_rf <- caret::train(y=data.training[,3],
                         x=data.training[,predictors(rfmodel)[1:5]],
                         metric="RMSE",
                         data=data,
                         trControl = trainControl(method = "cv",savePredictions = T),
                         method = "qrf"
)
x11()
varImpPlot(model_rf[11][[1]])
x11()
plot(model_rf[11][[1]])

str(model_rf)

pred <- predict(model_rf, data.validation, quantiles=c(0.5,0.75),)
(cor(pred,data.validation$elco))^2


rmse(pred,data.validation$elco)
sqrt(model_rf$finalModel$mse)

str(model_rf)
model_rf$bestTune


saveRDS(rf, "Model_elco_04052020.rds")
##super_model <- readRDS("finalmodel.rds")



qrf <- quantregForest(x=data.training[,predictors(rfmodel)[1:5]], y=data.training[,3],
                      nodesize=10,sampsize=30)
(conditionalQuantiles <- predict(qrf, data.validation))
dim(data.validati
