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
dat <- dat[,c(12,14,15,18:21)]
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


#==================================================
#====DEEP LEARNING-NEURAL NETWORK-KERAS PACKAGE====
#==================================================

set.seed(0324)
inTrain <- createDataPartition(y = dat1$elco, p = .70, list = FALSE)
train_data <- as.matrix(dat1[ inTrain,-1])
train_labels <- as.matrix(dat1[ inTrain,1])

dim(train_data)
#528  10

test_data <- dat1[-inTrain,-1] %>% as.matrix()
test_labels <- dat1[-inTrain,1] %>% as.matrix()
dim(test_data)
#132  10


train_data[1, ] 

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
    cat("-RuNnInG-")
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




model %>% save_model_hdf5("PTF_v1NN.h5")

new_model <- load_model_hdf5("my_model.h5")
new_model %>% summary()



#==================================================
#====MACHINE LEARNING - RANDOM FOREST==============
#==================================================
library(caret)
library(quantregForest)
library(randomForest)
library(doParallel)
library(ranger)


set.seed(720)
inTrain <- createDataPartition(y = dat1$elco, p = .70, list = FALSE)
data.training <- dat1[ inTrain,] 

dim(data.training)


data.validation <- dat1[-inTrain,]
dim(data.validation)

names(dat1)

model_rf <- caret::train(y=data.training[,2],
                         x=data.training[,-2],
                         metric="RMSE",
                         data=data,
                         trControl = trainControl(method = "cv",savePredictions = T),
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


modelo_ranger <- ranger(
  formula=elco~.,
  data=data.training,
  num.trees = 500,
  mtry = 2,
  quantreg = F,
  # Se emplean todos los cores disponibles -1
  num.threads = future::availableCores() - 1,
  seed = 123
)
pred <- predict(modelo_ranger, data.validation)

(cor(pred$predictions,data.validation$elco))^2
rmse(pred$predictions,data.validation$elco)

#===============================================================
#====MACHINE LEARNING - SUPPORT VECTOR MACHINES=================
#===============================================================
library(e1071)
library(caret)
library(kernlab)

rctrl2 <- trainControl(method = "LOOCV")

model_svm.lin <- caret::train(y=data.training[,2],
                         x=data.training[,-2],
                         data=data,
                         trControl = trainControl(method = "LOOCV",savePredictions = T),
                         method = "svmLinear",
                         preProc = c("center", "scale")
)


model_svm.lin

model_svm.lin$pred
pred <- predict(model_svm.lin,data.validation[,-2])
(cor(pred,data.validation$elco))^2
rmse(pred,data.validation$elco)

#===============================================================
#====MACHINE LEARNING - SUPER LEARNER-MODELS ENSEMBLE===========
#===============================================================
#install.packages("SuperLearner",dep=T)
library(SuperLearner)
library(ranger)
library(kernlab)

listWrappers()

names(data.training)

SL.ranger.mod <- function(...){
  SL.ranger(..., num.trees=500, mtry=2,quantreg=T)
}

SL.ksvm.mod <- function(...){
  SL.ksvm(..., C=1, kernel="vanilladot", scaled=T)
}

ens.model.mod <- SuperLearner(Y=data.training[,2],
                             X=data.training[,-2],
                             family=gaussian(),
                             SL.library=list("SL.ranger.mod","SL.ksvm.mod"))
ens.model.mod
pred.mod = predict(ens.model.mod, data.validation[,-2], onlySL = TRUE)
(cor(pred.mod$pred,data.validation$elco))^2
rmse(pred.mod$pred,data.frame(data.validation$elco))





ens.model <- SuperLearner(Y=data.training[,2],
                          X=data.training[,-2],
                          family=gaussian(),
                          SL.library=list("SL.ranger","SL.ksvm"))


ens.model
pred = predict(ens.model, data.validation[,-2], onlySL = TRUE)
(cor(pred$pred,data.validation$elco))^2
rmse(pred$pred,data.frame(data.validation$elco))




cv_sl.mod <- CV.SuperLearner(Y = data.training[,2],
                        X = data.training[,-2],
                        family = gaussian(),
                        V = 5,
                        SL.library = c("SL.ranger.mod","SL.ksvm.mod"))

summary(cv_sl.mod)

cv_sl <- CV.SuperLearner(Y = data.training[,2],
                        X = data.training[,-2],
                        family = gaussian(),
                        V = 5,
                        SL.library = c("SL.ranger","SL.ksvm"))

summary(cv_sl)


#============================================================
#====EXTERNAL VALIDATION AGROSAVIA TOPSOIL SAMPLES===========
#============================================================
dat_val <- read_excel("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\BASES\\BASE_NAL_2020.xlsx",
                  sheet = "PTF_V0") %>% data.frame
dim(dat_val)
names(dat_val)
dat_val <- dat_val[,c(12,14,15,18:21)]
names(dat_val)
dat_val <- na.omit(dat_val)

pred_total <- predict(ens.model,dat_val[,-2],onlySL=T)
dat_val <- data.frame(dat_val,pred=pred_total$pred)
View(dat_val)

(cor(dat_val$elco,dat_val$pred))^2
rmse(dat_val$elco,dat_val$pred)
plot(dat_val$pred,dat_val$elco)