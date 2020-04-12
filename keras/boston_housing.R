# --------------------- dependecies -------------

library(keras)
library(tidyverse)

# --------------- Regression ------------

# load dataset
boston <- dataset_boston_housing()

# separate training and testing data
c(c(train_features, train_labels), c(test_features, test_labels)) %<-% boston

# prepare features . note: why use training set mean and std for test features?
mean <- apply(train_features, 2, mean)
std <- apply(train_features, 2, sd)
train_features <- scale(train_features, center = mean, scale = std)
test_features <- scale(test_features, center = mean, scale = std)

# prepare labels no need:)

# build model
build_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", input_shape = dim(train_features)[[2]]) %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer="rmsprop",
    loss="mse",
    metrics=c("mae")
  )
}

# evaluate model using cross validation (to see epochs before overfitting)
k <- 4
indices <- sample(1:nrow(train_features))
folds <- cut(indices, breaks = k, labels = FALSE)
nums_epochs <- 500
all_mae_histories <- NULL
for(i in 1:k){
  cat("processing fold #", i, "\n")
  
  # create training val set from training set
  val_indices <- which(folds==i, arr.ind = TRUE)
  train_val_features <- train_features[val_indices,]
  train_val_labels <- train_labels[val_indices]
  
  # create testing val set from training set
  test_val_features <- train_features[-val_indices,]
  test_val_labels <- train_labels[-val_indices]
  
  # build model
  model <- build_model()
  
  # fit model with train val set
  history <- model %>% fit(train_val_features, train_val_labels, epochs=nums_epochs, batch_size=1, verbose=0)
  
  # errors across epochs
  mae_history <- model %>% evaluate(test_val_features, test_val_labels)
  
  # bind epochs scores
  all_mae_histories <- rbind(all_mae_histories, mae_history)
  
}

# build the average mae history
average_mae_history <- data.frame(
  epoch = seq(1:nrow(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

# plot validation scores
average_mae_history %>% 
  ggplot(aes(epoch, validation_mae))+geom_line()+geom_smooth()

# train the final model and evaluate errors with new values
model <- build_model()
model %>% fit(train_features, train_labels, epochs=80, batch_size=16, verbose=0)
result <- model %>% evaluate(test_features, test_labels)
predictions <- model %>% predict(test_features)
