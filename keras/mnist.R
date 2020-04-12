# ----------------------- Dependencies -----------------------

library(keras)
library(tidyverse)

# ----------------------- Sequential model --------------------------

# load dataset
mnist <- dataset_mnist()

# split train and testing data
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# preparing the features
x_train <- array_reshape(x_train, c(60000, 28 * 28))
x_train <- x_train/255

x_test <- array_reshape(x_test, c(10000,28 * 28))
x_test <- x_test/255

# preparing the labels
y_train <- to_categorical(y_train)
y_test <- to_categorical(y_test)

# create model
model <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>% 
  layer_dense(units = 10, activation = "softmax")

# compile data
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "rmsprop",
  metrics = c("accuracy")
)

# fit the model
model %>% fit(x_train, y_train, epochs = 5, batch_size = 128)

# predict with new values
model %>% 
  predict_classes(x_test[1:10,])

# compute errors (loss and accuracy)
metrics <- model %>% 
  evaluate(x_test, y_test)

# -------------------------------------------------


