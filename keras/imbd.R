# ------------------------------- Dependecies

library(keras)
library(tidyverse)

# ----------- Binary Classifier ----------------

# load data
imbd <- dataset_imdb(num_words = 10000)

# separe train and test dataset
c(c(train_features, train_labels), c(test_features, test_labels)) %<-% imbd

# prepare features: encoding integer sequences into binary matrix
# note: all sequences with 1 and the rest filled with 0
vectorize_sequences <- function(sequences, dimension=10000){
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  
  for (i in 1:length(sequences)){
    results[i, sequences[[i]]] <- 1
  }
  
  results
}

x_train <- vectorize_sequences(train_features)
x_test <- vectorize_sequences(test_features)

# prepare labels
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

# create the model
model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

# compile the model
model %>% compile(
  optimizer= optimizer_rmsprop(lr=0.001),
  loss="binary_crossentropy",
  metrics=c("accuracy")
)

# fit the model
model %>% 
  fit(x_train, y_train, epochs=5, batch_size=512) 

# predict with new value
model %>% predict(x_test[1:10,])

# calculate error
errors <- model %>% 
  evaluate(x_test, y_test)
