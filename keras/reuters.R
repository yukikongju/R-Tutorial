# ------------------------------- Dependecies

library(keras)
library(tidyverse)

# ----------- Single label Classifier ----------------

# load dataset
reuters <- dataset_reuters(num_words = 10000)

# separe training and testing data
c(c(train_features, train_labels), c(test_features, test_labels)) %<-% reuters

# decode newswires back to text
word_index <- dataset_reuters_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index
decoded_newswires <- sapply(train_features[[1]], function(index){
  word <- if(index>=3) reverse_word_index[[as.character(index-3)]]
  if(!is.null(word)) word else"?"
})

# prepare features
vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}
x_train <- vectorize_sequences(train_features)
x_test <- vectorize_sequences(test_features)

# encoding the labels
one_hot_train_labels <- to_categorical(train_labels)
one_hot_test_labels <- to_categorical(test_labels)

length(train_features)

# create the model
model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 46, activation = "softmax")

# compile model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer="rmsprop",
  metrics=c("accuracy")
)

# validation with val set
val_indices <- 1:1000
x_val <- x_train[val_indices,]   
partial_x_train <- x_train[-val_indices,]
y_val <- one_hot_train_labels[val_indices,]
partial_y_train <- one_hot_train_labels[-val_indices,]

# train val set 
history <- model %>% fit(
  partial_x_train, 
  partial_y_train,
  epochs=20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

plot(history)

# compute errors
errors <- model %>% evaluate(x_test, one_hot_test_labels)

# predict with new variables
predictions <- model %>% predict(x_test)
sum(predictions[1,]) # all weights sums for an observations sum up to 1 
which.max(predictions[1,]) # show the prediction based on probabilities for obs 1


