# --------------------- Dependencies -------------------------------

library(keras)

# --------------- Prepare the data ---------------------

max_features <- 1000
max_len <- 500
batch_size <- 32

# load the dataset
imbd <- dataset_imdb(num_words = 1000)

# separe train and test set
c(c(train_inputs, train_target), c(test_inputs, test_target)) %<-% imbd 

# ----------------------------- Preprocess data --------------------

train_inputs <- pad_sequences(train_inputs, maxlen = max_len)
test_inputs <- pad_sequences(test_inputs, maxlen = max_len)

# --------------------------- Create RNN -----------------------------

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer="rmsprop",
  loss= "binary_crossentropy",
  metrics=c("accuracy")
)

history <- model %>% fit(
  train_inputs,
  train_target,
  epochs=5, 
  batch_size=128,
  validation_split=0.02
)

model %>% evaluate(test_inputs, test_target, batch_size=32)











