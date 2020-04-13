# ------------------ Dependencies ------------

library(keras)

# ----------- Convolutional Neural Networks ----------

# load data
mnist <- dataset_mnist()

# separate train and test set
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist 

# reshape features data
train_images <- array_reshape(train_images, c(60000,28,28,1))
train_images <- train_images/255

test_images <- array_reshape(test_images, c(10000,28,28,1))
test_images <- test_images/255

# prepare labels data
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

# building the model
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                input_shape = c(28,28,1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")

# adding classifier over cnn
model <- model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

# compile cnn
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# fit model
model %>% fit(train_images, train_labels, epochs=5, batch_size=64)

# evaluate model
errors <- model %>% evaluate(test_images, test_labels)

# save model
model %>% save_model_hdf5("mnist.h5")
