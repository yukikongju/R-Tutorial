# ------------------- Dependencies ---------------

library(keras)

# ----------------- Useful Links -----------------

# https://towardsdatascience.com/image-classifier-cats-vs-dogs-with-convolutional-neural-networks-cnns-and-google-colabs-4e9af21ae7a8

# ----------------- Load data -----------------
original_dataset_dir <- "E:/kaggle_original_data/dogs-vs-cats/train/train"

base_dir <- "E:/kaggle_original_data/dogs-vs-cats-small"
dir.create(base_dir)

train_dir <- file.path(base_dir, "train")
dir.create(train_dir)
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir)
test_dir <- file.path(base_dir, "test")
dir.create(test_dir)

train_cats_dir <- file.path(train_dir, "cats")
dir.create(train_cats_dir)

train_dogs_dir <- file.path(train_dir, "dogs")
dir.create(train_dogs_dir)

validation_cats_dir <- file.path(validation_dir, "cats")
dir.create(validation_cats_dir)

validation_dogs_dir <- file.path(validation_dir, "dogs")
dir.create(validation_dogs_dir)

test_cats_dir <- file.path(test_dir, "cats")
dir.create(test_cats_dir)

test_dogs_dir <- file.path(test_dir, "dogs")
dir.create(test_dogs_dir)

fnames <- paste0("cat.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames), 
          file.path(train_cats_dir)) 

fnames <- paste0("cat.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames), 
          file.path(validation_cats_dir))

fnames <- paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_cats_dir))

fnames <- paste0("dog.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_dogs_dir))

fnames <- paste0("dog.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_dogs_dir)) 

fnames <- paste0("dog.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_dogs_dir))

# --------------- Read Image from directories and Preprocess -------------------

train_data <- image_data_generator(rescale = 1/255)
train_generator <- flow_images_from_directory(
  train_dir,
  train_data,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

test_data <- image_data_generator(rescale = 1/255)
test_generator <- flow_images_from_directory(
  test_dir,
  test_data,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

validation_data <- image_data_generator(rescale = 1/255)
validation_generator <- flow_images_from_directory(
  validation_dir,
  validation_data,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

batch <- generator_next(train_generator)

# ----------------------- Create Convolutional Neural Network ------------------------

model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr =  1e-4),
  metrics=c("acc")
)

# --------------- Fit CNN-------------------

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 10,
  validation_data = validation_generator,
  validation_steps = 50
)

plot(history)

model %>% save_model_hdf5("cats_dogs_cnn.h5")

# ----------- Evaluate CNN ----------------

errors <- model %>% 
  evaluate_generator(test_generator, steps = 50)

# -------------- Data Augmentation -----------------
# note: create more data by duplicating images with some distortion

# choose image to reshape and resize it
fnames <- list.files(train_cats_dir, full.names = TRUE)
img_path <- fnames[3]
img <- image_load(img_path, target_size = c(150,150))
img_array <- image_to_array(img)
img_array <- array_reshape(img_array, dim = c(1, 150,150,3))

# create transformation function
datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "neareast"
)

# transfomr image
augmentation_generator <- flow_images_from_data(
  img_array,
  generator = datagen,
  batch_size = 1
)

op <-  par(mfrow=c(2,2), pty= "s", mar=c(1,0,1,0))
for (i in 1:4) {
  batch <- generator_next(augmentation_generator)
  plot(as.raster(batch[1]))
}
par(op)

# ----------- Visualizing intermediate layers -------

# load image and rescale
fnames <- list.files(train_cats_dir, full.names = TRUE)
img_path <- fnames[28]
img <- image_load(img_path, target_size = c(150,150))
img_tensor <- image_to_array(img) 
img_tensor <- array_reshape(img_tensor,dim = c(1,150,150,3))
img_tensor <- img_tensor/255

# display original image
plot(as.raster(img_tensor[1,,,]))

# separate model by layers
layers_outputs <- lapply(model$layers[1:8], function(layer) layer$output)
activation_model <- keras_model(inputs = model$input, outputs = layers_outputs)

# get first layer
activation_layers <- activation_model %>% predict(img_tensor)
first_layer <- activation_layers[[1]]

# create function that recognize layers
plot_channel <- function(channel){
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(channel), axes=FALSE, asp=1,
        col=terrain.colors(12))
}

# view second activation layer
plot_channel(first_layer[1,,,2])

# visualizing every channel
image_size <-  58
images_per_row <- 16

for (i in 1:8) {
  
  layer_activation <- activation_layers[[i]]
  layer_name <- model$layers[[i]]$name
  
  n_features <- dim(layer_activation)[[4]]
  n_cols <- n_features %/% images_per_row
  png(paste0("cat_activations_", i, "_", layer_name, ".png"),
      width = image_size * images_per_row,
      height = image_size * n_cols)
  op <- par(mfrow = c(n_cols, images_per_row), mai = rep_len(0.02, 4))
  for (col in 0:(n_cols-1)) {
    for (row in 0:(images_per_row-1)) {
      channel_image <- layer_activation[1,,,(col*images_per_row) + row + 1]
      plot_channel(channel_image)
    }
  }
  par(op)
  dev.off()
}


