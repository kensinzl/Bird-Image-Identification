library(keras)
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)

# https://www.r-bloggers.com/2021/05/working-with-files-and-folders-in-r-ultimate-guide/
# https://forloopsandpiepkicks.wordpress.com/2021/03/16/how-to-build-your-own-image-recognition-app-with-r-part-1/
# https://www.r-bloggers.com/2021/03/how-to-build-your-own-image-recognition-app-with-r-part-1/

  
label_list = dir("/Users/zhaol/Downloads/Image_Bird/train")
# save it for the shiny usage rather than read from local
save(label_list, file="label_list.RData")

# there are 2254 train images 
sum(unlist(lapply(label_list, function(label) {
  length(list.files (paste0("/Users/zhaol/Downloads/Image_Bird/train/", label)))
})))

# there are 75 test images 
sum(unlist(lapply(label_list, function(label) {
  length(list.files (paste0("/Users/zhaol/Downloads/Image_Bird/test/", label)))
})))

# there are 75 valid images 
sum(unlist(lapply(label_list, function(label) {
  length(list.files (paste0("/Users/zhaol/Downloads/Image_Bird/valid/", label)))
})))

output_n = length(label_list)
save(label_list, file="label_list.R")

# the original image dimention is 224 * 224, if you decrease the dimention, then the image will loose the pixel of quality
width = 224
height = 224
target_size = c(width, height)
rgb = 3 #color channels

path_train = "/Users/zhaol/Downloads/Image_Bird/train"

# let’s just rescale the pixel values to values between 0 and 1 and tell the function to reserve 20% of the data for a validation dataset:
train_data_gen = image_data_generator(rescale = 1/255, validation_split = .2)

# We create two objects for the training 80% and validation data 20% from the above generator.
train_images = flow_images_from_directory(path_train,
                                           train_data_gen,
                                           subset = 'training',
                                           target_size = target_size,
                                           class_mode = "categorical",
                                           shuffle=F,
                                           classes = label_list,
                                           seed = 2021)

validation_images = flow_images_from_directory(path_train,
                                                train_data_gen, 
                                                subset = 'validation',
                                                target_size = target_size,
                                                class_mode = "categorical",
                                                classes = label_list,
                                                seed = 2021)

table(train_images$classes)
#sum(table(train_images$classes))
# 1809

table(validation_images$classes)
# sum(table(validation_images$classes))
# 445

# The first element of our train_images object has the pixel values of each image which is a 4D-tensor (number of image, width, height, rgb channel), 
# so with this call we are plotting image number 17.
# dim(train_images[[1]][[1]][1,,,])
# 224 224   3
plot(as.raster(train_images[[1]][[1]][1,,,]))

# # seems not be symmetric
# for (index in c(-57:56)) {
#   tryCatch({
#     data = train_images[[index]][[1]][1,,,]
#     for (subIndex in c(1:200)) {
#       data = train_images[[1]][[1]][subIndex,,,]
#     }
#   },
#   error = function(ex) {
#     print(paste0("The index: ", index, " and sub_index:", subIndex, " has error"))
#   })
# }
# # 56*32+17

# plot(as.raster(train_images[[-57]][[1]][2,,,]))
# plot(as.raster(train_images[[0]][[1]][2,,,]))


# https://easyai.tech/ai-definition/cnn/
# build model
mod_base = application_xception(weights = 'imagenet', include_top = FALSE, input_shape = c(width, height, 3))
freeze_weights(mod_base) 

model_function = function(learning_rate = 0.001, dropoutrate=0.2, n_dense=1024){
  
  k_clear_session()
  
  model = keras_model_sequential() %>%
    mod_base %>% 
    layer_global_average_pooling_2d() %>% 
    layer_dense(units = n_dense) %>%
    layer_activation("relu") %>%
    layer_dropout(dropoutrate) %>%
    layer_dense(units=output_n, activation="softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learning_rate),
    metrics = "accuracy"
  )
  
  return(model)
}

model = model_function()
model

# run this with 6 times and 32 batch to check have the overfit or not, here no overfit
hist = model %>% fit_generator(
  train_images,
  steps_per_epoch = train_images$n %/% 32, 
  epochs = 6, 
  validation_data = validation_images,
  validation_steps = validation_images$n %/% 32,
  verbose = 2
)



# Evaluating and testing the model
path_test = "/Users/zhaol/Downloads/Image_Bird/test"
test_data_gen = image_data_generator(rescale = 1/255)
test_images = flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = label_list,
                                          shuffle = F,
                                          seed = 2021)

model %>% evaluate_generator(test_images, steps = test_images$n)
#loss   accuracy 
#0.04064238 1.00000000

# manually test one image
test_image = image_load("/Users/zhaol/Downloads/Image_Bird/images to test/2.jpg", target_size = target_size)

x = image_to_array(test_image)
# 224 224 3
x = array_reshape(x, c(1, dim(x)))
# (number of image, width, height, rgb channel), 1 224 224   3
x = x/255
pred = model %>% predict(x)
pred = data.frame("Bird" = label_list, "Probability" = t(pred))
pred = pred[order(pred$Probability, decreasing=T),][1:5,]
# order return the index of the df
pred$Probability = paste(format(100*pred$Probability,2),"%")
pred


# predict each image probability for class 1 ~ class15, different way
predictions = model %>% predict_generator(generator = test_images, steps = test_images$n) %>% as.data.frame

# setting column name
names(predictions) = paste0("Class", 0:14)
# find the highest probability
predictions$predicted_class = paste0("Class", apply(predictions, 1, which.max) - 1)
# assign the real class of each test image
predictions$true_class = paste0("Class", test_images$classes)

predictions %>% group_by(true_class) %>% 
  summarise(percentage_true = 100*sum(predicted_class == true_class)/n()) %>% 
  left_join(data.frame(bird= names(test_images$class_indices), true_class=paste0("Class", 0:14)), by="true_class") %>%
  select(bird, percentage_true) %>% 
  mutate(bird = fct_reorder(bird, percentage_true)) %>%
  ggplot(aes(x=bird, y=percentage_true, fill=percentage_true, label=percentage_true)) +
  geom_col() + theme_minimal() + coord_flip() +
  geom_text(nudge_y = 3) + 
  ggtitle("Percentage correct classifications by bird species")





# Tune the model
tune_grid <- data.frame("learning_rate" = c(0.001,0.0001), "dropoutrate" = c(0.3,0.2), "n_dense" = c(1024,256))
tuning_results <- NULL
set.seed(2021)

for (i in 1:length(tune_grid$learning_rate)){
  for (j in 1:length(tune_grid$dropoutrate)){
    for (k in 1:length(tune_grid$n_dense)){
      
      model = model_function(learning_rate = tune_grid$learning_rate[i], 
        dropoutrate = tune_grid$dropoutrate[j],
        n_dense = tune_grid$n_dense[k])
      
      hist = model %>% fit_generator(
        train_images,
        steps_per_epoch = train_images$n %/% 32, 
        epochs = 6, 
        validation_data = validation_images,
        validation_steps = validation_images$n %/% 32,
        verbose = 2
      )
      
      #Save model configurations
      tuning_results <- rbind(
        tuning_results,
        c("learning_rate" = tune_grid$learning_rate[i],
          "dropoutrate" = tune_grid$dropoutrate[j],
          "n_dense" = tune_grid$n_dense[k],
          "val_accuracy" = hist$metrics$val_accuracy))
      
    }
  }
}

tuning_results

write_rds(tuning_results, "tuning_results.rds")

tuning_results = read_rds("tuning_results.rds")

# find the best
best_results = tuning_results[which( 
  tuning_results[,ncol(tuning_results)] == max(tuning_results[,ncol(tuning_results)])
  ),]


modelbest = model_function(learning_rate = best_results[,"learning_rate"][2], dropoutrate = best_results[,"dropoutrate"][2],  n_dense = best_results[,"n_dense"][2])

hist <- modelbest %>% fit_generator(
  train_images,
  steps_per_epoch = train_images$n %/% 32, 
  epochs = 6, 
  validation_data = validation_images,
  validation_steps = validation_images$n %/% 32,
  verbose = 2
)

#creates a folder with the Tensorflow model in your working directory.
model %>% save_model_tf("bird_mod")


