# library(testthat)
#
# robust_mean <- NumericStatistic(mean, na.rm = TRUE)
#
# robust_mean(iris$Petal.Length)
#
# format.NumericStatistic(robust_mean, data = iris$Petal.Length, fontsize_pt = 12, width_pt = 1.5*72, digits = 3)
#
# library(grid)
#
# dev.new(width = 5, height = 5)
#
# grid.draw(grid::circleGrob(.5, .2, .1))
#
# plot.describr <- function(descr, width = 5, height = 10, fontsize = 12) {
#   width    <- 6
#   height   <- 5in
#   fontsize <- 12
#   dev.new(width = width, height = height)
#   header <- viewport(
#     x = unit(0, "npc"),
#     y = unit(1, "npc"),
#     just = c("left", "top"),
#     width = unit(1, "npc"),
#     height = unit(fontsize*1.5, "pt"),
#     name = "header"
#   )
#   # pushViewport(header)
#   grid.rect(vp = header)
#   grid.text(vp = header, "Variable", x = unit(.1, "inches"), just = c("left", "center"))
#   grid.text(vp = header, "Statistic", x = unit(1.5 + .1, "inches"), just = c("left", "center"))
#   grid.text(vp = header, "Level", x = unit(3 +.1, "inches"), just = c("left", "center"))
#   grid.text(vp = header, "Allg", x = unit(4.5 + .1, "inches"), just = c("left", "center"))
#   p <- ggplot2::qplot(1:3, 1:3)
#   print(p, vp = header)
# }
#
# library(keras)
#
# mnist <- dataset_mnist()
# x_train <- mnist$train$x
# y_train <- mnist$train$y
# x_test <- mnist$test$x
# y_test <- mnist$test$y
#
# dim(x_train) <- c(nrow(x_train), 784)
# dim(x_test) <- c(nrow(x_test), 784)
# # rescale
# x_train <- x_train / 255
# x_test <- x_test / 255
#
# y_train <- to_categorical(y_train, 10)
# y_test <- to_categorical(y_test, 10)
#
# model <- keras_model_sequential()
# model %>%
#   layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
#   layer_dropout(rate = 0.4) %>%
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 10, activation = 'softmax')
#
# model %>% compile(
#   loss = 'categorical_crossentropy',
#   optimizer = optimizer_rmsprop(),
#   metrics = c('accuracy')
# )
#
# history <- model %>% fit(
#   x_train, y_train,
#   epochs = 30, batch_size = 128,
#   validation_split = 0.2
# )
#
# plot(history)
