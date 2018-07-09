


read_train_data <- function(file) {
  data <- read_csv(file, col_names = TRUE,
                   col_types = paste(c("c", rep("d", 4992)), collapse = ""))
  cm <- mapply(function(x, y) x - y, data[, -c(1, 2)], colMeans(data[, -c(1, 2)]))
  data <- select(data, - cols_const_to_remove)     # on enleve les colonnes constantes
  return(data)
}


read_test_data <- function(file) {
  data <- read_csv(file, col_names = TRUE,
                   col_types = paste(c("c", rep("d", 4991)), collapse = ""))
  cm <- mapply(function(x, y) x - y, data[, -c(1, 2)], colMeans(data[, -c(1, 2)]))
  w <- which(colMeans(cm) == 0) # 261
  data <- select(data, - w)     # on enleve les colonnes constantes
  return(data)
}




