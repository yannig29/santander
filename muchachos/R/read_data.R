


#' read_train_data
#'
#' @param file input train file
#' @import readr, dplyr
#' @return a data.frame
#' @export
read_train_data <- function(file) {
  data <- read_csv(file, col_names = TRUE,
                   col_types = paste(c("c", rep("d", 4992)), collapse = ""))
  data <- select(data, - one_of(cols_const_to_remove))     # on enleve les colonnes constantes
  return(data)
}


#' read_test_data
#'
#' @param file input test file
#' @import readr, dplyr
#' @return a data.frame
#' @export
read_test_data <- function(file) {
  data <- read_csv(file, col_names = TRUE,
                   col_types = paste(c("c", rep("d", 4991)), collapse = ""))
  data <- select(data, - one_of(cols_const_to_remove))     # on enleve les colonnes constantes
  return(data)
}




