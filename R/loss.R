

#' Title
#'
#' @param y 
#' @param ychap 
#'
#' @return
#' @export
#'
#' @examples
loss <- function(y, ychap) {
    sqrt(mean((log(ychap + 1) - log(y + 1))^2))
}
