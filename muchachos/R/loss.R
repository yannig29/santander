

#' loss
#'
#' @param y Real value
#' @param ychap Forecasted value
#'
#' @return logloss
#' @export
loss <- function(y, ychap) {
    sqrt(mean((log(ychap + 1) - log(y + 1))^2))
}
