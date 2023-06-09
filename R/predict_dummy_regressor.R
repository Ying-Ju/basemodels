#' dummy regressor predictor
#'
#' @param object a list from the dummy_regressor function
#' @param X a data frame
#'
#' @return the predicted values
#' @export
#'
#' @examples
#' model <- dummy_regressor(strategy = "mean")(iris[,-1], iris[,1])
#' predict_dummy_regressor(model, iris[,-1])
predict_dummy_regressor <- function(object, X) {

  n_samples <- nrow(X)
  strategy <- object$strategy
  percentile <- object$quantile
  constant <- object$constant
  y <- object$y
  if (!(is.numeric(y)|is.integer(y))) stop("The response variable is not numerical.")
  if (strategy == "mean") {
    y.hat <- rep(mean(y, na.rm=T), n_samples)
  } else if (strategy == c("median")) {
    y.hat <- rep(stats::median(y, na.rm=T), n_samples)
  } else if (strategy == "quantile") {
    if (percentile < 0 | percentile > 1){
      stop("quantile must be a value in the range [0, 1].")
    }
    y.hat <- rep(unname(stats::quantile(y, percentile, na.rm=T)), n_samples)
  } else if (strategy == "constant") {
    y.hat <- rep(constant, n_samples)
  } else {
    stop("Invalid strategy specified.")
  }

  return(y.hat)
}
