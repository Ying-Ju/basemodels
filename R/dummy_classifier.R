#' dummy classifier to a categorical variable
#'
#' @param strategy a strategy from "constant", "most_frequent", "proportional", "uniform", or "stratified".
#' @param constant a constant value for the constant strategy.
#' @param random_state a random seed.
#'
#' @return a list
#' @export
#'
#' @examples
#' model <- dummy_classifier(strategy = "uniform", random_state = 2024)(iris[,-5], iris$Species)
dummy_classifier <- function(strategy = "proportional", constant = NULL, random_state = NULL) {
  if (!strategy %in% c("stratified", "most_frequent", "proportional", "uniform", "constant")) {
    stop("Invalid strategy. Choose from 'stratified', 'most_frequent', 'proportional', 'uniform', or 'constant'.")
  }

  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }

  function(X, y) {
    model <- list()
    model$strategy <- strategy
    model$constant <- constant
    model$y <- as.factor(y)
    model$classes <- levels(model$y)
    model$n_classes <- length(model$classes)
    model$class_prior <- table(y) / length(y)
    model$random_state <- random_state

    if (strategy == "most_frequent") {
      model$most_frequent <- levels(y)[which.max(table(y))]
    }

    return(model)
  }
}
