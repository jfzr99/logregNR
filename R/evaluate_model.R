#' Evaluate Classification Performance
#'
#' @param actual Vector of actual labels (0 or 1)
#' @param predicted Vector of predicted labels (0 or 1)
#'
#' @return A list containing accuracy, precision, recall, specificity, and confusion matrix
#' @export
evaluate_model <- function(actual, predicted) {
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)

  acc <- (TP + TN) / length(actual)
  prec <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  spec <- TN / (TN + FP)

  list(
    accuracy = acc,
    precision = prec,
    recall = recall,
    specificity = spec,
    confusion_matrix = matrix(c(TN, FP, FN, TP), nrow = 2,
                              dimnames = list("Actual" = c("0", "1"), "Predicted" = c("0", "1")))
  )
}
