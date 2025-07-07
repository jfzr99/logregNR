#' Plot ROC Curve
#'
#' @param actual Vector of actual binary outcomes
#' @param prob Vector of predicted probabilities
#'
#' @return A ggplot object showing ROC curve
#' @export
plot_roc <- function(actual, prob) {
  library(ggplot2)
  library(pROC)

  roc_obj <- roc(actual, prob)

  ggroc(roc_obj, legacy.axes = TRUE) +
    geom_abline(linetype = "dashed", color = "grey") +
    ggtitle(sprintf("ROC Curve (AUC = %.3f)", auc(roc_obj))) +
    theme_minimal()
}
