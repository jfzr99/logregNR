#' Predict method for nr_logistic objects
#'
#' @param object An object of class "nr_logistic"
#' @param newdata A data frame containing new predictor values
#' @param type Type of prediction: "response" (default, gives probabilities) or "class" (gives 0/1)
#' @param threshold Threshold for classification when type = "class" (default = 0.5)
#' @param ... Additional arguments (ignored)
#'
#' @return A vector of predicted probabilities or classes
#' @export
#' @export
predict.nr_logistic <- function(object, newdata, type = c("response", "class"), threshold = 0.5, ...) {
  type <- match.arg(type)

  # Ambil terms dari formula (tanpa response)
  Terms <- delete.response(terms(object$formula))

  # Buat model.frame baru dari newdata
  mf <- model.frame(Terms, newdata)

  # Buat model matrix dari newdata
  X_new <- model.matrix(Terms, mf)

  # Hitung eta dan probabilitas
  eta <- X_new %*% object$coefficients
  prob <- 1 / (1 + exp(-eta))

  if (type == "response") return(as.vector(prob))
  if (type == "class") return(as.integer(prob >= threshold))
}

