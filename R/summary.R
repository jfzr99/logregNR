#' @export
summary.nr_logistic <- function(object, ...) {
  se <- sqrt(diag(object$vcov))
  zval <- object$coefficients / se
  pval <- 2 * pnorm(-abs(zval))

  coef_table <- cbind(
    Estimate = object$coefficients,
    `Std. Error` = se,
    `z value` = zval,
    `Pr(>|z|)` = pval
  )

  result <- list(
    call = object$call,
    coefficients = coef_table,
    loglik = object$loglik,
    iterations = object$iterations,
    converged = object$converged,
    aic = -2 * object$loglik + 2 * length(object$coefficients)
  )

  class(result) <- "summary.nr_logistic"
  return(result)
}

#' @export
print.summary.nr_logistic <- function(x, ...) {
  cat("\nNewton-Raphson Logistic Regression Summary\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, P.value = TRUE, has.Pvalue = TRUE)
  cat(sprintf("\nLog-likelihood: %.4f\n", x$loglik))
  cat(sprintf("AIC: %.4f\n", x$aic))
  cat(sprintf("Number of iterations: %d\n", x$iterations))
  cat(sprintf("Converged: %s\n", x$converged))
}
