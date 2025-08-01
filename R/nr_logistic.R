#' Logistic Regression via Newton-Raphson Optimization
#'
#' Fits a binary logistic regression model using the Newton-Raphson optimization algorithm.
#'
#' @param formula A model formula (e.g. y ~ x1 + x2).
#' @param data A data.frame containing the variables in the model.
#' @param tol Convergence tolerance. Default is 1e-8.
#' @param max_iter Maximum number of iterations. Default is 25.
#' @param verbose Logical. If TRUE, prints iteration log-likelihood. Default is FALSE.
#'
#' @return An object of class "nr_logistic" with elements:
#' \describe{
#'   \item{coefficients}{Estimated model parameters}
#'   \item{fitted.values}{Estimated probabilities}
#'   \item{loglik}{Final log-likelihood value}
#'   \item{iterations}{Total number of iterations run}
#'   \item{converged}{TRUE if converged within max_iter}
#'   \item{hessian}{Hessian matrix at convergence}
#'   \item{vcov}{Variance-covariance matrix}
#'   \item{formula}{The model formula}
#'   \item{call}{Function call}
#' }
#' @examples
#' data(mtcars)
#' model <- nr_logistic(vs ~ wt + mpg, data = mtcars)
#' summary(model)
#' @export
nr_logistic <- function(formula, data, tol = 1e-8, max_iter = 25, verbose = FALSE) {
  mf <- model.frame(formula, data)
  y <- model.response(mf, "numeric")
  X <- model.matrix(formula, mf)

  if (!all(y %in% c(0, 1))) stop("Response variable must be binary (0/1)")

  beta <- rep(0, ncol(X))
  converged <- FALSE
  loglik <- numeric(max_iter)

  for (i in 1:max_iter) {
    eta <- X %*% beta
    p <- 1 / (1 + exp(-eta))

    W <- diag(as.vector(p * (1 - p)))
    gradient <- t(X) %*% (y - p)
    hessian <- -t(X) %*% W %*% X

    beta_new <- beta - solve(hessian) %*% gradient

    loglik[i] <- sum(y * eta - log(1 + exp(eta)))

    if (max(abs(beta_new - beta)) < tol) {
      converged <- TRUE
      break
    }

    beta <- beta_new
    if (verbose) cat(sprintf("Iter %d: log-lik = %.6f\n", i, loglik[i]))
  }

  fitted <- as.vector(1 / (1 + exp(-X %*% beta)))
  vcov <- solve(-hessian)
  colnames(vcov) <- rownames(vcov) <- colnames(X)

  result <- list(
    coefficients = as.vector(beta),
    fitted.values = fitted,
    loglik = loglik[i],
    iterations = i,
    converged = converged,
    hessian = hessian,
    vcov = vcov,
    formula = formula,
    call = match.call()
  )

  class(result) <- "nr_logistic"
  return(result)
}
