make_nllh <- function(y) {
# --- helper: build neg-loglik function given data y ---
    k <- ncol(y)
    n <- nrow(y)
    logy <- log(y)
    function(logalpha) {
	alpha <- exp(logalpha)
	alpha_mat <- matrix(alpha - 1, ncol = k, nrow = n, byrow = TRUE)
        - n * lgamma(sum(alpha)) + n * sum(lgamma(alpha)) -
	    sum(alpha_mat * logy)
    }
}

make_grad <- function(y) {
# --- helper: build gradient function given data y ---
    k <- ncol(y)
    n <- nrow(y)
    logy <- log(y)
    sum_logy <- colSums(logy)
    function(logalpha) {
	alpha <- exp(logalpha)
	rval <- numeric(k)
        sum_alpha <- sum(alpha)
        gr_j <- function(j) {
	    - n * digamma(sum_alpha) + n * digamma(alpha[j]) - sum_logy[j]
	}
	vapply(seq_len(k), gr_j, numeric(1))
    }
}

make_score <- function(y) {
# --- helper: build score function given data y ---
    k <- ncol(y)
    n <- nrow(y)
    logy <- log(y)
    function(logalpha) {
	alpha <- exp(logalpha)    
        digamma_alpha <- digamma(sum(alpha)) - digamma(alpha)
        matrix(digamma_alpha, ncol = k, nrow = n, byrow = TRUE) + logy
    }
}

get_prop <- function(logalpha) {
# --- helper: compute proportions from logalphas ---
    exp(logalpha) / sum(exp(logalpha))
}

#' Fit Proportions.
#'
#' Maximum likelihood estimation of Dirichlet parameter.
#'
#' Perform MLE of parameters of a Dirichlet distribution.
#' Interface and return value are designed to fit
#' the modelparty infrastructure of the partykit package.
#' ML estimates are found numerically using `optim` with
#' BFGS algorithm.
#'
#' @param y A matrix or data.frame where each row corresponds to a k-dim observation.
#' @param x Not used.
#' @param start Not used.
#' @param weights Not used yet.
#' @param offset Not used.
#' @param ... Not used yet.
#' @param estfun logical. Should the matrix of score contributions (estimating
#'    functions) be returned.
#' @param object logical. Should the return from `optim()` be returned.
#'
#' @export
propfit <- function(y, x = NULL, start = NULL, weights = NULL,
		    offset = NULL, ...,
		    estfun = FALSE, object = FALSE) {

    # --- fit model ---
    y <- as.matrix(y)
    k <- ncol(y)
    nllh <- make_nllh(y)
    grad <- make_grad(y)
    init <- colMeans(y)
    opt <- stats::optim(init, nllh, grad, method = "BFGS", hessian = object)

    # --- check convergence ---
    conv <- opt$convergence == 0

    # --- keep coefficients ---
    coef <- opt$par
    names(coef) <- if (is.null(colnames(y))) paste0("y_", 1L:k) else colnames(y)

    # --- add proportions to optim object ---
    opt$prop <- get_prop(coef)

    # --- logLik ---
    objfun <- if (conv) opt$value else Inf

    # --- estfun ---
    if (estfun) {
        score <- make_score(y)
        estfun <- score(coef)
	colnames(estfun) <- names(coef)
    } else {
        estfun <- NULL
    }

    # --- return ---
    list(
        coefficients = coef,
        objfun = objfun,
	estfun = estfun,
	object = if (object) opt else NULL
    )
}


