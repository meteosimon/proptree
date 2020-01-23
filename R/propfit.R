

propfit <- function(y, x = NULL, start = NULL, weights = NULL,
		    offset = NULL, ...
		    estfun = FALSE, object = FALSE) {

    # --- parameters ---
    y <- as.matrix(y)
    k <- ncol(y)
    if (is.null(weights)) {
        coef <- rowMeans(y)
    } else {
        coef <- apply(y, 2, weighted.mean, w = weights)
    }
    names(coef) <- if (is.null(colnames(y))) paste0("y_", 1L:k) else colnames(y)

    # --- logLik ---

    # --- estfun ---

    # --- return ---
}

