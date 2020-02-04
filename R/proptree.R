#' Proportional Trees.
#'
#' Fit tree models based on the Dirichlet distribution.
#'
#' Fit tree models on proportional or compositional data
#' based on the Dirichlet distribution. This funciton
#' is a high-level interface to `partykit::mob()` in combination
#' with the fitter `propfit()`.
#'
#' @param formula A symbolic description of the model to be fit. This
#'  should either be of type `y1 + y2 + y3 ~ x1 + x2` with reponse
#'  vectors `y1`, `y2`, and `y3` or `y ~ x1 + x2`
#'  with a matrix response `y`. `x1` and `x2` are used as
#'  partitioning variables.
#' @param data A data frame containing the variables in the model.
#' @param na.action A function which indicates what should happen when the data
#'  contain missing values `NA`.
#' @param ... arguments passed to [partykit::mob_control()],
#'  [partykit::print.modelparty()] and [partykit::plot.modelparty()].
#'
#' @seealso [partykit::mob()], [propfit()]
#' 
#' @return An object of class `proptree` inheriting from `modelparty`.
#' 
#' @examples
#' ### --- toy data ---
#' set.seed(111)
#' n <- 500
#' x <- sample(c(-1, 1), n, replace = TRUE)
#' alpha <- cbind(
#'     rnorm(n, 1.4, sd = .1),
#'     rnorm(n, 1.4, sd = .1),
#'     jitter(2 + x)
#' )
#' y <- alpha / rowSums(alpha)
#' d <- as.data.frame(y)
#' names(d) <- paste0("y", 1:3)
#' d$x <- jitter(x)
#' tr <- proptree(y1 + y2 + y3 ~ x, data = d)
#' plot(tr)
#'
#' ### --- Sediment composition  ---
#' data("ArcticLake", package = "DirichletReg")
#' tr <- proptree(sand + silt + clay ~ depth, data = ArcticLake)
#' plot(tr)
#'
#' @export
proptree <- function(formula, data, na.action, ...) {
# --- high-level convenience interface to mob() + propfit() ---
    # --- keep call ---
    cl <- match.call(expand.dots = TRUE)
  
    # --- use dots for setting up mob_control ---
    control <- partykit::mob_control(...)
    control$ytype <- "matrix"
  
    # --- call mob ---
    m <- match.call(expand.dots = FALSE)
    m$fit <- propfit
    m$control <- control
    if("..." %in% names(m)) m[["..."]] <- NULL
    m[[1L]] <- as.call(quote(partykit::mob))
    rval <- eval(m, parent.frame())
    
    # --- extend class and keep original call ---
    rval$info$call <- cl
    class(rval) <- c("proptree", class(rval))
    return(rval)
}


# --- methods ---

#' @param x An object of class `proptree`.
#' @param title character, title for print method.
#' @param objfun character, labeling objective function.
#' @describeIn proptree print method
#' @export
print.proptree <- function(x, title = "Proportion tree",
			  objfun = "negative log-likelihood", ...) {
    partykit::print.modelparty(x, title = title, objfun = objfun, ...)
}

#' @param proportions logical, display proportions (default) or
#'   log-alphas of Dirichlet distribution.
#' @describeIn proptree plot method
#' @export
plot.proptree <- function(x, proportions = TRUE, ...) {
    if (proportions) {
	foo <- function(obj) {
	    intr <- sprintf("n = %d", obj$nobs)
            prop <- sprintf("%s: %0.1f %%", 
	        names(stats::coef(obj)),
		round(exp(stats::coef(obj)) / sum(exp(stats::coef(obj))), 3) * 100
	    )
            c(intr, "-----------", prop)
	}
        partykit::plot.modelparty(x, FUN = foo, ...)
    } else {
        NextMethod()
    }
}



