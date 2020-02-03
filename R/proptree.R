# --- high-level convenience interface to mob() + propfit() ---
proptree <- function(formula, data, na.action, ...) {
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

if(FALSE) {
    set.seed(111)
    n <- 500
    x <- sample(c(-1, 1), n, replace = TRUE)
    alpha <- cbind(rnorm(n, 1.4, sd = .1), rnorm(n, 1.4, sd = .1), jitter(2 + x))
    y <- alpha / rowSums(alpha)
    d <- as.data.frame(y)
    names(d) <- paste0("y", 1:3)
    d$x <- jitter(x)
    tr <- proptree(y1 + y2 + y3 ~ x, data = d)
}


# --- methods ---
print.mvntree <- function(x, title = "Proportion tree",
			  objfun = "negative log-likelihood", ...) {
    partykit::print.modelparty(x, title = title, objfun = objfun, ...)
}



