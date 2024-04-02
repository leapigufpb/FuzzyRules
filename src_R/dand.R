## Function: km.dand
##  Description:
##          A direct approach which is not based on derivatives.
##          It is actually a brute force method to calculate all y to avoid iterations in EIASC.
##          It is similar to EIASC, but there is no iterations and stopping conditions.
##          Thus, this is not EIASC any more. It has more calculations but all vectorised.
##          It is much more efficient than EIASC in R, but less efficient than EIASC in Matlab.

km.dand <- function(wl, wr, f, maximum=F, w.which=F, sorted=F, k.which=F) {

    # if(any(wl > wr)) {
    #     stop("the lower bound should be no larger than upper bound")
    # } else if(any(wl < 0) || any(wr < 0)) {
    #     stop("the firing strength should not be negative number")
    # }

    y <- NULL
    if(w.which) {
        wr.which <- rep(TRUE, length(wr))
    }
    k <- NULL

    m1 <- max(wl)
    if(identical(wl, wr)) {
        if(m1 != 0) {
            s1 <- sum(wl)
            y <- wl %*% f / s1
        } else {
            y <- mean(f)
        }
    } else if(m1 == 0) {
        if(maximum) {
            y <- max(f[wr!=0])
        } else {
            y <- min(f[wr!=0])
        }
        if(w.which) {
            wr.which[-which(f==y)[1]] = FALSE
        }
    }

    if(is.null(y)) {

        idx.trim <- which(wr!=0)
        wl <- wl[idx.trim]
        wr <- wr[idx.trim]
        f <- f[idx.trim]

        if(length(unique(f)) == 1) {
            y <- unique(f)
        } else {
            n <- length(f)

            if(!sorted) {
                idx.order <- order(f)
                idx.trim <- idx.trim[idx.order]
                f <- f[idx.order]
                wl <- wl[idx.order]
                wr <- wr[idx.order]
            }

            if(maximum) {
                a <- c(wr %*% f)
                b <- sum(wr)
                wd <- wl - wr
            } else {
                a <- c(wl %*% f)
                b <- sum(wl)
                wd <- wr - wl
            }

            a <- a + cumsum(f*wd)
            b <- b + cumsum(wd)
            y <- a/b

            if(maximum) {
                k <- which.max(y)
                # In some cases, when k == n, y could be Inf due to the precision issue of floating-point number calculations.
                if(k == n) k = k - 1
                if(w.which) wr.which[idx.trim[1:k]] = FALSE
            } else {
                k <- which.min(y)
                if(w.which && k < n) wr.which[idx.trim[(k+1):n]] = FALSE
            }

            y <- y[k]

            #cat("k: ", k, "\n")
        }
    }

    if(w.which) {
        wl.which <- !wr.which
        cbind(wl.which, wr.which)
    } else {
        if(k.which) {
            c(y, k)
        } else {
            c(y)
        }
    }
}
