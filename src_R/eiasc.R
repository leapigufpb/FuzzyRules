## Function: km.eiasc
##  Description:
##          it is an alternative algorithm for EKM

km.eiasc <- function(wl, wr, f, maximum=F, w.which=F, sorted=F, k.which=F) {

    if(any(wl > wr)) {
        stop("the lower bound should be no larger than upper bound")
    } else if(any(wl < 0) || any(wr < 0)) {
        stop("the firing strength should not be negative number")
    }

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
                mflag <- -1
                k <- n
            } else {
                mflag <- 1
                k <- 1
            }

            a <- wl %*% f
            b <- sum(wl)
            y <- a / b

            while((maximum&&y<f[k]) || (!maximum&&y>f[k]) ) {

                w <- wr[k] - wl[k]
                a <- a + f[k] * w
                b <- b + w
                y <- a / b

                k <- k + mflag
            }

            if(maximum) {
                if(w.which) wr.which[idx.trim[1:k]] = FALSE
            } else {
                k <- k - 1
                if(w.which && k < n) wr.which[idx.trim[(k+1):n]] = FALSE
            }
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