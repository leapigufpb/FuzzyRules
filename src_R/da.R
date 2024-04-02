#' @title km.da
#' @description
#' A Direct Approach for Determining the Switch Points in the Karnik-Mendel Algorithm.
#' @param wl A vector of lower membership grades.
#' @param wr A vector of upper membership grades.
#' @param f A vector of the primary values in the discrete universe of discourse X.
#' @param maximum T, to calculate the maximum centroid; F, to calulate the minimum centroid.
#' @param w.which T, to show which membership grade to be used to calculate maximum/minimum centroid for each primary value.
#' @param sorted T, to indicate that the primary values have already been put in ascending order.
#' @param k.which T, to show the index of the switch point selected by the algorithm.
#' @return w.which=T, a two-column matrix indicating which membership grades to be used; 
#' w.which=F and k.which=T, a vector of the centroid and the switch point; 
#' w.which=F and k.which=F, a single value of the centroid.
#' @examples
#' wr <- runif(100, 0, 1)
#' wl <- wr * runif(100, 0, 1)
#' f <- abs(runif(100, 0, 1))
#' f <- sort(f)
#' km.da(wl, wr, f)
#' @author Chao Chen
#' @references
#' A Direct Approach for Determining the Switch Points in the Karnik-Mendel Algorithm. \url{https://doi.org/10.1109/TFUZZ.2017.2699168}
#' @export

km.da <- function(wl, wr, f, maximum=F, w.which=F, sorted=F, k.which=F) {

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
                positive.w.cusum <- cumsum(wl[1:(n-1)])
                negative.w.cusum <- cumsum(wr[n:2])
            } else {
                positive.w.cusum <- cumsum(wr[1:(n-1)])
                negative.w.cusum <- cumsum(wl[n:2])
            }

            delta.f <- diff(f)
            positive <- delta.f * positive.w.cusum
            negative <- delta.f[(n-1):1] * negative.w.cusum

            positive.cusum <- c(0, cumsum(positive))
            negative.cusum <- c(cumsum(negative)[(n-1):1],0)

            derivatives <- positive.cusum - negative.cusum
            #k <- which.max(derivatives>=0)-1
            #k <- sum(derivatives<0)
            k <- length(which(derivatives<0))
            if(k == 0) k = 1

            if(maximum) {
                w <- c(wl[1:k], wr[(k+1):n])
            } else {
                w <- c(wr[1:k], wl[(k+1):n])
            }

            a <- w %*% f
            b <- sum(w)
            y <- as.numeric(a / b)

            #cat("k: ", k, "\n")

            if(w.which) {
                if(maximum) {
                    wr.which[idx.trim[1:k]] = FALSE
                } else {
                    if(k < n) wr.which[idx.trim[(k+1):n]] = FALSE
                }
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

