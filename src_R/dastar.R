#' @title km.dastar
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
#' A Direct Approach for Determining the Switch Points in the Karnik-Mendel Algorithm.
#' @export

km.dastar <- function(wl, wr, f, maximum=F, w.which=F, sorted=F, k.which=F) {

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
                positive.w.cusum <- cumsum(wl)
                wr.cumsum <- cumsum(wr)
                negative.w.cusum <- wr.cumsum[n] - wr.cumsum
            } else {
                positive.w.cusum <- cumsum(wr)
                wl.cumsum <- cumsum(wl)
                negative.w.cusum <- wl.cumsum[n] - wl.cumsum
            }

            delta.f <- c(diff(f),0)
            positive <- delta.f * positive.w.cusum
            negative <- delta.f * negative.w.cusum

            sn <- sum(negative)
            derivatives <- cumsum(positive+negative)

            k <- length(which(derivatives<sn)) + 1
            if(k >= n) k = n - 1

            if(maximum) {
                w <- c(wl[1:k], wr[(k+1):n])
            } else {
                w <- c(wr[1:k], wl[(k+1):n])
            }

            if(k != 1) {
                y <- f[k] - (derivatives[k-1] - sn) / sum(w)
            } else {
                y <- f[k] + sn / sum(w)
            }

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


