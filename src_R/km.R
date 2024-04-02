km <- function(wl, wr, f, maximum=F, w.which=F, sorted=F, k.which=F) {

    #if(any(wl > wr)) {
    #    stop("the lower bound should be no larger than upper bound")
    #} else
    #if(any(wl < 0) || any(wr < 0)) {
    #    stop("the firing strength should not be negative number")
    #}

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

            w <- (wl + wr) / 2
            y.new <- as.numeric((w %*% f) / sum(w))
            y <- y.new + 1
            cnt <- 1

            epsilon <- 10^-10
            while( abs(y.new - y) > epsilon && cnt <= n ) {
                y <- y.new
                cnt <- cnt + 1
                k <- length(which(f<=y))
                if(k == n) {
                    k <- n - 1
                } else if(k == 0) {
                    k <- 1
                }
                if(maximum) {
                    w <- c(wl[1:k], wr[(k+1):n])
                } else {
                    w <- c(wr[1:k], wl[(k+1):n])
                }
                y.new <- as.numeric((w %*% f) / sum(w))
            }

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



