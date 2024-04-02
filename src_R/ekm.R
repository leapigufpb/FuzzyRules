ekm <- function(wl, wr, f, maximum=F, w.which=F, sorted=F, k.which=F) {

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
                mflag <- 1
                k <- round(n/1.7)
                w <- if(k < n) c(wl[1:k], wr[(k+1):n]) else wl[1:n]
            } else {
                mflag <- -1
                k <- round(n/2.4)
                w <- if(k < n) c(wr[1:k], wl[(k+1):n]) else wr[1:n]
            }

            a <- w %*% f
            b <- sum(w)
            y <- as.numeric(a / b)

            k.new <- length(which(f<=y))
            if(k.new == n) k.new <- n - 1
            k.hist <- k

#            cnt <- 1
            while(k.new != k && !(k.new %in% k.hist)) {
#                cnt <- cnt + 1
                s <- sign(k.new - k)

                idx <- (min(k, k.new) + 1) : max(k, k.new)

                a <- a - mflag * s * (f[idx] %*% (wr[idx] - wl[idx]))
                b <- b - mflag * s * sum(wr[idx] - wl[idx])
                y <- as.numeric(a / b)

                k <- k.new
                k.hist <- c(k.hist, k)
                k.new <- length(which(f<=y))
                if(k.new == n) k.new <- n - 1
            }
#            cat("cnt:", cnt, "\n")
#            cat("k.hist: [", k.hist, "]\n")
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



