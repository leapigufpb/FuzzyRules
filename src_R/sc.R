## Function: sc
##  Description:
##          It is an alternative algorithm for EKM,
##          which is an enhancement of the costrwsr algorithm introduced in the below article:
##          https://doi.org/10.1109/TFUZZ.2016.2602392

sc <- function(wl, wr, f, maximum=F, w.which=F, k.which=F) {

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

            if(maximum) {
                init.all <- rep(1, n)
                init.one <- 0

            } else {
                init.all <- rep(0, n)
                init.one <- 1
            }

            delta <- rep(1, n)
            delta1 <- sum(wr)
            delta2 <- c(wr %*% f)
            deltaw <- wl - wr

            kk <- 0
            loop <- TRUE
            while(loop) {
                kk <- kk + 1
                loop <- FALSE
                deltanew <- init.all
                for(j in 1:n) {
                    Aj <- f[j]*delta1 - delta2
                    if(Aj < 0) {
                        deltanew[j] <- init.one
                    }

                    if(delta[j] != deltanew[j]) {
                        loop <- TRUE
                        if(delta[j]) {
                            delta1 <- delta1 + deltaw[j]
                            delta2 <- delta2 + deltaw[j]*f[j]
                        } else {
                            delta1 <- delta1 - deltaw[j]
                            delta2 <- delta2 - deltaw[j]*f[j]
                        }
                        delta[j] <- deltanew[j]
                    }
                }
                if(kk > n) loop <- FALSE
            }
            y <- delta2/delta1
            #cat("k: ", k, "\n")

            if(w.which) {
                wr.which[idx.trim] = delta
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