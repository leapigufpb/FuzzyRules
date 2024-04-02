## Function: ecostrwsr
##  Description:
##          It is an alternative algorithm for EKM,
##          which is an enhancement of the costrwsr algorithm introduced in the below article:
##          https://doi.org/10.1109/TFUZZ.2016.2602392

ecostrwsr <- function(wl, wr, f, maximum=F, w.which=F, k.which=F) {

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

            lambda <- rep(1, n)

            # if(maximum) {
            #     `%cmp%` <- `>`
            # } else {
            #     `%cmp%` <- `<`
            # }

            delta1 <- sum(wr)
            delta2 <- c(wr %*% f)
            delta3 <- 0
            delta4 <- 0

            kk <- 0
            loop <- TRUE
            while(loop) {
                kk <- kk + 1
                loop <- FALSE
                lambdaNew <- rep(0, n)
                for(j in 1:n) {
                    Aj <- f[j] - delta2/delta1 + delta3/delta1 - delta4/delta1*f[j]
                    if(maximum) {
                        if(Aj > 0) lambdaNew[j] <- 1
                    } else {
                        if(Aj < 0) lambdaNew[j] <- 1
                    }
                    # if(Aj %cmp% 0) {
                    #     lambdaNew[j] <- 1
                    # }

                    if(lambda[j] != lambdaNew[j]) {
                        loop <- TRUE
                        delta3 <- delta3 + (wr[j]-wl[j])*f[j]*(lambda[j]-lambdaNew[j])
                        delta4 <- delta4 + (wr[j]-wl[j])*(lambda[j]-lambdaNew[j])
                        lambda[j] <- lambdaNew[j]
                    }
                }
                if(kk > n) loop <- FALSE
            }
            y <- (delta2-delta3)/(delta1-delta4)
            # rm(`%cmp%`)
            #cat("k: ", k, "\n")

            if(w.which) {
                wr.which[idx.trim] = lambda
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