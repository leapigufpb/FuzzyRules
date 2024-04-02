## CHANGE THIS TO YOUR WORKING DIRECTOR PLEASE.
setwd("./")

source("km.R")
source("ekm.R")
source("eiasc.R")
source("da.R")
source("dastar.R")
source("dand.R")
source("costrwsr.R")
source("ecostrwsr.R")
source("sc.R")

set.seed(777)

# K is the number of FLCs, N is the number of discretisation in the input domain.
K <- 50
N <- 10
x1 <- seq(-1, 1, length.out=N)
x2 <- seq(-1, 1, length.out=N)
idx <- expand.grid(1:N,1:N)
e1 <- x1[idx[,1]]
e2 <- x2[idx[,2]]
# M is the number of membership functions for each input.
# M.max <- 50
# M <- c(2:9, seq(10, M.max, by=10))
M <- seq(2, 20, by=2)

## n is the number of methods
n <- 9
t <- array(0, dim=c(length(M), 3, n))
y <- array(NA, dim=c(K, N^2, length(M), n))
k <- array(NA, dim=c(K, N^2, length(M), n))
sorted <- F


for(i in 1:length(M)) {
    m <- M[i]
    rulebase <- array(0, dim=c(7, m^2, K))

    for(j in 1:K) {
        IN1_MF <- rbind(runif(m, -1, 1), apply(replicate(2, runif(m, 0.1, 0.5)), 1, sort))
        IN2_MF <- rbind(runif(m, -1, 1), apply(replicate(2, runif(m, 0.1, 0.5)), 1, sort))
        cons <- runif(m^2, -2, 2)
        if(sorted) cons <- sort(cons)
        rule <- t(expand.grid(1:m, 1:m))
        rulebase[,,j] <- rbind(IN1_MF[,rule[1,]], IN2_MF[,rule[2,]], cons)
    }

    wl <- wr <- array(,dim=c(N^2, m^2, K))
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            wr[nn,,kk] <- exp(-(e1[nn]-rulebase[1,,kk])^2/(2*rulebase[3,,kk]^2)-(e2[nn]-rulebase[4,,kk])^2/(2*rulebase[6,,kk]^2))
            wl[nn,,kk] <- exp(-(e1[nn]-rulebase[1,,kk])^2/(2*rulebase[2,,kk]^2)-(e2[nn]-rulebase[4,,kk])^2/(2*rulebase[5,,kk]^2))
        }
    }

    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- km(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, sorted=sorted, k.which=T)
            ret2 <- km(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, sorted=sorted, k.which=T)
            # y[kk, nn, i, 1] <- ret1[1]
            # k[kk, nn, i, 1] <- ret1[2]
        }
    }
    t[i,,1] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- ekm(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, sorted=sorted, k.which=T)
            ret2 <- ekm(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, sorted=sorted, k.which=T)
            # y[kk, nn, i, 2] <- ret1[1]
            # k[kk, nn, i, 2] <- ret1[2]
        }
    }
    t[i,,2] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- km.eiasc(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, sorted=sorted, k.which=T)
            ret2 <- km.eiasc(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, sorted=sorted, k.which=T)
            # y[kk, nn, i, 3] <- ret1[1]
            # k[kk, nn, i, 3] <- ret1[2]
        }
    }
    t[i,,3] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- km.da(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, sorted=sorted, k.which=T)
            ret2 <- km.da(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, sorted=sorted, k.which=T)
            # y[kk, nn, i, 4] <- ret1[1]
            # k[kk, nn, i, 4] <- ret1[2]
        }
    }
    t[i,,4] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- km.dastar(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, sorted=sorted, k.which=T)
            ret2 <- km.dastar(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, sorted=sorted, k.which=T)
            # y[kk, nn, i, 5] <- ret1[1]
            # k[kk, nn, i, 5] <- ret1[2]
        }
    }
    t[i,,5] <- (proc.time() - time)[1:3]


   time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- km.dand(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, sorted=sorted, k.which=T)
            ret2 <- km.dand(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, sorted=sorted, k.which=T)
            # y[kk, nn, i, 6] <- ret1[1]
            # k[kk, nn, i, 6] <- ret1[2]
        }
    }
    t[i,,6] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- costrwsr(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, k.which=T)
            ret2 <- costrwsr(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, k.which=T)
            # y[kk, nn, i, 7] <- ret1[1]
            # k[kk, nn, i, 7] <- ret1[2]
        }
    }
    t[i,,7] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- ecostrwsr(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, k.which=T)
            ret2 <- ecostrwsr(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, k.which=T)
            # y[kk, nn, i, 8] <- ret1[1]
            # k[kk, nn, i, 8] <- ret1[2]
        }
    }
    t[i,,8] <- (proc.time() - time)[1:3]


    time <- proc.time()
    for(kk in 1:K) {
        for(nn in 1:N^2) {
            ret1 <- sc(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=T, w.which=F, k.which=T)
            ret2 <- sc(wl[nn,,kk], wr[nn,,kk], rulebase[7,,kk], maximum=F, w.which=F, k.which=T)
            # y[kk, nn, i, 9] <- ret1[1]
            # k[kk, nn, i, 9] <- ret1[2]
        }
    }
    t[i,,9] <- (proc.time() - time)[1:3]

    cat("M:", M[i], "\n")
}

rm(wl, wr, rulebase)

library(ggplot2)
library(reshape2)
library(gridExtra)
legend.position="right"


#pdf(file=paste0("DA-bell-shaped-EIASC", N.max, ".pdf"), width=8, height=5)
#pdf(file=paste0("DA-rankings-bellshaped-R-", N.max, ".pdf"), width=5.15, height=4)


t.which <- 1

t.km    <- t[,t.which,1]
t.ekm   <- t[,t.which,2]
t.eiasc  <- t[,t.which,3]
t.da     <- t[,t.which,4]
t.dastar   <- t[,t.which,5]
t.dand   <- t[,t.which,6]
t.costrwsr   <- t[,t.which,7]
t.ecostrwsr   <- t[,t.which,8]
t.sc   <- t[,t.which,9]

t.ref <- t.da
r.km    <- t[,t.which,1]/t.ref
r.ekm   <- t[,t.which,2]/t.ref
r.eiasc  <- t[,t.which,3]/t.ref
r.da     <- t[,t.which,4]/t.ref
r.dastar   <- t[,t.which,5]/t.ref
r.dand   <- t[,t.which,6]/t.ref
r.costrwsr   <- t[,t.which,7]/t.ref
r.ecostrwsr   <- t[,t.which,8]/t.ref
r.sc   <- t[,t.which,9]/t.ref

times <- rbind(t.km, t.ekm, t.eiasc, t.da, t.dastar, t.dand, t.costrwsr, t.ecostrwsr, t.sc)
library(R.matlab)
writeMat("../../results/RExample3.mat", times=times)

## Methods to plot
methods <- 1:6
#methods <- methods[-c(1,3)]

df <- data.frame(M=M, KM=t.km, EKM=t.ekm, EIASC=t.eiasc, DA=t.da, DASTAR=t.dastar, DAND=t.dand, COSTRWSR=t.costrwsr, ECOSTRWSR=t.ecostrwsr, SC=t.sc)
df <- df[, c(1, methods+1)]
df <- melt(df,id.vars="M")
df$M <- factor(df$M)

## Plot the data by ggplot
labels <- list(KM=expression("KM"), EKM=expression("EKM"), EIASC=expression("EIASC"), DA=expression("DA"), DASTAR=expression("DA*"), DAND=expression("DAND"))
labels <- labels[methods]

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
cbPalette <- cbPalette[methods]
shapes <- c(21,24,22,25,23,4)
shapes <- shapes[methods]


g1 <- ggplot(df, aes(x=M, value, group=variable, fill=variable, shape=variable, colour=variable)) +
        scale_x_discrete(breaks=c(10,seq(200, 2000, 200))) +
        geom_line(aes(linetype=variable), size=1) +
        geom_point(size=1) +
#        scale_colour_hue(name="Algorithm", l=30, labels=labels) +
        scale_colour_manual(name="Algorithm", labels=labels, values=cbPalette) +
        scale_shape_manual(name="Algorithm", values=shapes, labels=labels) +
        scale_fill_manual(name="Algorithm", values=cbPalette, labels=labels) +
        scale_linetype_discrete(name="Algorithm", labels=labels) +
        labs(x="M", y="time(sec)") +
        ggtitle("") +
        theme_bw() + 
        theme(plot.title = element_text(lineheight=.8, face="bold"), legend.position="none")

#print(g1)


df <- data.frame(M=M, KM=r.km, EKM=r.ekm, EIASC=r.eiasc, DA=r.da, DASTAR=r.dastar, DAND=r.dand, COSTRWSR=r.costrwsr, ECOSTRWSR=r.ecostrwsr, SC=r.sc)
df <- df[, c(1, methods+1)]
df <- melt(df,id.vars="M")
df$M <- factor(df$M)

## Plot the data by ggplot


g2 <- ggplot(df, aes(x=M, value, group=variable, fill=variable, shape=variable, colour=variable)) +
        scale_x_discrete(breaks=c(10,seq(200, 2000, 200))) +
        scale_y_continuous(limits=layer_scales(g1)$y$range$range) + 
        geom_line(aes(linetype=variable), size=1) +
        geom_point(size=1) +
#        scale_colour_hue(name="Algorithm", l=30, labels=labels) +
        scale_colour_manual(name="Algorithm", labels=labels, values=cbPalette) +
        scale_shape_manual(name="Algorithm", values=shapes, labels=labels) +
        scale_fill_manual(name="Algorithm", values=cbPalette, labels=labels) +
        scale_linetype_discrete(name="Algorithm", labels=labels) +
        labs(x="M", y="ratio to DA") +
        ggtitle("") +
        theme_bw() + 
        theme(plot.title = element_text(lineheight=.8, face="bold"), legend.position=legend.position)

#print(g2)


    main.title <- paste("")

    grid.arrange(g1, g2, ncol = 2, top = main.title)


# Close and save the plot file.
#dev.off()

