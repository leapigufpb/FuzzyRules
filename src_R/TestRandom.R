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

m <- 5000
# N.max <- 2000
# N <- c(10, seq(200, N.max, by=200))
# N <- c(10, seq(20, N.max, by=10))
N <- seq(2, 20, by=2)^2


## n is the number of methods
n <- 9
t <- array(0, dim=c(length(N), 3, n))
y <- array(NA, dim=c(length(N), m, n))
k <- array(NA, dim=c(length(N), m, n))


for(i in 1:length(N)) {

    wr <- replicate(m, runif(N[i], 0, 1))
    wl <- wr * replicate(m, runif(N[i], 0, 1))
    # x <- replicate(m, sort(runif(N[i], 0, 1)))
    # sorted <- T
    x <- replicate(m, runif(N[i], 0, 1))
    sorted <- F

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- km(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, sorted=sorted, k.which=T)
        ret2 <- km(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, sorted=sorted, k.which=T)
        y[i, j, 1] <- ret1[1]
        k[i, j, 1] <- ret1[2]
    }
    t[i,,1] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- ekm(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, sorted=sorted, k.which=T)
        ret2 <- ekm(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, sorted=sorted, k.which=T)
        y[i, j, 2] <- ret1[1]
        k[i, j, 2] <- ret1[2]
    }
    t[i,,2] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- km.eiasc(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, sorted=sorted, k.which=T)
        ret2 <- km.eiasc(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, sorted=sorted, k.which=T)
        y[i, j, 3] <- ret1[1]
        k[i, j, 3] <- ret1[2]
    }
    t[i,,3] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- km.da(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, sorted=sorted, k.which=T)
        ret2 <- km.da(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, sorted=sorted, k.which=T)
        y[i, j, 4] <- ret1[1]
        k[i, j, 4] <- ret1[2]
    }
    t[i,,4] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- km.dastar(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, sorted=sorted, k.which=T)
        ret2 <- km.dastar(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, sorted=sorted, k.which=T)
        y[i, j, 5] <- ret1[1]
        k[i, j, 5] <- ret1[2]
    }
    t[i,,5] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- km.dand(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, sorted=sorted, k.which=T)
        ret2 <- km.dand(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, sorted=sorted, k.which=T)
        y[i, j, 6] <- ret1[1]
        k[i, j, 6] <- ret1[2]
    }
    t[i,,6] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- costrwsr(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, k.which=T)
        ret2 <- costrwsr(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, k.which=T)
        y[i, j, 7] <- ret1[1]
        # k[i, j, 7] <- ret1[2]
    }
    t[i,,7] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- ecostrwsr(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, k.which=T)
        ret2 <- ecostrwsr(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, k.which=T)
        y[i, j, 8] <- ret1[1]
        # k[i, j, 8] <- ret1[2]
    }
    t[i,,8] <- (proc.time() - time)[1:3]

    time <- proc.time()
    for(j in 1:m) {
        ret1 <- sc(wl[,j], wr[,j], x[,j], maximum=T, w.which=F, k.which=T)
        ret2 <- sc(wl[,j], wr[,j], x[,j], maximum=F, w.which=F, k.which=T)
        y[i, j, 9] <- ret1[1]
        # k[i, j, 9] <- ret1[2]
    }
    t[i,,9] <- (proc.time() - time)[1:3]

    cat("N:", N[i], "\n")
}

rm(wl, wr, x)

library(ggplot2)
library(reshape2)
library(gridExtra)
legend.position="right"


#pdf(file=paste0("DA-random-EIASC", N.max, ".pdf"), width=8, height=5)
#pdf(file=paste0("DA-rankings-random-R-", N.max, ".pdf"), width=5.15, height=4)


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
writeMat("../../results/RExample2.mat", times=times)

## Methods to plot
methods <- 1:6
#methods <- methods[-c(1,3)]
N.plot <- 1:length(N)
if(length(N) > 20) {
    N.plot <- c(1, seq(2,length(N),by=2))
} else {
    N.plot <- seq_len(length(N))
}

df <- data.frame(N=N, KM=t.km, EKM=t.ekm, EIASC=t.eiasc, DA=t.da, DASTAR=t.dastar, DAND=t.dand, COSTRWSR=t.costrwsr, ECOSTRWSR=t.ecostrwsr, SC=t.sc)
df <- df[N.plot, c(1, methods+1)]
df <- melt(df,id.vars="N")
df$N <- factor(df$N)

## Plot the data by ggplot
labels <- list(KM=expression("KM"), EKM=expression("EKM"), EIASC=expression("EIASC"), DA=expression("DA"), DASTAR=expression("DA*"), DAND=expression("DAND"))
labels <- labels[methods]

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
cbPalette <- cbPalette[methods]
shapes <- c(21,24,22,25,23,4)
shapes <- shapes[methods]


g1 <- ggplot(df, aes(x=N, value, group=variable, fill=variable, shape=variable, colour=variable)) +
        scale_x_discrete(breaks=c(10,seq(200, 2000, 200))) +
        geom_line(aes(linetype=variable), size=1) +
        geom_point(size=1) +
#        scale_colour_hue(name="Algorithm", l=30, labels=labels) +
        scale_colour_manual(name="Algorithm", labels=labels, values=cbPalette) +
        scale_shape_manual(name="Algorithm", values=shapes, labels=labels) +
        scale_fill_manual(name="Algorithm", values=cbPalette, labels=labels) +
        scale_linetype_discrete(name="Algorithm", labels=labels) +
        labs(x="N", y="time(sec)") +
        ggtitle("") +
        theme_bw() + 
        theme(plot.title = element_text(lineheight=.8, face="bold"), legend.position="none")

#print(g1)


df <- data.frame(N=N, KM=r.km, EKM=r.ekm, EIASC=r.eiasc, DA=r.da, DASTAR=r.dastar, DAND=r.dand, COSTRWSR=r.costrwsr, ECOSTRWSR=r.ecostrwsr, SC=r.sc)
df <- df[N.plot, c(1, methods+1)]
df <- melt(df,id.vars="N")
df$N <- factor(df$N)

## Plot the data by ggplot


g2 <- ggplot(df, aes(x=N, value, group=variable, fill=variable, shape=variable, colour=variable)) +
        scale_x_discrete(breaks=c(10,seq(200, 2000, 200))) +
        scale_y_continuous(limits=layer_scales(g1)$y$range$range) + 
        geom_line(aes(linetype=variable), size=1) +
        geom_point(size=1) +
#        scale_colour_hue(name="Algorithm", l=30, labels=labels) +
        scale_colour_manual(name="Algorithm", labels=labels, values=cbPalette) +
        scale_shape_manual(name="Algorithm", values=shapes, labels=labels) +
        scale_fill_manual(name="Algorithm", values=cbPalette, labels=labels) +
        scale_linetype_discrete(name="Algorithm", labels=labels) +
        labs(x="N", y="ratio to DA") +
        ggtitle("") +
        theme_bw() + 
        theme(plot.title = element_text(lineheight=.8, face="bold"), legend.position=legend.position)

#print(g2)


    main.title <- paste("")

    grid.arrange(g1, g2, ncol = 2, top = main.title)


# Close and save the plot file.
#dev.off()

