## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
                      collapse = TRUE,
                      comment = "#>"
)

## ----setup--------------------------------------------------------------------
require(zoo)
library(synthesis)

## ----mod1, fig.cap=c('Example of Random walk model','Example of Autoregressive models'), fig.height=c(5,7), fig.width=c(5,9), out.width= c('80%','100%')----
set.seed(2021)
sample=500

###Synthetic example - RW model
data.rw <- data.gen.rw(nobs=sample,drift=0.1,sd=1)

plot.ts(data.rw$xd, ylim=c(-35,55), main="Random walk", xlab=NA, ylab=NA, cex.axis=1.5)
lines(data.rw$x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.1, lty=2)

###Synthetic example - AR models
ar1 <- data.gen.ar1(nobs=sample)$x
ar4 <- data.gen.ar4(nobs=sample)$x
ar9 <- data.gen.ar9(nobs=sample)$x

plot.zoo(cbind(ar1,ar4,ar9), col=c("black","red","blue"), ylab=c("AR1","AR4","AR9"),
              main=NA, xlab=NA, cex.axis=1.5)

###Synthetic example - TAR models
# Two TAR models in Sharma (2000)
tar1 <- data.gen.tar1(nobs=1000)$x #TAR in Equation (8)
tar2 <- data.gen.tar2(nobs=1000)$x #TAR in Equation (9)

# Generalized TAR, an example in Jiang et al. (2020)
tar <- data.gen.tar(nobs=1000,ndim=9,phi1=c(0.6,-0.1),
                     phi2=c(-1.1,0),theta=0,d=2,p=2,noise=0.1)$x 

plot.zoo(cbind(tar1,tar2,tar), col=c("black","red","blue"), ylab=c("TAR1","TAR2","TAR"),
              main=NA, xlab=NA, cex.axis=1.5)

## ----mod2, fig.cap=c('Example of Hysteresis loop', 'Example of Friedman with independent and correlated uniform variates'), fig.height=c(4,7), fig.width=c(4,9), out.width= c('80%','100%')----
sample=1000
###synthetic example - Hysteresis loop
#Frequency, sampled from a given range
fd <- c(3,5,10,15,25,30,55,70,95)
data.HL <- data.gen.HL(nobs=sample,m=3,n=5,fp=25,fd=fd, sd.x=0, sd.y=0)

plot(data.HL$x,data.HL$dp[,data.HL$true.cpy], xlab="x", ylab = "y", type = "p", cex.axis=1.5,cex.lab=1.5)
###synthetic example - Friedman
#Friedman with independent uniform variates
data.fm1 <- data.gen.fm1(nobs=sample, ndim = 9, noise = 0)
#Friedman with correlated uniform variates
data.fm2 <- data.gen.fm2(nobs=sample, ndim = 9, r = 0.6, noise = 0) 

plot.zoo(cbind(data.fm1$x,data.fm2$x), col=c("red","blue"), main=NA, xlab=NA,
              ylab=c("Friedman with \n independent uniform variates",
                     "Friedman with \n correlated uniform variates"))

## ----mod3, fig.cap=c('Example of Hénon map','Example of Logistic map','Example of Duffing map'), fig.height=4, fig.width=9, out.width= '100%'----
###Synthetic example - Iterated mappings
set.seed(2021)
par(mfrow=c(1,3), ps=12, cex.lab=1.5, pty="s")
sample <- 1000
Henon.map <- data.gen.Henon(nobs = sample, do.plot=TRUE)
Logistic.map <- data.gen.Logistic(nobs = sample, do.plot=TRUE)
Duffing.map <- data.gen.Duffing(nobs = sample, do.plot=TRUE)

## ----mod4, fig.cap='Example of Rossler system: Phase portraits in a 2D projection of its state space', fig.height=5, fig.width=9, out.width= '100%'----
###Synthetic example - Rossler
ts.r <- data.gen.Rossler(a = 0.2, b = 0.2, w = 5.7, start=c(-2, -10, 0.2), 
                         time = seq(0, 50, length.out = 5000), s=0)

par(mfrow=c(1,2), ps=12, cex.lab=1.5)
plot(ts.r$x,ts.r$y, xlab="x",ylab = "y", type = "l")
plot(ts.r$x,ts.r$z, xlab="x",ylab = "z", type = "l")

## ----mod5, fig.cap='Example of Lorenz system: Phase portraits in a 2D projection of its state space', fig.height=5, fig.width=9, out.width= '100%'----
###Synthetic example - Lorenz
ts.l <- data.gen.Lorenz(sigma = 10, beta = 8/3, rho = 28, start = c(-13, -14, 47), 
                        time = seq(0, 50, length.out = 5000), s=0)

par(mfrow=c(1,2), ps=12, cex.lab=1.5)
plot(ts.l$x,ts.l$y, xlab="x",ylab = "y", type = "l")
plot(ts.l$x,ts.l$z, xlab="x",ylab = "z", type = "l")

## ----class, fig.cap='Example of Classification system: Blobs, Circles and Spirals', fig.height=4, fig.width=9, out.width= '100%'----
set.seed(2021)
sample=500

par(mfrow=c(1,3), ps=12, cex.lab=1.5, pty="s")
Blobs=data.gen.blobs(nobs=sample, features=2, centers=5, sd=1, bbox=c(-10,10), do.plot=TRUE)
Circles=data.gen.circles(n = sample, r_vec=c(1,1.5), start=runif(1,-1,1), s=0.1, do.plot=TRUE)
Spirals=data.gen.spirals(n = sample, cycles=3, s=0.01, do.plot=TRUE)

## ----ss, fig.cap='Example of Linear Gaussian state-space model', fig.height=7, fig.width=9, out.width= '100%'----
###Linear Gaussian state-space model

data.LGSS <- data.gen.LGSS(theta=c(0.75,1.00,0.10), nobs=500, do.plot = TRUE)


