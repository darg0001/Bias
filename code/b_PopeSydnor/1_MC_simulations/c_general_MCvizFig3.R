## 
## Simple MC Simulation with decision tree
##

rm(list=ls())


## Packages
library(boot)
library(rpart)
library(rpart.plot)


## Parameters
beta0 <- 0
beta1 <- 0.5 #SAP
beta2 <- 3 #CP
beta3 <- 10 #SUP
beta4 <- 0 # SAP * CP interaction
sigma <- 0.2

delta0 <- 0
delta1 <- 5 #Cor(CP, SUP)

displace <- 3

## Sample size
n <- 1000


## Data
generate.data <- function(beta0, beta1, beta2, beta3, beta4, sigma, delta0, delta1, displace, shift.sd = 1){
    
    ## Set up data frame with CP
    dta <- data.frame(x.cp = rnorm(n))

    ## Generate SUP
    dta$x.sup <- with(dta, as.numeric(inv.logit(delta0 + delta1*x.cp + rnorm(n)) > 0.5))
    
    ## Generate SAP with displacement 
    dta$x.sap <- rnorm(n, mean = (displace * dta$x.sup), sd = (shift.sd * dta$x.sup + (1-dta$x.sup)))

    ## Calculate outcomes
    dta$y <- with(dta,
                  beta0 + beta1 * x.sap + beta2 * x.cp + beta3 * x.sup +
                      beta4 * (x.sup * ((x.sap+2)^2)) + rnorm(n, sd = sigma))

    return(dta)
}


## Function to calculate RMSE with various LM approaches
lm.rmse <- function(dta, train.index){
    
    ## Fit models
    m.restricted <- lm(y ~ x.sap, data=dta, subset = train.index)
    m.full <- lm(y ~ x.sup + x.cp + x.sap, data=dta, subset = train.index)

    ## Create new data frame for mean imputation of SUP
    new.dta <- dta
    ## Impute mean
    new.dta$x.sup <- mean(dta$x.sup[train.index])

    ## Predict
    yhat.full <- predict(m.full, newdata=dta[!train.index,])
    yhat.proposed <- predict(m.full, newdata = new.dta[!train.index,])
    yhat.restricted <- predict(m.restricted, newdata=dta[!train.index,])
    
    ## RMSE
    rmse.full <- sqrt(mean((yhat.full - dta$y[!train.index])^2))
    rmse.proposed <- sqrt(mean((yhat.proposed - dta$y[!train.index])^2))
    rmse.restricted <- sqrt(mean((yhat.restricted - dta$y[!train.index])^2))
    
    return(list(rmse.full, rmse.proposed, rmse.restricted, yhat.full, yhat.proposed, yhat.restricted))
}


## Function to calculate RMSE with DT approaches
rpart.rmse <- function(dta, train.index){

    ## Fit decision trees
    dt.restricted <- rpart(y ~ x.sap, data=dta, subset = train.index)
    dt.full <- rpart(y ~ x.sup + x.cp + x.sap, data=dta, subset = train.index)

    ## Predict
    yhat.full <- predict(dt.full, newdata=dta[!train.index,])
    yhat.restricted <- predict(dt.restricted, newdata=dta[!train.index,])

    ## Create new data frame for mean imputation of SUP
    new.dta1 <- new.dta0 <- dta
    p1 <- mean(dta$x.sup[train.index])
    new.dta1$x.sup <- rep(1, nrow(dta))
    new.dta0$x.sup <- rep(0, nrow(dta))
    yhat.proposed1 <- predict(dt.full, newdata=new.dta1[!train.index,])
    yhat.proposed0 <- predict(dt.full, newdata=new.dta0[!train.index,])
    yhat.proposed <- p1*yhat.proposed1 + (1-p1)*yhat.proposed0

    ## RMSE
    rmse.full <- sqrt(mean((yhat.full - dta$y[!train.index])^2))
    rmse.proposed <- sqrt(mean((yhat.proposed - dta$y[!train.index])^2))
    rmse.restricted <- sqrt(mean((yhat.restricted - dta$y[!train.index])^2))

    return(list(rmse.full, rmse.proposed, rmse.restricted,
                yhat.full, yhat.proposed, yhat.restricted))
}

step.plot <- function(xx, yy, sup=NULL, type="s", ...){

    ## Sort
    yy <- yy[order(xx)]
    sup <- sup[order(xx)]
    xx <- xx[order(xx)]
    
    if(is.null(sup)){
        
        lines(xx, yy, type=type,...)
        
    } else {

        yy1 <- yy[sup==1]
        yy0 <- yy[sup==0]
        xx1 <- xx[sup==1]
        xx0 <- xx[sup==0]
        
        lines(xx0, yy0, type=type,...)
        lines(xx1, yy1, type=type,...)
    }

}

dta <- generate.data(beta0 = 0, beta1 = 1, beta2 = 0, beta3 = 5, beta4 = 0, sigma= 0.2,
                     delta0 = 0, delta1 = 0, displace = 2, shift.sd = 0.3)

## Fit models
my.train <- (1:nrow(dta))%in%sample(1:nrow(dta), round(0.8*nrow(dta)))
lm1 <- lm.rmse(dta, my.train)
rp1 <- rpart.rmse(dta, my.train)

## Renaming so variable names appear nicely on plot
dta.tmp <- dta
names(dta.tmp) <-  c("CP",  "SUP", "SAP", "Y") 
dt.full <- rpart(Y ~ SUP + CP + SAP, data=dta.tmp, subset = my.train)
dt.restricted <- rpart(Y ~ SAP, data=dta.tmp, subset = my.train)

par(mfrow=c(1,2))
par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0), tcl=-0.3)
rpart.plot(dt.full, main="Full")
rpart.plot(dt.restricted, main="Restricted")


## Colors and plot params
my.col <- dta$x.sup
my.col[dta$x.sup==1] <- rgb(1,0,0,0.4)
my.col[dta$x.sup==0] <- rgb(0,0,1,0.4)

my.col.light <- dta$x.sup
my.col.light[dta$x.sup==1] <- rgb(1,0,0,0.02)
my.col.light[dta$x.sup==0] <- rgb(0,0,1,0.02)

my.xlim <- range(dta$x.sap)
my.ylim <- range(dta$y)

my.pch <- (3*dta$x.sup) + 16*(1-dta$x.sup)


cairo_ps("./figs/IllustrateExtrapolation.eps",family = 'Times', height=2,width=8,
         fallback_resolution = 600)
par(mfrow=c(1,4))
par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0), tcl=-0.3)
plot(dta$x.sap, jitter(dta$y), col=my.col, pch=my.pch,
     xlim=my.xlim, ylim=my.ylim, main="a) Data", xlab="SAP", ylab=expression(italic("y")),  font.main = 1)#"y")
text(0,7,"SUP=1",col="red")
text(-2,0,"SUP=0",col="blue")

plot(dta$x.sap, jitter(dta$y), col=my.col.light, pch=my.pch,
     xlim=my.xlim, ylim=my.ylim, main="b) Full", xlab="SAP", ylab=expression(italic("y")),  font.main = 1)#"y")
step.plot(xx = dta[!my.train,"x.sap"], yy = lm1[[4]], sup = dta[!my.train,"x.sup"], type="l",
          col="darkgreen", lty=2, lwd=2)
step.plot(xx = dta[!my.train,"x.sap"], yy = rp1[[4]], sup = dta[!my.train,"x.sup"],
          col=rgb(1,0,1,0.6), lwd=2)
text(-1,-2,"Linear",col="darkgreen")
text(-2.5,0,"Tree",col=rgb(1,0,1))


plot(dta$x.sap, jitter(dta$y), col=my.col.light, pch=my.pch,
     xlim=my.xlim, ylim=my.ylim, main="c) Marginalization", xlab="SAP", ylab=expression(italic("y")),  font.main = 1)#"y")
step.plot(xx = dta[!my.train,"x.sap"], yy = lm1[[5]], type="l",
          col="darkgreen", lty=2, lwd=2)
step.plot(xx = dta[!my.train,"x.sap"], yy = rp1[[5]],
          col=rgb(1,0,1,0.6), lwd=2)
text(-1.5,4,"Extrapolation",col=rgb(1,0,1))

plot(dta$x.sap, jitter(dta$y), col=my.col.light, pch=my.pch,
     xlim=my.xlim, ylim=my.ylim, main="d) Restricted", xlab="SAP", ylab=expression(italic("y")),  font.main = 1)#"y")
step.plot(xx = dta[!my.train,"x.sap"], yy = lm1[[6]], type="l",
          col="darkgreen", lty=2, lwd=2)
step.plot(xx = dta[!my.train,"x.sap"], yy = rp1[[6]], 
          col=rgb(1,0,1,0.6), lwd=2)
dev.off()
