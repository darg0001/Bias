##
## Complaints vs. scores in NY data
##

rm(list=ls())

#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/JITE_git_upload/Bias/code/a_EthnicBias/')

library(data.table)
library(lubridate)
library(xtable)
library(lfe)

## Read data
dta <- fread("../../data/NYData.csv")


## Descriptive statistics

n1 <- sum(dta$Asian==1)
n0 <- sum(dta$Asian==0)
n1
n0

t.test(SCORE ~ Asian, data=dta)
sd(dta$SCORE[dta$Asian==1])/sqrt(n1)
sd(dta$SCORE[dta$Asian==0])/sqrt(n0)

t.test(avg.score ~ Asian, data=dta)
sd(dta$avg.score[dta$Asian==1])/sqrt(n1)
sd(dta$avg.score[dta$Asian==0])/sqrt(n0)

t.test(complaintBinary ~ Asian, data=dta)
sd(dta$complaintBinary[dta$Asian==1])/sqrt(n1)
sd(dta$complaintBinary[dta$Asian==0])/sqrt(n0)

all.boro <- unique(dta$BORO)
n.boro <- length(all.boro)
boro.tab <- data.frame(matrix(NA, n.boro,7))
for(i in 1:n.boro){
    boro.tab[i,1] <- all.boro[i]
    my.t <- t.test((dta$BORO==all.boro[i]) ~ dta$Asian)
    boro.tab[i,2] <- my.t$estimate[2]*100
    p1 <- mean(dta$BORO==all.boro[i] & dta$Asian==1)
    boro.tab[i,3] <- sqrt(p1 * (1-p1) /nrow(dta))*100
    boro.tab[i,4] <- my.t$estimate[1]*100
    p0 <- mean(dta$BORO==all.boro[i] & dta$Asian==0)
    boro.tab[i,5] <- sqrt(p0 * (1-p0)/nrow(dta))*100
    boro.tab[i,6] <- my.t$p.value
    boro.tab[i,7] <- sum(dta$BORO==all.boro[i])
}
print(xtable(boro.tab[,1:6], digits = c(0,0,2,2,2,2,2)), include.rownames=FALSE)


## ZIP codes
all.zip <- unique(dta$ZIPCODE)
n.zip <- length(all.zip)
zip.tab <-  data.frame(matrix(NA, n.zip,7))
for(i in 1:n.zip){
    zip.tab[i,1] <- all.zip[i]
    my.t <- t.test((dta$ZIPCODE==all.zip[i]) ~ dta$Asian)
    zip.tab[i,2] <- my.t$estimate[2]*100
    p1 <- mean(dta$ZIPCODE==all.zip[i] & dta$Asian==1)
    zip.tab[i,3] <- sqrt(p1 * (1-p1)/nrow(dta))*100
    zip.tab[i,4] <- my.t$estimate[1]*100
    p0 <- mean(dta$ZIPCODE==all.zip[i] & dta$Asian==0)
    zip.tab[i,5] <- sqrt(p0 * (1-p0)/nrow(dta))*100
    zip.tab[i,6] <- my.t$p.value
    zip.tab[i,7] <- sum(dta$ZIPCODE==all.zip[i])
}
zip.tab <- zip.tab[order(zip.tab[,7], decreasing=T),]
print(xtable(zip.tab[zip.tab[,7]>1000,1:6], digits = c(0,0,2,2,2,2,2)), include.rownames=FALSE)


## =========================
## DESCRIPTIVE PLOT
## =========================


my.breaks <- seq(0,max(dta$SCORE),by=2)
bin.labels <- 1:(length(my.breaks)-1)

dta$bin <- as.numeric(cut(dta$SCORE, breaks=my.breaks, labels=bin.labels, include.lowest=T))
complaint.agg <- aggregate(complaint ~ bin + Asian, data=dta, FUN=mean)
count.agg <- aggregate(rep(1,nrow(dta)) ~ bin + Asian, data=dta, FUN=sum)
complaint.agg$count <- count.agg[,3]
complaint.agg$se <- complaint.agg$complaint * (1-complaint.agg$complaint) / sqrt(complaint.agg$count)


## Plotting as pdf
pdf("../../figs/NY-ScoreComplaint.pdf", height=4.5, width=6)
par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0), tcl=-0.3,family = 'Times')
plot(complaint.agg$bin[complaint.agg$Asian==0],
     complaint.agg$complaint[complaint.agg$Asian==0], xlim=c(0,20),
     ylim=c(0, 0.07),
     axes=F,
     pch=16, cex=sqrt(complaint.agg$count[complaint.agg$Asian==0]/pi)*0.1,
     col=rgb(0,0,0,0.4), xlab="Inspection Score", ylab="Complaint")#, main="Inspection Score and 311 Complaint")
points(complaint.agg$bin[complaint.agg$Asian==1],
       complaint.agg$complaint[complaint.agg$Asian==1],col="black",
       pch=1, cex=sqrt(complaint.agg$count[complaint.agg$Asian==1]/pi)*0.1, lwd=1)
text(2.5,0.03, "Non-Asian",cex=0.8)
text(5, 0.06, "Asian",cex=0.8,col="black")
axis(1, at=bin.labels-0.5, labels=my.breaks[1:(length(my.breaks)-1)])
axis(2)
box()
dev.off()


## Plotting as eps
cairo_ps("../../figs/NY-ScoreComplaint.eps",family = 'Times',height=4.5, width=6,
         fallback_resolution = 600)
par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0), tcl=-0.3)
plot(complaint.agg$bin[complaint.agg$Asian==0],
     complaint.agg$complaint[complaint.agg$Asian==0], xlim=c(0,20),
     ylim=c(0, 0.07),
     axes=F,
     pch=16, cex=sqrt(complaint.agg$count[complaint.agg$Asian==0]/pi)*0.1,
     col=rgb(0,0,0,0.4), xlab="Inspection Score", ylab="Complaint")#, main="Inspection Score and 311 Complaint")
points(complaint.agg$bin[complaint.agg$Asian==1],
       complaint.agg$complaint[complaint.agg$Asian==1],col="black",
       pch=1, cex=sqrt(complaint.agg$count[complaint.agg$Asian==1]/pi)*0.1, lwd=1)
text(2.5,0.03, "Non-Asian",cex=0.8)
text(5, 0.06, "Asian",cex=0.8,col="black")
axis(1, at=bin.labels-0.5, labels=my.breaks[1:(length(my.breaks)-1)])
axis(2)
box()
dev.off()


## QQ plot
cairo_ps('../../figs/QQplot_NY.eps', height=3.5,width=3.5,family = 'Times')
par(mfrow=c(1,1))
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
with(dta,
     qqplot(SCORE[Asian==0],
            SCORE[Asian==1],
            xlim=c(0,80),ylim=c(0, 80),
            pch=16,cex=0.4,col=rgb(0,0,0,0.4),
            xlab="Non-Asian scores", ylab="Asian scores",
            main="b) New York",font.main=1)
)
abline(a=0,b=1,col="darkgrey")

dev.off()





## =========================
## ANALYSIS
## =========================


lfe0 <- felm(complaint ~ Asian | 0 | 0 | CAMIS, data = dta)
lfe1 <- felm(complaint ~ Asian + I(SCORE/100) + I(avg.score/100) | 0 | 0 | CAMIS, data = dta)
lfe2 <- felm(complaint ~ Asian + I(SCORE/100) + I(avg.score/100) | year  | 0 | CAMIS, data = dta)
lfe3 <- felm(complaint ~ Asian + I(SCORE/100) + I(avg.score/100) | year + BORO | 0 | CAMIS, data = dta)
lfe4 <- felm(complaint ~ Asian + I(SCORE/100) + I(avg.score/100) | year + ZIPCODE | 0 | CAMIS, data = dta)
summary(lfe0)
summary(lfe1)
summary(lfe2)
summary(lfe3)
summary(lfe4)


