##
## Analysis of potential ethnic bias in King County data
##

##=========================================
##  SETTING UP DATA
##=========================================

## Clearing memory
rm(list=ls())


## Packages 
library(data.table) #to read in larger datasets more effectively
library(lfe) # clustered SEs in OLS
library(clusterSEs) # clustered SEs for GLM 
library(xtable)


## Read in data 
dta <- fread("instances_mergerd_seattle.csv", verbose=F)

## Augment year
year <- as.numeric(substr(dta$inspection_period_end_date,start=1,stop=4))
dta <- data.frame(dta, year=year)

## One coding error
dta$inspection_prev_penalty_score[which(dta$inspection_prev_penalty_score==-1)] <- 0
dta$inspection_average_prev_penalty_scores[which(dta$inspection_average_prev_penalty_scores==-1)] <- 0


##
## Identify terms that indicate food poisoning (see NYC Yelp piece) 
##
poison <- as.numeric(grepl("food poisoning",dta$review_contents))
vomit <- as.numeric(grepl("vomit",dta$review_contents))
diarrhea <- as.numeric(grepl("diarrhea", dta$review_contents))
sick <- as.numeric(grepl("sick", dta$review_contents))
homesick <- as.numeric(grepl("homesick", dta$review_contents))
sick2 <- as.numeric(sick==1 & homesick==0)

## Foodborne illness indicators
fbi <- as.numeric((poison+vomit+diarrhea+sick)>0)
fbi2 <- as.numeric((poison+vomit+diarrhea+sick2)>0)


## Augmenting data frame
dta <- data.frame(dta, poison = poison, vomit = vomit, diarrhea = diarrhea, sick = sick,
                  sick2=sick2, fbi = fbi, fbi2 = fbi2)


##
## Cuisine classification
##
cuisines <- unique(dta$cuisines)
cuisines <- gsub("\\[|\\]|'","", cuisines)
cuisines <- strsplit(cuisines,", ")
cuisines <- unique(unlist(cuisines))

## Formatting as a matrix
cuisine.mat <- data.frame(matrix(NA, nrow(dta), length(cuisines)))
names(cuisine.mat) <- cuisines
for(i in 1:length(cuisines)){
    cuisine.mat[,i] <- as.numeric(grepl(cuisines[i], dta$cuisines))
}

## Manually augment cuisines with "Asian" and "ethnic"
labels <- read.csv("cuisines.csv", header=T, as.is=T)


## Simple indicator of Asian / Ethnic restaurant
##
asian <- as.numeric(apply(cuisine.mat[,labels$asian==1],1,sum)>0)
ethnic <- as.numeric(apply(cuisine.mat[,labels$ethnic==1],1,sum)>0)

## Augmenting data frame
dta <- data.frame(dta, asian = asian, ethnic = ethnic)



##=========================================
##  DESCRIPTIVE STATISTICS
##=========================================

## Covariates
n1 <- sum(dta$asian==1)
n0 <- sum(dta$asian==0)

mean(dta$inspection_prev_penalty_score[dta$asian==1])
sd(dta$inspection_prev_penalty_score[dta$asian==1])/sqrt(n1)
mean(dta$inspection_prev_penalty_score[dta$asian==0])
sd(dta$inspection_prev_penalty_score[dta$asian==0])/sqrt(n0)
t.test(dta$inspection_prev_penalty_score ~ I(dta$asian==1))

mean(dta$inspection_average_prev_penalty_scores[dta$asian==1])
sd(dta$inspection_average_prev_penalty_scores[dta$asian==1])/sqrt(n1)
mean(dta$inspection_average_prev_penalty_scores[dta$asian==0])
sd(dta$inspection_average_prev_penalty_scores[dta$asian==0])/sqrt(n0)
t.test(dta$inspection_average_prev_penalty_scores ~ I(dta$asian==1))

mean(dta$review_count[dta$asian==1])
sd(dta$review_count[dta$asian==1])/sqrt(n1)
mean(dta$review_count[dta$asian==0])
sd(dta$review_count[dta$asian==0])/sqrt(n0)
t.test(review_count ~ I(asian==1), data=dta)

mean(dta$average_review_rating[dta$asian==1])
sd(dta$average_review_rating[dta$asian==1])/sqrt(n1)
mean(dta$average_review_rating[dta$asian==0])
sd(dta$average_review_rating[dta$asian==0])/sqrt(n0)
t.test(average_review_rating ~ I(asian==1), data=dta)


## Zip code imbalance
all.zip <- unique(dta$zip_code)
n.zip <- length(all.zip)
zip.mat <- matrix(NA, nrow(dta), n.zip)
for(i in 1:n.zip){
    zip.mat[,i] <- as.numeric(dta$zip_code==all.zip[i])
}
zip.mat <- data.frame(zip.mat)
names(zip.mat) <- paste("zip",all.zip, sep="")
dta <- data.frame(dta, zip.mat)


## Balance table
tab.out <- matrix(NA, n.zip, 7)
for(i in 1:n.zip){
    
    my.t <- t.test(zip.mat[,i] ~ dta$asian)
    tab.out[i,1] <- all.zip[i]
    tab.out[i,2] <- my.t$estimate[2]*100
    p1 <- mean(zip.mat[,i]==1 & asian==1)
    tab.out[i,3] <- sqrt(p1 * (1-p1)/nrow(dta))*100
    tab.out[i,4] <- my.t$estimate[1]*100
    p0 <- mean(zip.mat[,i]==1 & asian==0)
    tab.out[i,5] <- sqrt(p0 * (1-p0)/nrow(dta))*100
    tab.out[i,6] <- my.t$p.value
    tab.out[i,7] <- sum(zip.mat[,i])
}

tab.out <- tab.out[order(tab.out[,7], decreasing=T),]
tab.out <- data.frame(tab.out)


## Binning to visualize data points
all.scores <- seq(min(dta$inspection_average_prev_penalty_scores),
                  max(dta$inspection_average_prev_penalty_scores), length=1000)
bin.cuts <- c(seq(0,35, by=2),90)
k.bin <- length(bin.cuts)-1
store <- matrix(NA, k.bin, 5)
for(i in 1:k.bin){
    which.sub <- dta$inspection_average_prev_penalty_scores>= bin.cuts[i] &
        dta$inspection_average_prev_penalty_scores < bin.cuts[i+1]
    dta.sub <- dta[which.sub,]
    store[i,1] <- mean(dta.sub$fbi[dta.sub$asian==0])
    store[i,2] <- mean(dta.sub$fbi[dta.sub$asian==1])
    store[i,3] <- sum(dta.sub$asian==0)
    store[i,4] <- sum(dta.sub$asian==1)
    store[i,5] <- sum(bin.cuts[i]+bin.cuts[i+1])/2
}
## Fixing up final bin (so that all data is displayed)
store[k.bin,5] <- 34
store <- data.frame(store)
names(store) <- c("mean.nonasian","mean.asian", "n.nonasian", "n.asian", "mid")



## Plotting
par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0),tcl=-0.3)
plot(store$mid, store[,1],type="n",ylim=c(0.035,0.15),xlim=c(0,35),
     main="Inspection Score and Food Poisoning Review", xlab="Inspection score", ylab="Probability of term")
points(store$mid, store$mean.nonasian,cex=sqrt(store$n.nonasian/pi)*0.2,col=rgb(0,0,0,0.4),pch=16)
points(store$mid, store$mean.asian,cex=sqrt(store$n.asian/pi)*0.2,col="black", pch=1)
text(6, 0.11, "Asian",col="black")
text(5, 0.05, "Non-Asian")



##=========================================
##   LOGIT MODELS
##=========================================


##
## Asian measure
##

## Bivariate comparison 
m0 <-glm(fbi2 ~ asian, family="binomial", data=dta)
summary(m0)
m0c <- cluster.bs.glm(m0, dta, cluster= ~ restaurant_id)
dim(model.matrix(m0))


## Different covariate sets
m1 <- glm(fbi2 ~ asian + inspection_prev_penalty_score + review_count + as.factor(year), family="binomial", data=dta)
summary(m1)
m1c <- cluster.bs.glm(m1, dta, cluster= ~ restaurant_id)  ## Cluster bootstrapped SEs
dim(model.matrix(m1))

m2 <- glm(fbi2 ~ asian + inspection_average_prev_penalty_scores + review_count + as.factor(year), family="binomial", data=dta)
summary(m2)
m2c <- cluster.bs.glm(m2, dta, cluster= ~ restaurant_id)  ## Cluster bootstrapped SEs
dim(model.matrix(m2))

m3 <- glm(fbi2 ~ asian + inspection_average_prev_penalty_scores + review_count + average_review_rating + as.factor(year), family="binomial", data=dta)
summary(m3)
m3c <- cluster.bs.glm(m3, dta, cluster= ~ restaurant_id)  ## Cluster bootstrapped SEs
dim(model.matrix(m3))

m4 <- glm(fbi2 ~ asian + inspection_average_prev_penalty_scores + review_count + average_review_rating + as.factor(zip_code) + as.factor(year), family="binomial", data=dta)
summary(m4)
m4c <- cluster.bs.glm(m4, dta, cluster= ~ restaurant_id)  ## Cluster bootstrapped SEs
dim(model.matrix(m4))


## Robust SEs
bsse1 = (m1c$ci[, 2] - m1c$ci[, 1]) / (2 * 1.96)
bsse2 = (m2c$ci[, 2] - m2c$ci[, 1]) / (2 * 1.96)
bsse3 = (m3c$ci[, 2] - m3c$ci[, 1]) / (2 * 1.96)
bsse4 = (m4c$ci[, 2] - m4c$ci[, 1]) / (2 * 1.96)
