## 8/31/2018
rm(list=ls())
library(readxl)
library(latex2exp)
library(calibrate)
library(readxl) 

#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/simulation_epsilon_revised/output/') # set to working path of all output of simulation

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


setwd("./output/")

file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a1b_PAPER_*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)


## initialize df for storing -- left panel
df_displace <- data.frame()

df_displace <- data.frame(displace=numeric(),
                 RF_degradation=numeric(), 
                 OLS_degradation=numeric())

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    tmp <- tmp[(tmp$delta_SUP_CP==1) & 
                       (tmp$shift_sd_record==1)  &
                       (((tmp$Beta1_SAP==1) & (tmp$Beta2_CP==1) & (tmp$Beta3_SUP==5))),]
    
    df_displace_add <- data.frame(displace=tmp$displace_record,
                              RF_degradation=tmp$RMSE_Restricted_DT_test - tmp$RMSE_Proposed_DT_test, 
                              OLS_degradation=tmp$RMSE_Restricted_OLS_test - tmp$RMSE_Proposed_OLS_test)
    
    df_displace <- rbind(df_displace, df_displace_add)
  }
}

## then aggregating by displace: compute 5th/95th-stats
head(df_displace)
table(df_displace$displace) ## spot-check we should have 100 in each b/c we did 100 simulations for each configuration across displace parameter


## RF
percentile_5th_RF <- aggregate(RF_degradation ~ displace, data = df_displace, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_RF <-aggregate(RF_degradation ~ displace, data = df_displace, FUN = function(x) quantile(x, probs = 0.95))


## OLS
percentile_5th_OLS <- aggregate(OLS_degradation ~ displace, data = df_displace, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_OLS <-aggregate(OLS_degradation ~ displace, data = df_displace, FUN = function(x) quantile(x, probs = 0.95))


df_percentile <- merge(percentile_5th_RF,
                  percentile_95th_RF,
                  by = 'displace',
                  how ='left')
df_percentile <- merge(df_percentile,
                       percentile_5th_OLS,
                       by = 'displace',
                       how ='left')
df_percentile <- merge(df_percentile,
                       percentile_95th_OLS,
                       by = 'displace',
                       how ='left')



## initialize df for storing -- left panel
df_mean_variance <- data.frame()
df_mean_variance <- data.frame(shift_sd_record=numeric(),
                          beta3=numeric(),
                          RF_degradation=numeric())


file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2a_*',file.list)]
df.list <-lapply(file.list, read_excel_allsheets)

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==1)
    
    df_mean_variance_add <- data.frame(shift_sd_record=tmp$shift_sd_record[idx],
                                       beta3=tmp$Beta3_SUP[idx],
                                       RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
  
   
    df_mean_variance <- rbind(df_mean_variance, df_mean_variance_add)
    
  
    idx <- which(tmp$Beta3_SUP==5)
    df_mean_variance_add <- data.frame(shift_sd_record=tmp$shift_sd_record[idx],
                                       beta3=tmp$Beta3_SUP[idx],
                                       RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_mean_variance <- rbind(df_mean_variance, df_mean_variance_add)

    idx <- which(tmp$Beta3_SUP==10)
    df_mean_variance_add <- data.frame(shift_sd_record=tmp$shift_sd_record[idx],
                                       beta3=tmp$Beta3_SUP[idx],
                                       RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    
    
    df_mean_variance <- rbind(df_mean_variance, df_mean_variance_add)
    
  }
}
## spot-check -- that we have 100 in the grid
table(df_mean_variance$beta3, df_mean_variance$shift_sd_record)

## now again -- create aggregate percentiles for each \beta3 x shift_sd_record combo
## RF
percentile_5th_RF <- aggregate(RF_degradation ~ beta3 + shift_sd_record, data = df_mean_variance, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_RF <-aggregate(RF_degradation ~ beta3 + shift_sd_record, data = df_mean_variance, FUN = function(x) quantile(x, probs = 0.95))



df_percentile_beta3 <- merge(percentile_5th_RF,
                       percentile_95th_RF,
                       by = c('beta3','shift_sd_record'),
                       how ='left')

head(df_percentile_beta3)


## eps version
#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/JITE_git_upload/Bias/code/b_PopeSydnor/1_MC_simulations/') 
cairo_ps("./figs/1_displacement.eps",family = 'Times', height=3.25,width=7.6,
         fallback_resolution = 600)
y_axis_label <- 'Predictive degradation'
layout(matrix(c(1,2),nrow=1,byrow = TRUE), widths = c(2.1,2), heights = c(0.8))
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=1.2)
plot(100,100,
     pch = '.',
     col = rgb(0,0,0,0.4),
     cex=3,
     ylim = c(-2, 0.7),
     xlim = c(0.07, 1.93),
     ylab=y_axis_label,
     xlab =expression("mean displacement ("*italic("\u03B7")*")"))


abline(h=0, col=rgb(0,0,0,0.4),lty=5)

polygon(c(df_percentile$displace, rev(df_percentile$displace)), 
        c(df_percentile$RF_degradation.x, rev(df_percentile$RF_degradation.y)),
        border = NA,
        lwd = 2,  
        col = rgb(0,0,0,0.2))
text(1.2, -0.58, 'Random forest', col='Black', adj = c(0,0))


polygon(c(df_percentile$displace, rev(df_percentile$displace)), 
        c(df_percentile$OLS_degradation.x, rev(df_percentile$OLS_degradation.y)),
        border = NA,
        col = rgb(0,0,0,0.6))
text(1.2, 0.32, 'Linear regression', col='Black', adj = c(0,0))


par(mar=c(3,1,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
par(cex.lab=1.2)

plot(100,100,
     xlim =c(0.23, 0.97),
     ylim = c(-2, 0.7),
     cex = 4,
     col=rgb(0,0,0,0.1),
     pch = '.',
     xlab = expression("mean-variance displacement ("*italic("\u03C3")*")"),
     ylab='')
abline(h=0, col=rgb(0,0,0,0.4),lty=5)


idx <- which(df_percentile_beta3$beta3==1)
polygon(c(df_percentile_beta3$shift_sd_record[idx], rev(df_percentile_beta3$shift_sd_record[idx])), 
        c(df_percentile_beta3$RF_degradation.x[idx], rev(df_percentile_beta3$RF_degradation.y[idx])),
        border = NA,
        col = rgb(0,0,0,0.9))
text(0.22, 0.4,  expression(italic("\u03B2")[3]*' = 1'),
     col='black',
     cex = 1.1, adj = c(0,0))


idx <- which(df_percentile_beta3$beta3==5)
polygon(c(df_percentile_beta3$shift_sd_record[idx], rev(df_percentile_beta3$shift_sd_record[idx])), 
        c(df_percentile_beta3$RF_degradation.x[idx], rev(df_percentile_beta3$RF_degradation.y[idx])),
        border = NA,
        col = rgb(0,0,0,0.6))

text(0.22, -0.25,expression(italic("\u03B2")[3]*' = 5'),
     col='black',
     cex = 1.1, adj = c(0,0))



idx <- which(df_percentile_beta3$beta3==10)
polygon(c(df_percentile_beta3$shift_sd_record[idx], rev(df_percentile_beta3$shift_sd_record[idx])), 
        c(df_percentile_beta3$RF_degradation.x[idx], rev(df_percentile_beta3$RF_degradation.y[idx])),
        border = NA,
        col = rgb(0,0,0,0.4))

text(0.22, -0.92,expression(italic("\u03B2")[3]*' = 10'),
     cex = 1.1, adj = c(0,0))
dev.off()




### Figure 2
#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/simulation_epsilon_revised/output/')

# set-up df for percentile calc.

## row 1) (vary \sigma)
df_sigma <- data.frame()
df_sigma <- data.frame(delta_SUP_CP=numeric(),
                               beta3=numeric(),
                          shift_sd_record=numeric(),
                       displace_record =numeric(),
                       RF_degradation=numeric())

file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2_new_vary*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]

  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==1 & tmp$displace_record==0)
    df_sigma_add <- data.frame(delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                       beta3=tmp$Beta3_SUP[idx],
                               shift_sd_record = tmp$shift_sd_record[idx],
                               displace_record = tmp$displace_record[idx],
                                       RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_sigma <- rbind(df_sigma, df_sigma_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==1 & tmp$displace_record==1)
    df_sigma_add <- data.frame(delta_SUP_CP=tmp$delta_SUP_CP[idx],
                               beta3=tmp$Beta3_SUP[idx],
                               shift_sd_record = tmp$shift_sd_record[idx],
                               displace_record = tmp$displace_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_sigma <- rbind(df_sigma, df_sigma_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.6 & tmp$displace_record==0)
    df_sigma_add <- data.frame(delta_SUP_CP=tmp$delta_SUP_CP[idx],
                               beta3=tmp$Beta3_SUP[idx],
                               shift_sd_record = tmp$shift_sd_record[idx],
                               displace_record = tmp$displace_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_sigma <- rbind(df_sigma, df_sigma_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.6 & tmp$displace_record==1)
    df_sigma_add <- data.frame(delta_SUP_CP=tmp$delta_SUP_CP[idx],
                               beta3=tmp$Beta3_SUP[idx],
                               shift_sd_record = tmp$shift_sd_record[idx],
                               displace_record = tmp$displace_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_sigma <- rbind(df_sigma, df_sigma_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.3 & tmp$displace_record==0)
    df_sigma_add <- data.frame(delta_SUP_CP=tmp$delta_SUP_CP[idx],
                               beta3=tmp$Beta3_SUP[idx],
                               shift_sd_record = tmp$shift_sd_record[idx],
                               displace_record = tmp$displace_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_sigma <- rbind(df_sigma, df_sigma_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.3 & tmp$displace_record==1)
    df_sigma_add <- data.frame(delta_SUP_CP=tmp$delta_SUP_CP[idx],
                               beta3=tmp$Beta3_SUP[idx],
                               shift_sd_record = tmp$shift_sd_record[idx],
                               displace_record = tmp$displace_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_sigma <- rbind(df_sigma, df_sigma_add)
  }
}



colnames(df_sigma)
table(df_sigma$delta_SUP_CP,
      df_sigma$shift_sd_record)

table(df_sigma$delta_SUP_CP[ df_sigma$displace_record==1],
      df_sigma$shift_sd_record[ df_sigma$displace_record==1])

table(df_sigma$delta_SUP_CP[ df_sigma$displace_record==0],
      df_sigma$shift_sd_record[ df_sigma$displace_record==0])


percentile_5th_RF <- aggregate(RF_degradation ~ delta_SUP_CP + displace_record + shift_sd_record, data = df_sigma, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_RF <-aggregate(RF_degradation ~ delta_SUP_CP + displace_record + shift_sd_record, data = df_sigma, FUN = function(x) quantile(x, probs = 0.95))
head(percentile_5th_RF)


df_percentile_SUP_CP <- merge(percentile_5th_RF,
                             percentile_95th_RF,
                             by = c('delta_SUP_CP','displace_record', 'shift_sd_record'),
                             how ='left')

head(df_percentile_SUP_CP)


## row 2) (vary \beta_3 get percentiles.
file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2c_*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)

df_delta0 <- data.frame()
df_delta0 <- data.frame(delta_C0=numeric(),
                        Beta3_SUP=numeric(),
                        delta_SUP_CP=numeric(),
                        displace_record =numeric(),
                        shift_sd_record = numeric(),
                       RF_degradation=numeric())

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    
    
    # \beta3=1
    idx <- which(tmp$Beta3_SUP==1 & tmp$delta_SUP_CP==1 & tmp$displace_record==0 & tmp$shift_sd_record==1  )
    df_delta0_add <- data.frame(delta_C0=tmp$delta_C0[idx],
                                Beta3_SUP=tmp$Beta3_SUP[idx],
                                delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                displace_record =tmp$displace_record[idx],
                                shift_sd_record = tmp$shift_sd_record[idx],
                                RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_delta0 <- rbind(df_delta0, df_delta0_add)
    
    
    idx <- which(tmp$Beta3_SUP==1 & tmp$delta_SUP_CP==1 & tmp$displace_record==1 & tmp$shift_sd_record==1  )
    df_delta0_add <- data.frame(delta_C0=tmp$delta_C0[idx],
                                Beta3_SUP=tmp$Beta3_SUP[idx],
                                delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                displace_record =tmp$displace_record[idx],
                                shift_sd_record = tmp$shift_sd_record[idx],
                                RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_delta0 <- rbind(df_delta0, df_delta0_add)
    
    
    # \beta3=5
    idx <- which(tmp$Beta3_SUP==5 & tmp$delta_SUP_CP==1 & tmp$displace_record==0 & tmp$shift_sd_record==1  )
    df_delta0_add <- data.frame(delta_C0=tmp$delta_C0[idx],
                                Beta3_SUP=tmp$Beta3_SUP[idx],
                                delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                displace_record =tmp$displace_record[idx],
                                shift_sd_record = tmp$shift_sd_record[idx],
                                RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_delta0 <- rbind(df_delta0, df_delta0_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$delta_SUP_CP==1 & tmp$displace_record==1 & tmp$shift_sd_record==1  )
    df_delta0_add <- data.frame(delta_C0=tmp$delta_C0[idx],
                                Beta3_SUP=tmp$Beta3_SUP[idx],
                                delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                displace_record =tmp$displace_record[idx],
                                shift_sd_record = tmp$shift_sd_record[idx],
                                RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_delta0 <- rbind(df_delta0, df_delta0_add)
    
    # \beta3=10
    idx <- which(tmp$Beta3_SUP==10 & tmp$delta_SUP_CP==1 & tmp$displace_record==0 & tmp$shift_sd_record==1  )
    df_delta0_add <- data.frame(delta_C0=tmp$delta_C0[idx],
                                Beta3_SUP=tmp$Beta3_SUP[idx],
                                delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                displace_record =tmp$displace_record[idx],
                                shift_sd_record = tmp$shift_sd_record[idx],
                                RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_delta0 <- rbind(df_delta0, df_delta0_add)
    
    idx <- which(tmp$Beta3_SUP==10 & tmp$delta_SUP_CP==1 & tmp$displace_record==1 & tmp$shift_sd_record==1 )
    df_delta0_add <- data.frame(delta_C0=tmp$delta_C0[idx],
                                Beta3_SUP=tmp$Beta3_SUP[idx],
                                delta_SUP_CP=tmp$delta_SUP_CP[idx],
                                displace_record =tmp$displace_record[idx],
                                shift_sd_record = tmp$shift_sd_record[idx],
                                RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx])
    df_delta0 <- rbind(df_delta0, df_delta0_add)
  }
}

## spot-check 100 iterations per parameter combo
colnames(df_delta0)
table(df_delta0$delta_C0[df_delta0$displace_record==0], 
      df_delta0$Beta3_SUP[df_delta0$displace_record==0])
table(df_delta0$delta_C0[df_delta0$displace_record==1], 
      df_delta0$Beta3_SUP[df_delta0$displace_record==1])



percentile_5th_RF <- aggregate(RF_degradation ~delta_C0 + displace_record + Beta3_SUP, data = df_delta0, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_RF <-aggregate(RF_degradation ~ delta_C0 + displace_record + Beta3_SUP, data = df_delta0, FUN = function(x) quantile(x, probs = 0.95))

head(percentile_5th_RF)


df_percentile_delta0 <- merge(percentile_5th_RF,
                              percentile_95th_RF,
                              by = c('delta_C0','displace_record', 'Beta3_SUP'),
                              how ='left')

df_percentile_delta0[which(df_percentile_delta0$Beta3_SUP==1 & df_percentile_delta0$displace_record==0),]


## eps version
#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/JITE_git_upload/Bias/code/b_PopeSydnor/1_MC_simulations/') # here only
cairo_ps("./figs/2_vary_delta.eps",family = 'Times', height=4.4,width=8,
         fallback_resolution = 600)
pch0 = '.'
pch1 = 16
cex0 = 3
cex1 = 1.2
col0 = rgb(0,0,0,0.4)
col1 = rgb(0,0,0,0.7)

cex_size <- 1.1
cex.main<- 1
layout(matrix(c(1,2,3,4,1,5,6,7), 2, 4, byrow = TRUE), widths = c(0.12,1,1,1), heights = c(1,1,1))#heights = c(0.25,1,1,1))
par(mar=c(1,0.5,1,0.5),mgp=c(1.5,0.5,0),tcl=0)
plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.7,1.5,y_axis_label, cex=1.5, srt = 90)


#setwd('~/Dropbox/YelpBias/kristen_sandbox/final_paper_code/simulation_epsilon_revised/output/')
file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2_vary*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)




par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size)
##top row.
plot(100,100,
     xlim =c(0.04, 1.96),
     ylim = c(-0.8, 0.8),
     cex = cex0,
     col=col0, pch = pch0,
     xlab = expression(italic("\u03B4")['CP']),
     ylab='',
     main = expression('a) '*italic("\u03C3")~'= 1',font.main=1))

idx <- which(df_percentile_SUP_CP$shift_sd_record==1 & df_percentile_SUP_CP$displace_record==0)
polygon(c(df_percentile_SUP_CP$delta_SUP_CP[idx], rev(df_percentile_SUP_CP$delta_SUP_CP[idx])), 
        c(df_percentile_SUP_CP$RF_degradation.x[idx], rev(df_percentile_SUP_CP$RF_degradation.y[idx])),
        border = NA,
        col = col0)
text(0.1,0.44,expression(italic("\u03B7")~'= 0'),
     col='black', 
     cex = 1.1, adj = c(0,0))

idx <- which(df_percentile_SUP_CP$shift_sd_record==1 & df_percentile_SUP_CP$displace_record==1)
polygon(c(df_percentile_SUP_CP$delta_SUP_CP[idx], rev(df_percentile_SUP_CP$delta_SUP_CP[idx])), 
        c(df_percentile_SUP_CP$RF_degradation.x[idx], rev(df_percentile_SUP_CP$RF_degradation.y[idx])),
        border = NA,
        col = col1)
text(0.1,-0.19,expression(italic("\u03B7")~'= 1'),
     col='black', 
     cex = 1.1, adj = c(0,0))

abline(h=0, col=rgb(0,0,0,0.4),lty=5)


par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size, cex.main = cex.main)
plot(100,100,
     xlim =c(0.04, 1.96),
     ylim = c(-0.8, 0.8),
     cex = cex0,
     col=col0, pch = pch0,
     xlab = expression(italic("\u03B4")['CP']),
     ylab='',
     main = expression('b) '*italic("\u03C3")~'= 0.6'),font.main=1)
idx <- which(df_percentile_SUP_CP$shift_sd_record==0.6 & df_percentile_SUP_CP$displace_record==0)
polygon(c(df_percentile_SUP_CP$delta_SUP_CP[idx], rev(df_percentile_SUP_CP$delta_SUP_CP[idx])), 
        c(df_percentile_SUP_CP$RF_degradation.x[idx], rev(df_percentile_SUP_CP$RF_degradation.y[idx])),
        border = NA,
        col = col0)

idx <- which(df_percentile_SUP_CP$shift_sd_record==0.6 & df_percentile_SUP_CP$displace_record==1)
polygon(c(df_percentile_SUP_CP$delta_SUP_CP[idx], rev(df_percentile_SUP_CP$delta_SUP_CP[idx])), 
        c(df_percentile_SUP_CP$RF_degradation.x[idx], rev(df_percentile_SUP_CP$RF_degradation.y[idx])),
        border = NA,
        col = col1)
abline(h=0, col=rgb(0,0,0,0.4),lty=5)

par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size, cex.main = cex.main)
plot(100,100,
     xlim =c(0.04, 1.96),
     ylim = c(-0.8, 0.8),
     cex = cex0,
     col=col0, pch = pch0,
     xlab = expression(italic("\u03B4")['CP']),
     ylab='',
     main = expression('c) '*italic("\u03C3")~'= 0.3'),font.main=1)#expression(sigma==0.3))
idx <- which(df_percentile_SUP_CP$shift_sd_record==0.3 & df_percentile_SUP_CP$displace_record==0)
polygon(c(df_percentile_SUP_CP$delta_SUP_CP[idx], rev(df_percentile_SUP_CP$delta_SUP_CP[idx])), 
        c(df_percentile_SUP_CP$RF_degradation.x[idx], rev(df_percentile_SUP_CP$RF_degradation.y[idx])),
        #lty = "dashed", lwd = 2,  
        border = NA,
        col = col0)

idx <- which(df_percentile_SUP_CP$shift_sd_record==0.3 & df_percentile_SUP_CP$displace_record==1)
polygon(c(df_percentile_SUP_CP$delta_SUP_CP[idx], rev(df_percentile_SUP_CP$delta_SUP_CP[idx])), 
        c(df_percentile_SUP_CP$RF_degradation.x[idx], rev(df_percentile_SUP_CP$RF_degradation.y[idx])),
        border = NA,
        col = col1)
abline(h=0, col=rgb(0,0,0,0.4),lty=5)
abline(h=0, col=rgb(0,0,0,0.4),lty=5)



## bottom row.
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size, cex.main = cex.main)
plot(100,100,
     xlim =c(-1.93, 1.93),
     ylim = c(-2, 0.8),
     cex = cex0,
     col=col0, pch = pch0,
     xlab = expression(italic("\u03B4")[0]),
     main =  expression('d) '*italic("\u03B2")[3]~'= 1'),font.main=1,
     ylab='')
abline(h=0, col=rgb(0,0,0,0.4),lty=5)
idx <- which(df_percentile_delta0$displace_record==0 & df_percentile_delta0$Beta3_SUP==1)

t <- df_percentile_delta0[idx,]
t <- t[order(t$delta_C0),]
polygon(c(t$delta_C0, rev(t$delta_C0)), 
        c(t$RF_degradation.x, rev(t$RF_degradation.y)),
        border = NA,
        col = col0)

idx <- which(df_percentile_delta0$displace_record==1 & df_percentile_delta0$Beta3_SUP==1)
t <- df_percentile_delta0[idx,]
t <- t[order(t$delta_C0),]
polygon(c(t$delta_C0, rev(t$delta_C0)), 
        c(t$RF_degradation.x, rev(t$RF_degradation.y)),
        border = NA,
        col = col1)

par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size, cex.main = cex.main)
plot(100,100,
     xlim =c(-1.93, 1.93),
     ylim = c(-2, 0.8), main =  expression('e) '*italic("\u03B2")[3]~'= 5'),#expression(beta[3]==5),
     cex = cex0,font.main=1,
     col=col0, pch = pch0,
     xlab = expression(italic("\u03B4")[0]),#expression(delta[0]),
     ylab='')
abline(h=0, col=rgb(0,0,0,0.4),lty=5)
idx <- which(df_percentile_delta0$displace_record==0 & df_percentile_delta0$Beta3_SUP==5)
t <- df_percentile_delta0[idx,]
t <- t[order(t$delta_C0),]
polygon(c(t$delta_C0, rev(t$delta_C0)), 
        c(t$RF_degradation.x, rev(t$RF_degradation.y)),
        border = NA,
        col = col0)

idx <- which(df_percentile_delta0$displace_record==1 & df_percentile_delta0$Beta3_SUP==5)
t <- df_percentile_delta0[idx,]
t <- t[order(t$delta_C0),]
polygon(c(t$delta_C0, rev(t$delta_C0)), 
        c(t$RF_degradation.x, rev(t$RF_degradation.y)),
        border = NA,
        col = col1)



par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size, cex.main = cex.main)
plot(100,100,
     xlim =c(-1.93, 1.93),
     ylim = c(-2, 0.8), main =  expression('f) '*italic("\u03B2")[3]~'= 10'),#expression(beta[3]==10),
     cex = cex0,
     col=col0, pch = pch0,font.main=1,
     xlab = expression(italic("\u03B4")[0]),
     ylab='')
idx <- which(df_percentile_delta0$displace_record==0 & df_percentile_delta0$Beta3_SUP==10)
t <- df_percentile_delta0[idx,]
t <- t[order(t$delta_C0),]
polygon(c(t$delta_C0, rev(t$delta_C0)), 
        c(t$RF_degradation.x, rev(t$RF_degradation.y)),
        border = NA,
        col = col0)

idx <- which(df_percentile_delta0$displace_record==1 & df_percentile_delta0$Beta3_SUP==10)
t <- df_percentile_delta0[idx,]
t <- t[order(t$delta_C0),]
polygon(c(t$delta_C0, rev(t$delta_C0)), 
        c(t$RF_degradation.x, rev(t$RF_degradation.y)),
        border = NA,
        col = col1)
abline(h=0, col=rgb(0,0,0,0.4),lty=5)
dev.off()






### Figure 3 (\beta4)
#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/simulation_epsilon_revised/output/')
file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2d_*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)

df_beta4 <- data.frame()
df_beta4 <- data.frame(displace_record=numeric(),
                       Beta3_SUP=numeric(),
                       Beta4_SAP_SUP=numeric(),
                       shift_sd_record =numeric(),
                        RF_degradation=numeric(),
                       OLS_degradation=numeric())

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==0 & tmp$shift_sd_record==1)
    df_beta4_add <- data.frame(displace_record=tmp$displace_record[idx],
                               Beta3_SUP=tmp$Beta3_SUP[idx],
                               Beta4_SAP_SUP=tmp$Beta4_SAP_SUP[idx],
                               shift_sd_record =tmp$shift_sd_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
                               OLS_degradation=tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx])
    df_beta4 <- rbind(df_beta4, df_beta4_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==0.5 & tmp$shift_sd_record==1)
    df_beta4_add <- data.frame(displace_record=tmp$displace_record[idx],
                               Beta3_SUP=tmp$Beta3_SUP[idx],
                               Beta4_SAP_SUP=tmp$Beta4_SAP_SUP[idx],
                               shift_sd_record =tmp$shift_sd_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
                               OLS_degradation=tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx])
    df_beta4 <- rbind(df_beta4, df_beta4_add)
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==1 & tmp$shift_sd_record==1)
    df_beta4_add <- data.frame(displace_record=tmp$displace_record[idx],
                               Beta3_SUP=tmp$Beta3_SUP[idx],
                               Beta4_SAP_SUP=tmp$Beta4_SAP_SUP[idx],
                               shift_sd_record =tmp$shift_sd_record[idx],
                               RF_degradation=tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
                               OLS_degradation=tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx])
    df_beta4 <- rbind(df_beta4, df_beta4_add)
    
    
  }
}

## spot-check should be 100 in each cell
table(df_beta4$displace_record,
      df_beta4$Beta4_SAP_SUP)
    

colnames(df_beta4)

percentile_5th_RF <- aggregate(RF_degradation ~displace_record + Beta4_SAP_SUP, data = df_beta4, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_RF <-aggregate(RF_degradation ~displace_record + Beta4_SAP_SUP, data = df_beta4, FUN = function(x) quantile(x, probs = 0.95))

percentile_5th_OLS <- aggregate(OLS_degradation ~displace_record + Beta4_SAP_SUP, data = df_beta4, FUN = function(x) quantile(x, probs = 0.05))
percentile_95th_OLS <-aggregate(OLS_degradation ~displace_record + Beta4_SAP_SUP, data = df_beta4, FUN = function(x) quantile(x, probs = 0.95))


df_percentile_beta4 <- merge(percentile_5th_RF,
                              percentile_95th_RF,
                              by = c('displace_record','Beta4_SAP_SUP'),
                              how ='left')

df_percentile_beta4 <- merge(df_percentile_beta4,
                             percentile_5th_OLS,
                             by = c('displace_record','Beta4_SAP_SUP'),
                             how ='left')

df_percentile_beta4 <- merge(df_percentile_beta4,
                             percentile_95th_OLS,
                             by = c('displace_record','Beta4_SAP_SUP'),
                             how ='left')

head(df_percentile_beta4)


## eps version
#setwd('/Users/kristen/Dropbox/YelpBias/kristen_sandbox/final_paper_code/JITE_git_upload/Bias/code/b_PopeSydnor/1_MC_simulations/') # here only

cairo_ps("./figs/3_SAP_SUP_interactions.eps",
         height=4,width=8,fallback_resolution = 600,family = 'Times')

col0 = rgb(0,0,0,0.3)
col1 = rgb(0,0,0,0.5)
col2 = rgb(0,0,0,0.8)

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(1,1), heights = c(1))

par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=1.2)

plot(100,100,
     xlim =c(0.07,1.93),
     ylim = c(-4.5, 0.8), 
     col='red',
     xlab = expression(italic("\u03B7")),
     ylab=y_axis_label,main = 'a) Linear regression', font.main = 1)
idx <- which(df_percentile_beta4$Beta4_SAP_SUP==0)

polygon(c(df_percentile_beta4$displace_record[idx], rev(df_percentile_beta4$displace_record[idx])), 
        c(df_percentile_beta4$OLS_degradation.x[idx], rev(df_percentile_beta4$OLS_degradation.y[idx])),
        border = NA,
        col = col0)
abline(h=0, col=rgb(0,0,0,0.4),lty=5)


idx <- which(df_percentile_beta4$Beta4_SAP_SUP==0.5)

polygon(c(df_percentile_beta4$displace_record[idx], rev(df_percentile_beta4$displace_record[idx])), 
        c(df_percentile_beta4$OLS_degradation.x[idx], rev(df_percentile_beta4$OLS_degradation.y[idx])),
        border = NA,
        col = col1)

idx <- which(df_percentile_beta4$Beta4_SAP_SUP==1)
polygon(c(df_percentile_beta4$displace_record[idx], rev(df_percentile_beta4$displace_record[idx])), 
        c(df_percentile_beta4$OLS_degradation.x[idx], rev(df_percentile_beta4$OLS_degradation.y[idx])),
        border = NA,
        col = col2)
par(mar=c(3,1,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

par(cex.lab=1.2)
plot(100,100,
     xlim =c(0.07,1.93),
     ylim = c(-4.5, 0.8),yaxt = 'n',
     cex = cex0,
     col=col0, pch = pch0,
     xlab = expression(italic("\u03B7")),
     ylab='',main = 'b) Random forest', font.main = 1)

idx <- which(df_percentile_beta4$Beta4_SAP_SUP==0)

polygon(c(df_percentile_beta4$displace_record[idx], rev(df_percentile_beta4$displace_record[idx])), 
        c(df_percentile_beta4$RF_degradation.x[idx], rev(df_percentile_beta4$RF_degradation.y[idx])),
        border = NA,
        col = col0)


idx <- which(df_percentile_beta4$Beta4_SAP_SUP==0.5)

polygon(c(df_percentile_beta4$displace_record[idx], rev(df_percentile_beta4$displace_record[idx])), 
        c(df_percentile_beta4$RF_degradation.x[idx], rev(df_percentile_beta4$RF_degradation.y[idx])),
        border = NA,
        col = col1)

idx <- which(df_percentile_beta4$Beta4_SAP_SUP==1)

polygon(c(df_percentile_beta4$displace_record[idx], rev(df_percentile_beta4$displace_record[idx])), 
        c(df_percentile_beta4$RF_degradation.x[idx], rev(df_percentile_beta4$RF_degradation.y[idx])),
        border = NA,
        col = col2)

abline(h=0, col=rgb(0,0,0,0.4),lty=5)
text(1.65,-0.2, expression(italic("\u03B2")[4]*' = 0.0'), col='black', cex = cex_size,adj = c(0,0))
text(1.65, -1.2, expression(italic("\u03B2")[4]*' = 0.5'), col='black', cex = cex_size,adj = c(0,0))
text(1.65, -2.4, expression(italic("\u03B2")[4]*' = 1.0'), col='black', cex = cex_size,adj = c(0,0))
dev.off()