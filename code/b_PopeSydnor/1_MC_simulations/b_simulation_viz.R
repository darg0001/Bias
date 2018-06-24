rm(list=ls())
library(readxl)
library(latex2exp)
library(calibrate)
library(readxl)    

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
y_axis_label <- 'Predictive Degradation'


##
## CREATE 1-FIGURE
##
pdf("./figs/1_displacement.pdf", height=4,width=8)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths = c(2,2), heights = c(0.2,1))
par(mar=c(1,3,1,1),mgp=c(1.5,0.5,0),tcl=-0.3)
plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.5,1.5,'Effect of SAP Displacement', cex=1.4)

file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a1b_PAPER_*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)
cex_size = 1.2

num_pts1 = 0
num_pts2 = 0

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  
  for(j in 1:length(tmp1)){
      tmp <- tmp1[[j]]
      
      if(sum(tmp$displace_record==0.25)>0){
        num_pts1 = num_pts1 + 1
      }else{
        num_pts2 = num_pts2 + 1
      }
      
      #idx <- which(tmp$Beta3_SUP==1)
      tmp <- tmp[      (tmp$delta_SUP_CP==1) & 
                      (tmp$shift_sd_record==1)  &
                        (((tmp$Beta1_SAP==1) & (tmp$Beta2_CP==1) & (tmp$Beta3_SUP==5))),]      
      if(i==1 & j == 1){
        par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
        par(cex.lab=1.2)
        plot(tmp$displace_record-0.03, tmp$RMSE_Restricted_DT_test - tmp$RMSE_Proposed_DT_test,
             pch = '.',
             col = rgb(0,0,0,0.4),#rgb(1,0,0,0.4),
             cex=3,
             ylim = c(-2, 0.7),
             xlim = c(-0.1, 2.1),
             ylab=y_axis_label,#'',#'(Restricted-RMSE) - (Proposed-RMSE)',
             xlab = expression(eta), main = TeX("mean displacement ($\\eta$)"))
        #mtext(y_axis_label, side =2, outer = TRUE , cex = 1.5)
    }
  else{
    points(tmp$displace_record-0.03, tmp$RMSE_Restricted_DT_test - tmp$RMSE_Proposed_DT_test,
           pch = '.',
           cex=3,
           col = rgb(0,0,0,0.4)) #rgb(1,0,0,0.4))
  }
  points(tmp$displace_record+0.03, tmp$RMSE_Restricted_OLS_test - tmp$RMSE_Proposed_OLS_test,
         pch =  '-',
         cex=cex_size,
         col = rgb(0,0,0,0.1))
  
  }
}
abline(h=0,lty=5)

num_pts1
text(1.25, 0.62, '.  Random Forest', col='Black', adj = c(0,0))
text(1.25, 0.47, '- Linear Regression', col='Black', adj = c(0,0))


file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2a_*',file.list)]
library(plyr)
df.list <-lapply(file.list, read_excel_allsheets)
par(mar=c(3,1,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
par(cex.lab=1.2)


count = 0 
for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==1)
    count = count + 1
    if(i==1 & j == 1){
      plot(tmp$shift_sd_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           xlim =c(0.14, 1.15),
           ylim = c(-2, 0.7),
           cex = 4,
           col=rgb(0,0,0,0.1),
           pch = '.',
           xlab = expression(sigma), main = TeX("mean-variance displacement ($\\sigma$)"),
           ylab='')
    }else{
      points(tmp$shift_sd_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=rgb(0,0,0,0.1),
             pch = '.',
             cex = 4)
    }
    idx <- which(tmp$Beta3_SUP==5)
    points(tmp$shift_sd_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=rgb(0,0,0,0.1),
           cex = 1.1,
           pch = 16)
    
    idx <- which(tmp$Beta3_SUP==10)
    points(tmp$shift_sd_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=rgb(0,0,0,0.1),
           cex = 2,pch = 19)
  }
}
abline(h=0,lty=5)
count
text(1.03, 0.4, expression(beta[3]==1), col='black', cex = 1.1, adj = c(0,0))
text(1.03, 0.14, expression(beta[3]==5), col='black', cex = 1.1, adj = c(0,0))
text(1.03, -0.19, expression(beta[3]==10), col='black', cex = 1.1, adj = c(0,0))
dev.off()






##
## 2. vary delta_cp or delta_c0
##
pdf("./figs/2_vary_delta.pdf", height=4.4,width=8)

pch0 = '.'
pch1 = 16
cex0 = 3
cex1 = 1.2
col0 = rgb(0,0,0,0.8)
col1 = rgb(0,0,0,0.2)

cex_size <- 1.1
cex.main<- 1
layout(matrix(c(1,2,2,2,1,3,4,5,1,6,7,8), 3, 4, byrow = TRUE), widths = c(0.25,1,1,1), heights = c(0.25,1,1,1))
par(mar=c(1,1,1,1),mgp=c(1.5,0.5,0),tcl=0)
plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.5,1.5,y_axis_label, cex=1.5, srt = 90)


par(mar=c(1,3,1,1),mgp=c(1.5,0.5,0),tcl=-0.3)
plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.5,1.5,TeX('Effect of SAP-SUP Correlation ($\\delta_{CP}$) and SUP-Class Imbalance ($\\delta_{0}$)'),
     cex=1.5)


file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2_vary*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
par(cex.lab=cex_size)

num_pts1 = 0
num_pts2 = 0
for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]

  for(j in 1:length(tmp1)){
    print(j)
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==1 & tmp$displace_record==0)
    

    
    if(sum(tmp$delta_SUP_CP==0)>0){
      num_pts1 = num_pts1 + 1
    }else{
      num_pts2 = num_pts2 + 1
    }
    
    if(i==1 & j==1){
      plot(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           xlim =c(-0.1, 2),
           ylim = c(-0.8, 0.8),
           cex = cex0,
           col=col0, pch = pch0,
           xlab = expression(delta[CP]),
           ylab='',
           main = expression(sigma==1))
    }else{
      points(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=col0, pch = pch0,
             cex = cex0)
    }
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==1 & tmp$displace_record==1)
    points(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=col1, pch = pch1,    cex =cex1)
  
  }
}
text(0.1,0.75, expression(eta==0), cex = cex_size)
points(-0.12, 0.75, pch =pch0, cex = cex0)
text(0.1,0.6, expression(eta==1), cex = cex_size)
points(-0.12, 0.6, pch =pch1, cex = cex1+0.2)

abline(h=0,lty=5)

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]

  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.6 & tmp$displace_record==0)
    
  
    if(i==1 & j ==1 ){
      par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
      par(cex.lab=cex_size, cex.main = cex.main)
      plot(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           ylim = c(-0.8, 0.8),
           cex = cex0,
           col=col0, pch = pch0,
           xlab = expression(delta[CP]),
           ylab='',
           main = expression(sigma==0.6))
    }else{
      points(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=col0, pch = pch0,
             cex = cex0)
    }
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.6 & tmp$displace_record==1)
    points(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=col1, pch = pch1,    cex = cex1)
  }
  
}
abline(h=0,lty=5)



for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]

  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.3 & tmp$displace_record==0)
    
    
    if(i==1 & j == 1){
      par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
      par(cex.lab=cex_size, cex.main = cex.main)
      plot(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           ylim = c(-0.8, 0.8),
           cex = cex0,
           col=col0, pch = pch0,
           xlab = expression(delta[CP]),
           ylab='',
           main = expression(sigma==0.3))
    }else{
      points(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=col0, pch = pch0,
             cex = cex0)
    }
    idx <- which(tmp$Beta3_SUP==5 & tmp$shift_sd_record==0.3 & tmp$displace_record==1)
    points(tmp$delta_SUP_CP[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=col1, pch = pch1,    cex = cex1)
  
  }
}
abline(h=0,lty=5)




file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2c_*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)

num_pts1 = 0
for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
  par(cex.lab=cex_size, cex.main = cex.main)
  
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==1 & tmp$delta_SUP_CP==1 & tmp$displace_record==0 & tmp$shift_sd_record==1)
    
    num_pts1 = num_pts1 +1
    if(i==1 & j ==1){
      plot(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           xlim =c(-2,2),
           ylim = c(-2, 0.8),
           cex = cex0,
           col=col0, pch = pch0,
           xlab = expression(delta[0]), main =  expression(beta[3]==1),
           ylab='')
    }else{
      points(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=col0, pch = pch0,
             cex = cex0)
    }
    idx <- which(tmp$Beta3_SUP==1 & tmp$delta_SUP_CP==1 & tmp$displace_record==1 & tmp$shift_sd_record==1)
    points(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=col1, pch = pch1,    cex = cex1)
  }
}
abline(h=0,lty=5)


text(-1.60,-1.5, expression(eta==0), col='black', cex = cex_size)
points(-2.02, -1.5, pch =pch0, cex = cex0)
text(-1.60,-1.8, expression(eta==1), col='black', cex = cex_size)
points(-2.02, -1.8, pch =pch1, cex = cex1+0.2)



num_pts1 = 0

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
  par(cex.lab=cex_size, cex.main = cex.main)
  
  for(j in 1:length(tmp1)){
    num_pts1 = num_pts1 + 1
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==5 & tmp$delta_SUP_CP==1 & tmp$displace_record==0 & tmp$shift_sd_record==1)
    
    if(i==1 & j ==1){
      plot(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           xlim =c(-2,2),
           ylim = c(-2, 0.8), main =  expression(beta[3]==5),
           cex = cex0,
           col=col0, pch = pch0,
           xlab = expression(delta[0]),
           ylab='')
    }else{
      points(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=col0, pch = pch0,
             cex = cex0)
    }
    idx <- which(tmp$Beta3_SUP==5 & tmp$delta_SUP_CP==1 & tmp$displace_record==1 & tmp$shift_sd_record==1)
    points(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=col1, pch = pch1,    cex = cex1)
  }
}
abline(h=0,lty=5)
num_pts1 = 0
num_pts2 = 0
for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  par(mar=c(3,2,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
  par(cex.lab=cex_size, cex.main = cex.main)
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==10 & tmp$delta_SUP_CP==1 & tmp$displace_record==0 & tmp$shift_sd_record==1)
    
    if(sum(tmp$delta_C0==-2)>0){
      num_pts1 = num_pts1 + 1
    }else{
      num_pts2 = num_pts2 + 1
    }
    
    if(i==1 & j ==1 ){
      plot(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           xlim =c(-2,2),
           ylim = c(-2, 0.8), main =  expression(beta[3]==10),
           cex = cex0,
           col=col0, pch = pch0,
           xlab = expression(delta[0]),
           ylab='')
    }else{
      points(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             col=col0, pch = pch0,
             cex = cex0)
    }
    idx <- which(tmp$Beta3_SUP==10 & tmp$delta_SUP_CP==1 & tmp$displace_record==1 & tmp$shift_sd_record==1)
    points(tmp$delta_C0[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
           col=col1, pch = pch1,    cex = cex1)
  }
}
abline(h=0,lty=5)
dev.off()








##
## 3. Higher-Order Interactions
##
file.list <- list.files(pattern='*.xlsx')
file.list <- file.list[grep('a2d_*',file.list)]
df.list <- lapply(file.list, read_excel_allsheets)
pdf("./figs/3_SAP_SUP_interactions.pdf", height=4,width=8)

#'-'
pch0 = '.'
pch1 = 16
pch2 = 19

pch0_lr = '-'
pch1_lr = '-'
pch2_lr = '-'

cex0_lr = 1
cex1_lr = 1.75
cex2_lr = 3.2

cex0 = 3
cex1 = 1.2
cex2 = 1.8

col0 = rgb(0,0,0,0.1)
col1 = rgb(0,0,0,0.1)
col2 = rgb(0,0,0,0.1)

col0_lr = rgb(0,0,0,0.1)
col1_lr = rgb(0,0,0,0.1)
col2_lr = rgb(0,0,0,0.1)


layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths = c(2,2), heights = c(0.2,1))
par(mar=c(1,3,1,1),mgp=c(1.5,0.5,0),tcl=-0.3)
plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.5,1.5,TeX('Effect of Mean-Displacement ($\\eta$) across SAP-SUP Interactions ($\\beta_4 > 0$)'), cex=1.4)

n = 0

for(i in 1:length(df.list)){
  tmp1 <- df.list[[i]]
  for(j in 1:length(tmp1)){
    tmp <- tmp1[[j]]
    idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==0 & tmp$shift_sd_record==1)
    
    n = n+1
    if(i==1 & j ==1){
      par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
      par(cex.lab=1.2)
      
      plot(tmp$displace_record[idx], tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx],
           xlim =c(0,2),
           ylim = c(-4.5, 0.8),
           cex = cex0_lr,
           col='red',
             pch = pch0_lr,
           xlab = expression(eta),
           ylab=y_axis_label,
           main = 'Linear Regression')
    }else{
      points(tmp$displace_record[idx], tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx],
             cex = cex0_lr,
             col=col0_lr, pch = pch0_lr)
    }
    idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==0.5 & tmp$shift_sd_record==1)
    points(tmp$displace_record[idx], tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx],
           cex = cex1_lr,
           col=col1_lr, pch = pch1_lr)
    
    
    idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==1 & tmp$shift_sd_record==1)
    points(tmp$displace_record[idx], tmp$RMSE_Restricted_OLS_test[idx] - tmp$RMSE_Proposed_OLS_test[idx],
           cex = cex2_lr,
           col=col2_lr, pch = pch2_lr)
    
  }
}
abline(h=0,lty=5)
text(0.2,-1, expression(beta[4]==0.0), col='black', cex = cex_size,adj = c(0,0))
text(0.2, -1.5, expression(beta[4]==0.5), col='black', cex = cex_size,adj = c(0,0))
text(0.2, -2, expression(beta[4]==1.0), col='black', cex = cex_size,adj = c(0,0))


n = 0
for(i in 1:length(df.list)){
    tmp1 <- df.list[[i]]
    for(j in 1:length(tmp1)){
      tmp <- tmp1[[j]]
      idx <- which(tmp$Beta3_SUP==5 & tmp$Beta4_SAP_SUP==0 & tmp$shift_sd_record==1)
      
      n = n+1
      if(i==1 & j ==1 ){
        par(mar=c(3,1,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
        par(cex.lab=1.2)
        plot(tmp$displace_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             xlim =c(0, 2),
             ylim = c(-4.5, 0.8),yaxt = 'n',
             cex = cex0,
             col=col0, pch = pch0,
             xlab = expression(eta),
             ylab='',
        main = 'Random Forest')
      }else{
        points(tmp$displace_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
               cex = cex0,
               col=col0, pch = pch0)
      }
      idx <- which(tmp$Beta3_SUP==1 & tmp$Beta4_SAP_SUP==0.5& tmp$shift_sd_record==1)
      points(tmp$displace_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             cex = cex1,
             col=col1, pch = pch1)
      
      
      idx <- which(tmp$Beta3_SUP==1 & tmp$Beta4_SAP_SUP==1& tmp$shift_sd_record==1)
      points(tmp$displace_record[idx], tmp$RMSE_Restricted_DT_test[idx] - tmp$RMSE_Proposed_DT_test[idx],
             cex = cex2,
             col=col2, pch = pch2)
    
    }
}
abline(h=0,lty=5)
