# 6/23/2018

#Plot NY vs. KC results for Predictive Degradation and comparing OLS vs. RF (Restricted/Proposed Approaches)
rm(list=ls())

#setwd('~/Dropbox/YelpBias/kristen_sandbox/final_paper_code/JITE_git_upload/Bias/code/b_PopeSydnor/2_KC_NY_analysis/')

## King County
kang_df1 <- read.csv('./output/Kang_output_additional_Features_meanSUP_1.csv')
kang_df2 <- read.csv('./output/Kang_output_additional_Features_meanSUP_2.csv')
kang_df3 <- read.csv('./output/Kang_output_additional_Features_meanSUP_3.csv')
kang_df4 <- read.csv('./output/Kang_output_additional_Features_meanSUP_4.csv')
kang_df <- rbind(kang_df1, kang_df2, kang_df3, kang_df4)

## Predictive Degradation
kang_df$restricted_minus_proposed_TEST <- kang_df$Restricted_RF_RMSE_test - kang_df$Proposed_RF_RMSE_test
kang_df$restricted_minus_proposed_TEST_OLS <- kang_df$Restricted_OLS_RMSE_test - kang_df$Proposed_OLS_RMSE_test

mean(kang_df$restricted_minus_proposed_TEST)
#1] -0.0165316

## New York
ny_df1 <- read.csv('./output/NYC_output_all_years_additional_features_meanSUP_1.csv')
ny_df2 <- read.csv('./output/NYC_output_all_years_additional_features_meanSUP_2.csv')
ny_df3 <- read.csv('./output/NYC_output_all_years_additional_features_meanSUP_3.csv')
ny_df4 <- read.csv('./output/NYC_output_all_years_additional_features_meanSUP_4.csv')
ny_df <- rbind(ny_df1, ny_df2, ny_df3, ny_df4)

## Predictive Degradation
ny_df$restricted_minus_proposed_TEST <- ny_df$Restricted_RF_RMSE_test - ny_df$Proposed_RF_RMSE_test
ny_df$restricted_minus_proposed_TEST_OLS <- ny_df$Restricted_OLS_RMSE_test - ny_df$Proposed_OLS_RMSE_test

mean(ny_df$restricted_minus_proposed_TEST)
#[1] 0.0029749


pdf("./figs/Kang_NY_Proposed_v_restricted.pdf", height=2.5,width=6)
layout(matrix(c(2,1,3,3), 2, 2, byrow = TRUE), widths = c(2,2), heights = c(1,0.25))

par(mar=c(1.2,3,1,1),mgp=c(1.5,0.5,0),tcl=-0.3)
hist(kang_df$restricted_minus_proposed_TEST, #pch = 16, 
     #xlim = c(1.804,1.835), 
     col = rgb(1,0,0,0.5),
     breaks=seq(-0.088, 0.08, length.out=30),
     #cex=1.5,
     xaxt='n',
     xlim = c(-0.09, 0.08), 
     xlab = '', 
     #ylab = '(restricted-RMSE - proposed-RMSE)',
     freq = F, border = 'white',
     main = 'King County')
abline(v=0,  lty=5)
axis(1, at=seq(-0.09, 0.08, by = 0.02),  lwd.ticks=1)
#axis(1, at=seq(0 , 2000, by=200), lwd=0, lwd.ticks=1)

#par(mar=c(3,3,1.5,1),mgp=c(1.5,0.5,0),tcl=-0.3)
par(mar=c(1.2,3,1,1),mgp=c(1.5,0.5,0),tcl=-0.3)
hist(ny_df$restricted_minus_proposed_TEST, #pch = 16, 
     #xlim = c(1.804,1.835), 
     col =rgb(0,0,1, 0.5),
     breaks=seq(-0.03, 0.03, length.out=15),
     #cex=1.5,
     xlim = c(-0.09, 0.08), 
     xaxt = 'n',
     # xlab = '',  
     xlab = '',##'Proposed Predictive Degradation',
     freq = F, border = 'white', add =F,
     main =  'New York')
axis(1, at=seq(-0.09, 0.08, by = 0.02),  lwd.ticks=1)
abline(v=0,  lty=5)
plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.5,1.5,'Predictive Degradation', cex=1.1)
dev.off()





pdf("./figs/Kang_NY_RF_v_OLS.pdf", height=5,width=5)
cex_pt <- 0.5
par(mfrow=c(2,2),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)


## NYC
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

plot(ny_df$Proposed_OLS_RMSE_test ,
     ny_df$Proposed_RF_RMSE_test ,
     xlab = 'Linear',
     ylab = 'RF',
     main = 'Proposed (New York)',
     pch =  20,
     cex = cex_pt,
     col=rgb(0,0,0,0.4),
     xlim = c(10.8,12.2),
     ylim = c(10.8,12.2))
abline(a=0,b=1)
par(mar=c(3,2.5,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

plot(ny_df$Restricted_OLS_RMSE_test ,
     ny_df$Restricted_RF_RMSE_test ,
     xlab = 'Linear',
     ylab = 'RF',
     main = 'Restricted (New York)',
     cex = cex_pt,
     pch =  20,
     col=rgb(0,0,0,0.4),
     xlim = c(10.8,12.2),
     ylim = c(10.8,12.2))
abline(a=0,b=1)


plot(kang_df$Proposed_OLS_RMSE_test ,
     kang_df$Proposed_RF_RMSE_test ,
     xlab = 'Linear',
     ylab = 'RF',
     main = 'Proposed (King County)',
     pch = 20, cex = cex_pt,
     col=rgb(0,0,0,0.4),
     xlim = c(13.25,15.5),
     ylim = c(13.25,15.5))
abline(a=0,b=1)


par(mar=c(3,2.5,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

plot(kang_df$Restricted_OLS_RMSE_test ,
     kang_df$Restricted_RF_RMSE_test ,
     xlab = 'Linear',
     ylab = 'RF',
     cex = cex_pt,
     main = 'Restricted (King County)',
     pch =  20,
     col=rgb(0,0,0,0.4),
     xlim = c(13.25,15.5),
     ylim = c(13.25,15.5))
abline(a=0,b=1)

dev.off()


