# 6/23/2018

#Plot NY vs. KC results for Predictive Degradation and comparing OLS vs. RF (Restricted/Proposed Approaches)
rm(list=ls())


## King County
kang_df1 <- read.csv('./output/Kang_output_additional_Features_meanSUP_1.csv')
kang_df2 <- read.csv('./output/Kang_output_additional_Features_meanSUP_2.csv')
kang_df3 <- read.csv('./output/Kang_output_additional_Features_meanSUP_3.csv')
kang_df4 <- read.csv('./output/Kang_output_additional_Features_meanSUP_4.csv')
kang_df <- rbind(kang_df1, kang_df2, kang_df3, kang_df4)


## Predictive Degradation
kang_df$restricted_minus_proposed_TEST <- kang_df$Restricted_RF_RMSE_test - kang_df$Proposed_RF_RMSE_test
kang_df$restricted_minus_proposed_TEST_OLS <- kang_df$Restricted_OLS_RMSE_test - kang_df$Proposed_OLS_RMSE_test



#kang_df_explore$restricted_minus_proposed_TEST <- kang_df_explore$Restricted_RF_RMSE_test - kang_df_explore$Proposed_RF_RMSE_test
#kang_df_explore$restricted_minus_proposed_TEST_OLS <- kang_df_explore$Restricted_OLS_RMSE_test - kang_df_explore$Proposed_OLS_RMSE_test



## NYC
# NY still running for 50 iterations
ny_df1 <- read.csv('./NY/NYC_output_all_years_additional_features_meanSUP_1.csv')
ny_df2 <- read.csv('./NY/NYC_output_all_years_additional_features_meanSUP_2.csv')
ny_df3 <- read.csv('./NY/NYC_output_all_years_additional_features_meanSUP_3.csv')
ny_df4 <- read.csv('./NY/NYC_output_all_years_additional_features_meanSUP_4.csv')

ny_df <- rbind(ny_df1, ny_df2, ny_df3, ny_df4)


# 
# # head(ny_df)
# ## this is the code we're running: https://www.dropbox.com/s/9xs7bdi6hirz2r1/a_NY_2016_BORO_python_script.py?dl=0
ny_df$restricted_minus_proposed_TEST <- ny_df$Restricted_RF_RMSE_test - ny_df$Proposed_RF_RMSE_test
ny_df$restricted_minus_proposed_TEST_OLS <- ny_df$Restricted_OLS_RMSE_test - ny_df$Proposed_OLS_RMSE_test


mean(ny_df$restricted_minus_proposed_TEST < 0)
#[1] 0.35

mean(kang_df$restricted_minus_proposed_TEST < 0)
#[1] 0.77





## why does KC have more mass >> 0? now that we have 50 iterations?
# par(c(2,1))
# plot(seq(1,length(kang_df$Proposed_RF_RMSE_test)), 
#      kang_df$Proposed_RF_RMSE_test, col='red', xlab = 'train/test iteration #',
#      ylab = 'RMSE', main = 'Proposed', pch = 16)
# points(seq(1,length(kang_df$Proposed_RF_RMSE_test)), 
#        kang_df$Proposed_RF_RMSE_train, col = 'black', pch = 16)
# 
# 
# plot(seq(1,length(kang_df$Restricted_RF_RMSE_test)), 
#      kang_df$Restricted_RF_RMSE_test, col='red', xlab = 'train/test iteration #',
#      ylab = 'RMSE', main = 'Restricted', pch = 16)
# points(seq(1,length(kang_df$Restricted_RF_RMSE_test)), 
#      kang_df$Restricted_RF_RMSE_train, col = 'black', pch = 16)
# ###



# pdf("Kang_NY_RF_vs_OLS.pdf", height=6,width=5)
# par(mfrow=c(2,1),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
# 
# 
# plot(kang_df$restricted_minus_proposed_TEST_OLS,
#      kang_df$restricted_minus_proposed_TEST,
#      pch = 16,
#      col=rgb(0,0,0,0.4), xlab='OLS',
#      ylab='RF',
#      xlim = c(-0.08,0.05),
#      ylim = c(-0.08,0.05),
#      main = 'King County \n (restricted-RMSE) - (proposed-RMSE)')
# abline(a=0, b=1,  lty=5)
# 
# 
# plot(ny_df$restricted_minus_proposed_TEST_OLS,
#      ny_df$restricted_minus_proposed_TEST,
#      pch = 16,
#      col=rgb(0,0,0,0.4), xlab='OLS',
#      ylab='RF',
#      xlim = c(-0.015,0.03),
#      ylim = c(-0.015,0.03),
#      main = 'NY \n (restricted-RMSE) - (proposed-RMSE)')
# abline(a=0, b=1,  lty=5)
# dev.off()


mean(kang_df$restricted_minus_proposed_TEST)
mean(ny_df$restricted_minus_proposed_TEST)



# pdf("Kang_NY_Proposed_v_restricted_with_all_Yelp_as_CP.pdf", height=5,width=5)
# par(mfrow=c(2,1),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
# #layout(matrix(c(1,2), 2,1, byrow = TRUE), widths = c(1), heights = c(1,1))
# 
# #par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.1)
# 
# hist(kang_df$restricted_minus_proposed_TEST, #pch = 16, 
#      #xlim = c(1.804,1.835), 
#      col = rgb(1,0,0,0.5),
#      breaks=seq(-0.08, 0.13, length.out=30),
#      #cex=1.5,
#      xaxt='n',
#      xlim = c(-0.08, 0.13), 
#      xlab = '', 
#      #ylab = '(restricted-RMSE - proposed-RMSE)',
#      freq = F, border = 'white',
#      main = 'King County')
# abline(v=0,  lty=5)
# axis(1, at=seq(-0.08, 0.13, by = 0.02),  lwd.ticks=1)
# #axis(1, at=seq(0 , 2000, by=200), lwd=0, lwd.ticks=1)
# 
# 
# hist(ny_df$restricted_minus_proposed_TEST, #pch = 16, 
#      #xlim = c(1.804,1.835), 
#      col =rgb(0,0,1, 0.5),
#      breaks=seq(-0.03, 0.03, length.out=15),
#      #cex=1.5,
#      xlim = c(-0.08, 0.13), 
#      xaxt = 'n',
#      # xlab = '',  
#      xlab = 'Proposed Predictive Degradation',
#      freq = F, border = 'white', add =F,
#      main =  'NYC')
# axis(1, at=seq(-0.08, 0.13, by = 0.02),  lwd.ticks=1)
# abline(v=0,  lty=5)
# dev.off()





pdf("Kang_NY_Proposed_v_restricted.pdf", height=2.5,width=6)
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
     main =  'NYC')
axis(1, at=seq(-0.09, 0.08, by = 0.02),  lwd.ticks=1)
abline(v=0,  lty=5)


plot(0,0, bty = 'n', xaxt='n', yaxt='n', ylim = c(1,2), xlim = c(1,2), ylab = '', xlab = '')
text(1.5,1.5,'Predictive Degradation', cex=1.1)


dev.off()













pdf("Kang_NY_Proposed_v_restricted_previous.pdf", height=4.4,width=5)
#par(mfrow=c(2,1),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths = c(1), heights = c(1,1.25))
par(mar=c(1.5,3,1,1),mgp=c(1.5,0.5,0),tcl=-0.3)
hist(kang_df$restricted_minus_proposed_TEST, #pch = 16, 
     #xlim = c(1.804,1.835), 
     col = rgb(1,0,0,0.5),
    breaks=seq(-0.08, 0.08, length.out=30),
     #cex=1.5,
     xaxt='n',
     xlim = c(-0.08, 0.08), 
     xlab = '', 
    #ylab = '(restricted-RMSE - proposed-RMSE)',
     freq = F, border = 'white',
     main = 'King County')
abline(v=0,  lty=5)
axis(1, at=seq(-0.08, 0.08, by = 0.02),  lwd.ticks=1)
#axis(1, at=seq(0 , 2000, by=200), lwd=0, lwd.ticks=1)

par(mar=c(3,3,1.5,1),mgp=c(1.5,0.5,0),tcl=-0.3)
hist(ny_df$restricted_minus_proposed_TEST, #pch = 16, 
      #xlim = c(1.804,1.835), 
      col =rgb(0,0,1, 0.5),
     breaks=seq(-0.03, 0.03, length.out=15),
     #cex=1.5,
     xlim = c(-0.08, 0.08), 
     xaxt = 'n',
     # xlab = '',  
      xlab = 'Proposed Predictive Degradation',
      freq = F, border = 'white', add =F,
      main =  'NYC')
axis(1, at=seq(-0.08, 0.08, by = 0.02),  lwd.ticks=1)
abline(v=0,  lty=5)
dev.off()





pdf("Kang_NY_RF_v_OLS.pdf", height=5,width=5)
cex_pt <- 0.5
par(mfrow=c(2,2),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

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

## NYC
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

plot(ny_df$Proposed_OLS_RMSE_test ,
     ny_df$Proposed_RF_RMSE_test ,
     xlab = 'Linear',
     ylab = 'RF',
     main = 'Proposed (NY)',
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
     main = 'Restricted (NY)',
     cex = cex_pt,
     pch =  20,
     col=rgb(0,0,0,0.4),
     xlim = c(10.8,12.2),
     ylim = c(10.8,12.2))
abline(a=0,b=1)
dev.off()



#text( -0.05,4, '(restricted-RMSE - proposed-RMSE)')

#points(ny_df$X,
#       cex=1.5,
#       ny_df$restricted_minus_proposed_TEST, pch = 16,
#       col =rgb(1,0,0, 0.5))
#abline(h=0, lty=5)
#axis(1, at=c(1.81, 1.83), labels=x)
#text(1.805, 0.1, 'Train',col = 'red')#rgb(0,0,0,0.8))
#text(1.805, 0.08, 'Test',col ='black')#rgb(1,0,0, 0.4))



#pdf("Kang_NY_Proposed_v_restricted.pdf", height=4,width=5)
#par(mfrow=c(1,1),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)

#plot(kang_df$X, 
#     kang_df$restricted_minus_proposed_TEST, pch = 16, 
#     xlim = c(1.804,1.835), col = rgb(1,0,0,0.5),
#     cex=1.5,
#     ylim = c(-0.15, 0.15), xlab = '', xaxt='n', ylab = '(restricted-RMSE - proposed-RMSE)')


#points(ny_df$X,
#       cex=1.5,
#       ny_df$restricted_minus_proposed_TEST, pch = 16,
#       col =rgb(1,0,0, 0.5))
#abline(h=0, lty=5)
#axis(1, at=c(1.81, 1.83), labels=x)
#text(1.805, 0.1, 'Train',col = 'red')#rgb(0,0,0,0.8))
#text(1.805, 0.08, 'Test',col ='black')#rgb(1,0,0, 0.4))
#dev.off()



# Old
# pdf("Kang_NY_Proposed_v_restricted.pdf", height=4,width=5)
# par(mfrow=c(1,1),mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
# 
# plot(kang_df$X, 
#      kang_df$restricted_minus_proposed_TRAIN, pch = 16, 
#      xlim = c(1.8,1.84), col = rgb(0,0,0,0.8),
#      cex=1.2,
#      ylim = c(-0.15, 0.15), xlab = '', xaxt='n', ylab = '(restricted-RMSE - proposed-RMSE)')
# plot(kang_df$X+0.0011,
#        cex=1.2,
#        
#        kang_df$restricted_minus_proposed_TEST, pch = 16, 
#        col =rgb(1,0,0, 0.4))
# points(ny_df$X,
#        cex=1.2,
#        
#        ny_df$restricted_minus_proposed_TRAIN, pch = 16, col = rgb(0,0,0,0.8))
# points(ny_df$X+0.0011,
#        cex=1.2,
#        
#        ny_df$restricted_minus_proposed_TEST, pch = 16,
#        col =rgb(1,0,0, 0.4))
# abline(h=0, lty=5)
# axis(1, at=c(1.81, 1.83), labels=x)
# text(1.805, 0.1, 'Train',col = 'red')#rgb(0,0,0,0.8))
# text(1.805, 0.08, 'Test',col ='black')#rgb(1,0,0, 0.4))
# dev.off()
