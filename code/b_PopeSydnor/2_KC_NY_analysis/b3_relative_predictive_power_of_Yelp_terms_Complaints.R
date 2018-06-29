rm(list = ls())

## compute relative % decrease in RMSE when adding CP to model for King County
## compute: without_CP - with_CP / without_CP * 100
kang_df_rmse <- read.csv('./output/Kang_output_record_model_with_and_without_keywords.csv')

# for RF
mean(( kang_df_rmse$Full_RF_RMSE_test_without_CP - kang_df_rmse$Full_RF_RMSE_test )/ kang_df_rmse$Full_RF_RMSE_test_without_CP) * 100
#[1] 0.003301463
mean(( kang_df_rmse$Full_RF_RMSE_train_without_CP - kang_df_rmse$Full_RF_RMSE_train )/ kang_df_rmse$Full_RF_RMSE_train_without_CP) * 100
#[1] -0.04015178



## compute relative % decrease in RMSE when adding CP to model for New York
ny_df_rmse <- read.csv('./output/NYC_output_with_without_complaints.csv')

# for RF
mean(( ny_df_rmse$Full_RF_RMSE_test_without_CP - ny_df_rmse$Full_RF_RMSE_test )/ ny_df_rmse$Full_RF_RMSE_test_without_CP) * 100
#[1] 0.01819071
mean(( ny_df_rmse$Full_RF_RMSE_train_without_CP - ny_df_rmse$Full_RF_RMSE_train )/ ny_df_rmse$Full_RF_RMSE_train_without_CP) * 100
#[1] 0.2212024

