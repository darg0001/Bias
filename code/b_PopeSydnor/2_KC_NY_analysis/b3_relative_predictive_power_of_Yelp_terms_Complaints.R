rm(list = ls())

## compute relative % decrease in RMSE when adding CP to model for King County
kang_df_rmse <- read.csv('./output/Kang_output_record_model_with_and_without_keywords.csv')

# for RF
mean(( kang_df_rmse$Full_RF_RMSE_test_without_CP - kang_df_rmse$Full_RF_RMSE_test )/ kang_df_rmse$Full_RF_RMSE_test)
#[1] -0.0003055639
mean(( kang_df_rmse$Full_RF_RMSE_train_without_CP - kang_df_rmse$Full_RF_RMSE_train )/ kang_df_rmse$Full_RF_RMSE_train)
#[1] 0.001294404



## compute relative % decrease in RMSE when adding CP to model for New York
ny_df_rmse <- read.csv('./output/NYC_output_with_without_complaints.csv')

# for RF
mean(( ny_df_rmse$Full_RF_RMSE_test_without_CP - ny_df_rmse$Full_RF_RMSE_test )/ ny_df_rmse$Full_RF_RMSE_test)
#[1] 0.0001899566
mean(( ny_df_rmse$Full_RF_RMSE_train_without_CP - ny_df_rmse$Full_RF_RMSE_train )/ ny_df_rmse$Full_RF_RMSE_train)
#[1] 0.001642343
