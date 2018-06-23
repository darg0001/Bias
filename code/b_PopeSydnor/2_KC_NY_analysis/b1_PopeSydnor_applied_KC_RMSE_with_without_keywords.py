from __future__ import division
from matplotlib.backends.backend_pdf import PdfPages
import os
from sklearn.model_selection import KFold
import re, ast
import platform
import sklearn.cross_validation
import matplotlib.pyplot as plt

execfile('../functions/python_libraries.py')
execfile('../functions/simulation_functions.py')


## This dataframe is instances_mergerd_seattle.csv from Kang et al. 2013 with additional
## features created in KC.R.
dta = pd.read_csv('../../../data/KangData_with_additional_features.csv')

## set -1 scores to 0
dta.inspection_penalty_score[dta.inspection_penalty_score==-1]=0
dta.inspection_average_prev_penalty_scores[dta.inspection_average_prev_penalty_scores==-1]=0
dta.inspection_prev_penalty_score[dta.inspection_prev_penalty_score==-1]=0


features = ['inspection_average_prev_penalty_scores', 'inspection_prev_penalty_score', # SAP
            'inspection_penalty_score', # y
            'asian', # SUP
            'poison', 'vomit','diarrhea', 'sick2', # CP
            'review_count', 'average_review_rating', 'non_positive_review_count'] # CP




ny_df_subset = dta[features]


## Rename Columns
ny_df_subset.columns = ['inspection_average_prev_penalty_scores_SAP', 'inspection_prev_penalty_score_SAP',
                        'y',
                        'asian_SUP',
                        'poison_CP', 'vomit_CP','diarrhea_CP', 'sick_CP',
                        'review_count_SAP', 'average_review_rating_SAP', 'non_positive_review_count_SAP']


## Get SAP and CP columns
SAP_cols = [s for s in np.array(ny_df_subset.columns) if "SAP" in s]
CP_cols = [s for s in np.array(ny_df_subset.columns) if "CP" in s]



k_fold = sklearn.cross_validation.ShuffleSplit(len(ny_df_subset.y), n_iter=50,
                                                         test_size=0.2)

## DT Results
full_model = []
full_rmse = []
full_train_rmse = []
full_model_without_CP = []
full_rmse_without_CP = []
full_train_rmse_without_CP = []




## OLS Results
full_rmse_OLS = []
full_train_rmse_OLS = []
full_rmse_OLS_without_CP = []
full_train_rmse_OLS_without_CP = []




for k, (train, test) in enumerate(k_fold):
    print( 'iteration #', k)
    df_subset_train = ny_df_subset.iloc[train,:]
    df_subset_test = ny_df_subset.iloc[test,:]
    
    
    ## Hyperparamter Search set-up for RF
    clf = sklearn.ensemble.RandomForestRegressor()
    max_depth = [3, 5, 10]
    max_depth.append(None)
    min_samples_leaf = [5, 10, 20, 50, 100]
    min_samples_split = [2, 3, 4, 5, 10]
    n_estimators = [50, 100, 150] #[10, 50, 100, 150, 200]
    max_features = ['auto', 0.25, 0.5, 0.75]
    random_grid = {'max_depth': max_depth,
                'min_samples_leaf': min_samples_leaf,
                  'max_features': max_features,
                  'n_estimators': n_estimators,
                  'min_samples_split': min_samples_split}

    model = GridSearchCV(estimator = clf,
                    param_grid = random_grid,
                    cv = 3, verbose=0,
                    n_jobs = -1)


    
    ## Full
    model.fit(df_subset_train[sum([SAP_cols, CP_cols, ['asian_SUP']], [])], 
                    df_subset_train.y)
    y_predict_full = model.predict(df_subset_test[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])
    full_rmse.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_full)))
    
    y_predict_train = model.predict(df_subset_train[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])
    full_train_rmse.append(sqrt(mean_squared_error(df_subset_train.y, y_predict_train)))
    full_model.append(np.array(model.best_params_))


    ## Full -- without CP
    model.fit(df_subset_train[sum([SAP_cols, ['asian_SUP']], [])],
                    df_subset_train.y)
    y_predict_full = model.predict(df_subset_test[sum([SAP_cols, ['asian_SUP']], [])])
    full_rmse_without_CP.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_full)))

    y_predict_train = model.predict(df_subset_train[sum([SAP_cols, ['asian_SUP']], [])])
    full_train_rmse_without_CP.append(sqrt(mean_squared_error(df_subset_train.y, y_predict_train)))
    full_model.append(np.array(model.best_params_))





    ##
    ## OLS Portion
    ##
    model = sklearn.linear_model.LinearRegression()
    
    ## Full
    model.fit(df_subset_train[sum([SAP_cols, CP_cols, ['asian_SUP']], [])], 
                    df_subset_train.y)
    #print model.best_params_
    y_predict_full = model.predict(df_subset_test[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])
    full_rmse_OLS.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_full)))
    
    y_predict_train = model.predict(df_subset_train[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])
    full_train_rmse_OLS.append(sqrt(mean_squared_error(df_subset_train.y, y_predict_train)))


    ## Full -- without CP
    model.fit(df_subset_train[sum([SAP_cols, ['asian_SUP']], [])],
                    df_subset_train.y)
    y_predict_full = model.predict(df_subset_test[sum([SAP_cols, ['asian_SUP']], [])])
    full_rmse_OLS_without_CP.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_full)))

    y_predict_train = model.predict(df_subset_train[sum([SAP_cols, ['asian_SUP']], [])])
    full_train_rmse_OLS_without_CP.append(sqrt(mean_squared_error(df_subset_train.y, y_predict_train)))




results = pd.DataFrame({'Full_RF_RMSE_test': full_rmse,
                       'Full_RF_RMSE_train': full_train_rmse,
                       'Full_OLS_RMSE_test': full_rmse_OLS,
                       'Full_OLS_RMSE_train': full_train_rmse_OLS,
                       'Full_RF_RMSE_test_without_CP': full_rmse_without_CP,
                       'Full_RF_RMSE_train_without_CP': full_train_rmse_without_CP,
                       'Full_OLS_RMSE_test_without_CP': full_rmse_OLS_without_CP,
                       'Full_OLS_RMSE_train_without_CP': full_train_rmse_OLS_without_CP})

results.to_csv('./output/Kang_output_record_model_with_and_without_keywords.csv')

