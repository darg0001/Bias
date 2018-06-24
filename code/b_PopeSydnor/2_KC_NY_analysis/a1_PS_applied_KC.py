from __future__ import division
from matplotlib.backends.backend_pdf import PdfPages
import os
from sklearn.model_selection import KFold
import re, ast
import platform
import sklearn.cross_validation
import matplotlib.pyplot as plt

execfile('../../functions/python_libraries.py')
execfile('../../functions/simulation_functions.py')

## This dataframe is instances_mergerd_seattle.csv from Kang et al. 2013 with additional
## features created in KC.R.
dta = pd.read_csv('../../../data/KangData_with_additional_features.csv')
#dta = pd.read_csv('/Users/kristen/Desktop/KangData_with_additional_features.csv')


## set -1 scores to 0
dta.inspection_penalty_score[dta.inspection_penalty_score==-1]=0
dta.inspection_average_prev_penalty_scores[dta.inspection_average_prev_penalty_scores==-1]=0
dta.inspection_prev_penalty_score[dta.inspection_prev_penalty_score==-1]=0


features = ['inspection_average_prev_penalty_scores', 'inspection_prev_penalty_score', # SAP
            'inspection_penalty_score', # y
            'asian', # SUP
            'poison', 'vomit','diarrhea', 'sick2', # CP
            'review_count', 'average_review_rating', 'non_positive_review_count'] # CP


## subset to these features
kc_df_subset = dta[features]


## Rename Columns
kc_df_subset.columns = ['inspection_average_prev_penalty_scores_SAP', 'inspection_prev_penalty_score_SAP',
                        'y',
                        'asian_SUP',
                        'poison_CP', 'vomit_CP','diarrhea_CP', 'sick_CP',
                        'review_count_SAP', 'average_review_rating_SAP', 'non_positive_review_count_SAP']


## Get SAP and CP columns
SAP_cols = [s for s in np.array(kc_df_subset.columns) if "SAP" in s]
CP_cols = [s for s in np.array(kc_df_subset.columns) if "CP" in s]



k_fold = sklearn.cross_validation.ShuffleSplit(len(kc_df_subset.y), n_iter=1,#200,
                                                         test_size=0.2)

## RF Results
full_rmse = []
full_train_rmse = []

proposed_rmse = []
proposed_train_rmse = []

common_model = []
common_rmse=[]
common_train_rmse = []

restricted_rmse = []
restricted_train_rmse = []


## OLS Results
full_rmse_OLS = []
full_train_rmse_OLS = []

proposed_rmse_OLS = []
proposed_train_rmse_OLS = []

restricted_rmse_OLS = []
restricted_train_rmse_OLS = []



for k, (train, test) in enumerate(k_fold):
    print( 'iteration #', k)
    df_subset_train = kc_df_subset.iloc[train,:]
    df_subset_test = kc_df_subset.iloc[test,:]
    
    
    ## Hyperparamter Search set-up for RF
    clf = sklearn.ensemble.RandomForestRegressor()
    max_depth = [3, 5, 10]
    max_depth.append(None)
    min_samples_leaf = [5, 10, 20, 50, 100]
    min_samples_split = [2, 3, 4, 5, 10]
    n_estimators = [50, 100, 150]
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

    
    ##
    ## Train/Proposed-RMSE
    ##
    p = np.mean(df_subset_train.asian_SUP==1)
    #print p
    n_SUP1 = np.int(np.round(p * 1000))
    n_SUP0 = 1000 - n_SUP1

    df_subset_test_proposed_tmp = df_subset_train.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 0
    yhat0= n_SUP0 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])

    df_subset_test_proposed_tmp = df_subset_train.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 1
    yhat1= n_SUP1 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])
    y_predict = (yhat0 + yhat1)/(n_SUP0 + n_SUP1)
    proposed_train_rmse.append(sqrt(mean_squared_error(df_subset_train.y, y_predict)))
    
    
    ##
    ## Test/Proposed-RMSE
    ##
    p = np.mean(df_subset_test.asian_SUP==1)
    n_SUP1 = np.int(np.round(p * 1000))
    n_SUP0 = 1000 - n_SUP1

    df_subset_test_proposed_tmp = df_subset_test.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 0
    yhat0= n_SUP0 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])

    df_subset_test_proposed_tmp = df_subset_test.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 1
    yhat1= n_SUP1 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols, CP_cols, ['asian_SUP']], [])])
    y_predict_proposed = (yhat0 + yhat1)/(n_SUP0 + n_SUP1)
    proposed_rmse.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_proposed)))
    

    
    ## Restricted
    model.fit(df_subset_train[SAP_cols],
                        df_subset_train.y)
    y_predict_restricted = model.predict(df_subset_test[SAP_cols])
    y_predict_train_restricted= model.predict(df_subset_train[SAP_cols])
    restricted_rmse.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_restricted)))
    restricted_train_rmse.append(sqrt(mean_squared_error(df_subset_train.y, y_predict_train_restricted)))


    ##
    ## OLS Portion
    ##
    model = sklearn.linear_model.LinearRegression()
    
    ## Full
    (MSE_full_test, predict_full_test, b0_full_test, coef_full_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                method="Full",
                                                                                                SAP_features=SAP_cols,
                                                                                                SUP_features=['asian_SUP'],
                                                                                                CP_features=CP_cols,
                                                                                                outcome=['y'],
                                                                                                data_train = df_subset_train,
                                                                                                data_test = df_subset_test)
    
    full_rmse_OLS.append(MSE_full_test)
    
    (MSE_full_train, predict_full_train, b0_full_train, coef_full_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                        method="Full",
                                                                                                        SAP_features=SAP_cols,
                                                                                                        SUP_features=['asian_SUP'],
                                                                                                        CP_features=CP_cols,
                                                                                                        outcome=['y'],
                                                                                                        data_train = df_subset_train,
                                                                                                        data_test = df_subset_train)
    full_train_rmse_OLS.append(MSE_full_train)




    
    ##
    ## Train/Proposed-RMSE
    ##
    (MSE_prop_train, predict_prop_train, b0_prop_train, coef_prop_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                        method="Proposed",
                                                                                                        SAP_features=SAP_cols,
                                                                                                        SUP_features=['asian_SUP'],
                                                                                                        CP_features=CP_cols,
                                                                                                        outcome=['y'],
                                                                                                        data_train = df_subset_train,
                                                                                                        data_test = df_subset_train)
    proposed_train_rmse_OLS.append(MSE_prop_train)
    
    
    
    
    ##
    ## Test/Proposed-RMSE
    ##
    (MSE_prop_test, predict_prop_test, b0_prop_test, coef_prop_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                    method="Proposed",
                                                                                                    SAP_features=SAP_cols,
                                                                                                    SUP_features=['asian_SUP'],
                                                                                                    CP_features=CP_cols,
                                                                                                    outcome=['y'],
                                                                                                    data_train = df_subset_train,
                                                                                                    data_test = df_subset_test)
    proposed_rmse_OLS.append(MSE_prop_test)
    
    
    
    
    
 
    ## Restricted
    model.fit(df_subset_train[SAP_cols],
                        df_subset_train.y)
    y_predict_restricted = model.predict(df_subset_test[SAP_cols])
    y_predict_train_restricted= model.predict(df_subset_train[SAP_cols])
    restricted_rmse_OLS.append(sqrt(mean_squared_error(df_subset_test.y, y_predict_restricted)))
    restricted_train_rmse_OLS.append(sqrt(mean_squared_error(df_subset_train.y, y_predict_train_restricted)))



results = pd.DataFrame({'Full_RF_RMSE_test': full_rmse,
                       'Full_RF_RMSE_train': full_train_rmse,
                       'Full_OLS_RMSE_test': full_rmse_OLS,
                       'Full_OLS_RMSE_train': full_train_rmse_OLS,
                       'Proposed_RF_RMSE_test': proposed_rmse,
                       'Proposed_RF_RMSE_train': proposed_train_rmse,
                       'Proposed_OLS_RMSE_test': proposed_rmse_OLS,
                       'Proposed_OLS_RMSE_train': proposed_train_rmse_OLS,
                       'Restricted_RF_RMSE_test': restricted_rmse,
                       'Restricted_RF_RMSE_train': restricted_train_rmse,
                       'Restricted_OLS_RMSE_test': restricted_rmse_OLS,
                       'Restricted_OLS_RMSE_train': restricted_train_rmse_OLS})

results.to_csv('./output/Kang_output_additional_Features_meanSUP.csv')

