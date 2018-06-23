from __future__ import division
from IPython.display import Image
import matplotlib.pyplot as plt
import sklearn.cross_validation

execfile('../../functions/python_libraries.py')
execfile('../../functions/simulation_functions.py')

ny_df = pd.read_csv('../../../data/NYData.csv')



ny_df_subset = ny_df[['BORO', 'SCORE', 'complaint','Asian','subsequentScore', 'avg.score']]


ny_df_subset.reset_index(inplace=True, drop=True)
ny_df_subset.columns = ['BORO_SAP', 'score_SAP', 'complaint_CP',
                        'asian_SUP','y','avg_score_SAP']

ny_df_subset.BORO_SAP = map(str,ny_df_subset.BORO_SAP)
ny_df_subset = ny_df_subset.loc[~ny_df_subset.y.isnull()]
ny_df_subset = ny_df_subset.loc[ny_df_subset.y != -1]

## note we analyze BORO_SAP.
zip_SAP = pd.get_dummies(ny_df_subset.BORO_SAP)

## subset to only 4-columns -- (i.e. K-1 for K categories)
zip_SAP.columns = np.array(zip_SAP.columns) + '_SAP'
print np.shape(zip_SAP)
ny_df_subset_RF = pd.concat((ny_df_subset,zip_SAP), axis = 1)
SAP_cols_RF = [s for s in np.array(ny_df_subset_RF.columns) if "SAP" in s]
CP_cols_RF = ['complaint_CP']

zip_SAP = pd.get_dummies(ny_df_subset.BORO_SAP)
zip_SAP = zip_SAP[np.array(zip_SAP.columns)[0:len(np.array(zip_SAP.columns))-1]]
zip_SAP.columns = np.array(zip_SAP.columns) + '_SAP'
ny_df_subset_OLS = pd.concat((ny_df_subset,zip_SAP), axis = 1)
SAP_cols_OLS = [s for s in np.array(ny_df_subset_OLS.columns) if "SAP" in s]
CP_cols_OLS= ['complaint_CP']


ny_df_subset_RF.drop(['BORO_SAP'], inplace = True, axis = 1)
ny_df_subset_OLS.drop(['BORO_SAP'], inplace = True, axis = 1)



SAP_cols_OLS = [s for s in np.array(ny_df_subset_OLS.columns) if "SAP" in s]
SAP_cols_RF = [s for s in np.array(ny_df_subset_RF.columns) if "SAP" in s]


ny_df_subset_RF.drop_duplicates(inplace = True)
ny_df_subset_OLS.drop_duplicates(inplace = True)
ny_df_subset_RF.y = map(np.int,ny_df_subset_RF.y)
ny_df_subset_OLS.y = map(np.int,ny_df_subset_OLS.y)


## Train/Test Split
k_fold = sklearn.cross_validation.ShuffleSplit(len(ny_df_subset_OLS.y), n_iter=200,
                                                         test_size=0.2)

full_rmse = []
full_train_rmse = []

proposed_rmse = []
proposed_train_rmse = []


restricted_rmse = []
restricted_train_rmse = []


full_rmse_OLS = []
full_train_rmse_OLS = []

proposed_rmse_OLS = []
proposed_train_rmse_OLS = []


restricted_rmse_OLS = []
restricted_train_rmse_OLS = []




for k, (train, test) in enumerate(k_fold):
    print( 'iteration #', k)
    df_subset_train_RF = ny_df_subset_RF.iloc[train,:]
    df_subset_test_RF = ny_df_subset_RF.iloc[test,:]
    
    df_subset_train_OLS = ny_df_subset_OLS.iloc[train,:]
    df_subset_test_OLS = ny_df_subset_OLS.iloc[test,:]
    
    
    
    ## Hyperparamter Search set-up
    clf = sklearn.ensemble.RandomForestRegressor(n_jobs = 2)
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
    model.fit(df_subset_train_RF[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])], 
                    df_subset_train_RF.y)
    y_predict_full = model.predict(df_subset_test_RF[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])
    full_rmse.append(sqrt(mean_squared_error(df_subset_test_RF.y, y_predict_full)))
    
    y_predict_train = model.predict(df_subset_train_RF[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])
    full_train_rmse.append(sqrt(mean_squared_error(df_subset_train_RF.y, y_predict_train)))

    
    ##
    ## Train/Proposed-RMSE
    ##
    p = np.mean(df_subset_train_RF.asian_SUP==1)
    n_SUP1 = np.int(np.round(p * 1000))
    n_SUP0 = 1000 - n_SUP1

    df_subset_test_proposed_tmp = df_subset_train_RF.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 0
    yhat0= n_SUP0 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])

    df_subset_test_proposed_tmp = df_subset_train_RF.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 1
    yhat1= n_SUP1 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])
    y_predict = (yhat0 + yhat1)/(n_SUP0 + n_SUP1)
    proposed_train_rmse.append(sqrt(mean_squared_error(df_subset_train_RF.y, y_predict)))
    
    
    ##
    ## Test/Proposed-RMSE
    ##
    p = np.mean(df_subset_test_RF.asian_SUP==1)
    n_SUP1 = np.int(np.round(p * 1000))
    n_SUP0 = 1000 - n_SUP1

    df_subset_test_proposed_tmp = df_subset_test_RF.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 0
    yhat0= n_SUP0 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])

    df_subset_test_proposed_tmp = df_subset_test_RF.copy()
    df_subset_test_proposed_tmp.drop('asian_SUP', inplace=True, axis = 1)
    df_subset_test_proposed_tmp['asian_SUP'] = 1
    yhat1= n_SUP1 * model.predict(df_subset_test_proposed_tmp[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])
    y_predict_proposed = (yhat0 + yhat1)/(n_SUP0 + n_SUP1)
    proposed_rmse.append(sqrt(mean_squared_error(df_subset_test_RF.y, y_predict_proposed)))
    
   
    ## Restricted
    model.fit(df_subset_train_RF[SAP_cols_RF],
                        df_subset_train_RF.y)
    y_predict_restricted = model.predict(df_subset_test_RF[SAP_cols_RF])
    y_predict_train_restricted= model.predict(df_subset_train_RF[SAP_cols_RF])

    restricted_rmse.append(sqrt(mean_squared_error(df_subset_test_RF.y, y_predict_restricted)))
    restricted_train_rmse.append(sqrt(mean_squared_error(df_subset_train_RF.y, y_predict_train_restricted)))

    
    
    model = sklearn.linear_model.LinearRegression()
    
    ## Full
    (MSE_full_test, predict_full_test, b0_full_test, coef_full_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                    method="Full",
                                                                                                    SAP_features=SAP_cols_OLS,
                                                                                                    SUP_features=['asian_SUP'],
                                                                                                    CP_features=['complaint_CP'],
                                                                                                    outcome=['y'],
                                                                                                    data_train = df_subset_train_OLS,
                                                                                                    data_test = df_subset_test_OLS)
    
    full_rmse_OLS.append(MSE_full_test)
    
    (MSE_full_train, predict_full_train, b0_full_train, coef_full_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                        method="Full",
                                                                                                        SAP_features=SAP_cols_OLS,
                                                                                                        SUP_features=['asian_SUP'],
                                                                                                        CP_features=['complaint_CP'],
                                                                                                        outcome=['y'],
                                                                                                        data_train = df_subset_train_OLS,
                                                                                                        data_test = df_subset_train_OLS)
    full_train_rmse_OLS.append(MSE_full_train)

    
    ##
    ## Train/Proposed-RMSE
    ##
    (MSE_prop_train, predict_prop_train, b0_prop_train, coef_prop_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                        method="Proposed",
                                                                                                        SAP_features=SAP_cols_OLS,
                                                                                                        SUP_features=['asian_SUP'],
                                                                                                        CP_features=['complaint_CP'],
                                                                                                        outcome=['y'],
                                                                                                        data_train = df_subset_train_OLS,
                                                                                                        data_test = df_subset_train_OLS)
    proposed_train_rmse_OLS.append(MSE_prop_train)
    
    
    ##
    ## Test/Proposed-RMSE
    ##
    (MSE_prop_test, predict_prop_test, b0_prop_test, coef_prop_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                    method="Proposed",
                                                                                                    SAP_features=SAP_cols_OLS,
                                                                                                    SUP_features=['asian_SUP'],
                                                                                                    CP_features=['complaint_CP'],
                                                                                                    outcome=['y'],
                                                                                                    data_train = df_subset_train_OLS,
                                                                                                    data_test = df_subset_test_OLS)
    proposed_rmse_OLS.append(MSE_prop_test)
    
 
    ## Restricted
    model.fit(df_subset_train_OLS[SAP_cols_OLS],
                        df_subset_train_OLS.y)
    y_predict_restricted = model.predict(df_subset_test_OLS[SAP_cols_OLS])
    y_predict_train_restricted= model.predict(df_subset_train_OLS[SAP_cols_OLS])

    restricted_rmse_OLS.append(sqrt(mean_squared_error(df_subset_test_OLS.y, y_predict_restricted)))
    restricted_train_rmse_OLS.append(sqrt(mean_squared_error(df_subset_train_OLS.y, y_predict_train_restricted)))

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

results.to_csv('./output/NYC_output_all_years_additional_features_meanSUP.csv')


