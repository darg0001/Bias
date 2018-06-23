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

zip_SAP.columns = np.array(zip_SAP.columns) + '_SAP'
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
k_fold = sklearn.cross_validation.ShuffleSplit(len(ny_df_subset_OLS.y), n_iter=50,
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
    df_subset_train_RF = ny_df_subset_RF.iloc[train,:]
    df_subset_test_RF = ny_df_subset_RF.iloc[test,:]
    
    df_subset_train_OLS = ny_df_subset_OLS.iloc[train,:]
    df_subset_test_OLS = ny_df_subset_OLS.iloc[test,:]
    
    
    ## Hyperparamter Search set-up
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
                    n_jobs = 2)
    
    ## Full
    model.fit(df_subset_train_RF[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])], 
                    df_subset_train_RF.y)
    y_predict_full = model.predict(df_subset_test_RF[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])
    full_rmse.append(sqrt(mean_squared_error(df_subset_test_RF.y, y_predict_full)))
    
    y_predict_train = model.predict(df_subset_train_RF[sum([SAP_cols_RF, ['complaint_CP'], ['asian_SUP']], [])])
    full_train_rmse.append(sqrt(mean_squared_error(df_subset_train_RF.y, y_predict_train)))

    
    ## Full without CP
    model.fit(df_subset_train_RF[sum([SAP_cols_RF, ['asian_SUP']], [])],
                    df_subset_train_RF.y)
    y_predict_full = model.predict(df_subset_test_RF[sum([SAP_cols_RF, ['asian_SUP']], [])])
    full_rmse_without_CP.append(sqrt(mean_squared_error(df_subset_test_RF.y, y_predict_full)))

    y_predict_train = model.predict(df_subset_train_RF[sum([SAP_cols_RF, ['asian_SUP']], [])])
    full_train_rmse_without_CP.append(sqrt(mean_squared_error(df_subset_train_RF.y, y_predict_train)))




    model = sklearn.linear_model.LinearRegression()
    
    ## Full
    model.fit(df_subset_train_OLS[sum([SAP_cols_OLS, ['complaint_CP'], ['asian_SUP']], [])],
                    df_subset_train_OLS.y)
    #print model.best_params_
    y_predict_full = model.predict(df_subset_test_OLS[sum([SAP_cols_OLS, ['complaint_CP'], ['asian_SUP']], [])])
    full_rmse_OLS.append(sqrt(mean_squared_error(df_subset_test_OLS.y, y_predict_full)))
    
    y_predict_train = model.predict(df_subset_train_OLS[sum([SAP_cols_OLS, ['complaint_CP'], ['asian_SUP']], [])])
    full_train_rmse_OLS.append(sqrt(mean_squared_error(df_subset_train_OLS.y, y_predict_train)))

    ## Full -- without CP
    model.fit(df_subset_train_OLS[sum([SAP_cols_OLS,  ['asian_SUP']], [])],
                    df_subset_train_OLS.y)
    y_predict_full = model.predict(df_subset_test_OLS[sum([SAP_cols_OLS, ['asian_SUP']], [])])
    full_rmse_OLS_without_CP.append(sqrt(mean_squared_error(df_subset_test_OLS.y, y_predict_full)))

    y_predict_train = model.predict(df_subset_train_OLS[sum([SAP_cols_OLS, ['asian_SUP']], [])])
    full_train_rmse_OLS_without_CP.append(sqrt(mean_squared_error(df_subset_train_OLS.y, y_predict_train)))

results = pd.DataFrame({'Full_RF_RMSE_test': full_rmse,
                       'Full_RF_RMSE_train': full_train_rmse,
                       'Full_OLS_RMSE_test': full_rmse_OLS,
                       'Full_OLS_RMSE_train': full_train_rmse_OLS,
                       'Full_RF_RMSE_test_without_CP': full_rmse_without_CP,
                       'Full_RF_RMSE_train_without_CP': full_train_rmse_without_CP,
                       'Full_OLS_RMSE_test_without_CP': full_rmse_OLS_without_CP,
                       'Full_OLS_RMSE_train_without_CP': full_train_rmse_OLS_without_CP})


results.to_csv('./output/NYC_output_with_without_complaints.csv')


