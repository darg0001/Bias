#!/usr/bin/env
from __future__ import division
import platform
from sklearn.metrics import mean_squared_error
from math import sqrt
from sklearn import tree
from sklearn.grid_search import RandomizedSearchCV
execfile('../../functions/python_libraries.py')
execfile('../../functions/simulation_functions.py')


N = 10000
beta_1_SAP_array = np.array([1])
beta_2_CP_array = np.array([1])
beta_3_SUP_array = np.array([1,5,10])

## amount of SAP overlap displacement
displacement_array = np.array([0,1])


## SUP via Eqn 20.
delta_0 = 0
delta_CP_array = np.array([0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2])

n_rep = 100
binned = np.array(['unbinned_SAP'])


## replace Figure 4 by varying shift_sd
shift_sd_array = np.array([1, 0.6, 0.3])

for m in binned:
    if m == 'unbinned_SAP':
        writer = pd.ExcelWriter('./output/a2_vary_deltaCP_unbinnedSAP_revised' +str(np.int(np.round(10000*random.random())))+'.xlsx', engine='xlsxwriter')
    for k in range(n_rep):
        print k
        shift_sd_record = []
        rmse_full = []
        rmse_full_train = []
        rmse_full_test = []

        rmse_prop = []
        rmse_prop_train = []
        rmse_prop_test = []

        #rmse_common = []
        rmse_restricted = []
        rmse_restricted_train = []
        rmse_restricted_test = []

        
        
        rmse_full_DT = []
        rmse_full_DT_train = []
        rmse_full_DT_test = []

        rmse_prop_DT = []
        rmse_prop_DT_train = []
        rmse_prop_DT_test = []

        #rmse_common_DT = []
        rmse_restricted_DT = []
        rmse_restricted_DT_train = []
        rmse_restricted_DT_test = []

        displace_record = []
        delta_CP_record = []
        beta1_SAP_record = []
        beta_2_CP_record = []
        beta_3_SUP_record = []
        for displacement in displacement_array:
            for delta_CP in delta_CP_array:
                for shift_sd in shift_sd_array:
                    CP  = np.random.normal(0, 1, N)
                    SUP = np.array((scipy.special.expit(delta_0 + delta_CP * CP + np.random.normal(0,1,N)) > 0.5)+0) # note: have option to choose threshold "> 0.5" or whatever
                    SAP = np.random.normal(loc = displacement * SUP,
                                           scale = (shift_sd * SUP + (1-SUP)),
                                           size = N )

                    for beta_1_SAP in beta_1_SAP_array:
                        for beta_2_CP in beta_2_CP_array:
                            for beta_3_SUP in beta_3_SUP_array:
                                
                                y = beta_1_SAP * SAP + beta_2_CP * CP + beta_3_SUP * SUP + np.random.normal(0,1, N) # note: have option to vary error term here.


                                if m == 'unbinned_SAP':
                                    df = pd.DataFrame({'SAP':SAP,
                                                      'CP':CP,
                                                      'y': y,
                                                      'SUP': SUP})
                                    SAP_cols = ["SAP"]
                            
                                ## Train/Test-Split-Step
                                k_fold = sklearn.cross_validation.ShuffleSplit(len(df.y), n_iter=1,
                                                                               test_size=0.2)
                                for n, (train, test) in enumerate(k_fold):
                                    df_train = df.iloc[train,:]
                                    df_test = df.iloc[test,:]





                                    ### OLS

                                    ## FULL APPROACH
                                    #fit on train/report on train
                                    (MSE_full_train, predict_full_train, b0_full_train, coef_full_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                         method="Full",
                                                                                                         SAP_features=SAP_cols,
                                                                                                         SUP_features=['SUP'],
                                                                                                         CP_features=['CP'],
                                                                                                         outcome=['y'],
                                                                                                         data_train = df_train,
                                                                                                        data_test = df_train)

                                    #fit on train/report on test
                                    (MSE_full_test, predict_full_test, b0_full_test, coef_full_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                         method="Full",
                                                                                                         SAP_features=SAP_cols,
                                                                                                         SUP_features=['SUP'],
                                                                                                         CP_features=['CP'],
                                                                                                         outcome=['y'],
                                                                                                         data_train = df_train,
                                                                                                        data_test = df_test)


                                    ## PROPOSED APPROACH
                                    (MSE_prop_test, predict_prop_test, b0_prop_test, coef_prop_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                     method="Proposed",
                                                                                                     SAP_features=SAP_cols,
                                                                                                     SUP_features=['SUP'],
                                                                                                     CP_features=['CP'],
                                                                                                                outcome=['y'],
                                                                                                                data_train = df_train,
                                                                                                                data_test = df_test)
                                    (MSE_prop_train, predict_prop_train, b0_prop_train, coef_prop_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                     method="Proposed",
                                                                                                     SAP_features=SAP_cols,
                                                                                                     SUP_features=['SUP'],
                                                                                                     CP_features=['CP'],
                                                                                                                outcome=['y'],
                                                                                                                data_train = df_train,
                                                                                                                data_test = df_train)





                                    (MSE_restrict_train, predict_restrict_train, b0_restrict_train, coef_restrict_train) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                                         method="Restricted",
                                                                                                                         SAP_features=SAP_cols,
                                                                                                                         SUP_features=['SUP'],
                                                                                                                         CP_features=['CP'],
                                                                                                                                outcome=['y'],
                                                                                                                                data_train = df_train,
                                                                                                                                data_test = df_train)
                                    (MSE_restrict_test, predict_restrict_test, b0_restrict_test, coef_restrict_test) =report_OLS_results_train_test(model=linear_model.LinearRegression(),
                                                                                                                         method="Restricted",
                                                                                                                         SAP_features=SAP_cols,
                                                                                                                         SUP_features=['SUP'],
                                                                                                                         CP_features=['CP'],
                                                                                                                                outcome=['y'],
                                                                                                                                data_train = df_train,
                                                                                                                                data_test = df_test)
                                    rmse_full_train.append(MSE_full_train)
                                    rmse_full_test.append(MSE_full_test)
                                    rmse_prop_train.append(MSE_prop_train)
                                    rmse_prop_test.append(MSE_prop_test)
                                    rmse_restricted_train.append(MSE_restrict_train)
                                    rmse_restricted_test.append(MSE_restrict_test)

                                    displace_record.append(displacement)
                                    delta_CP_record.append(delta_CP)
                                    beta1_SAP_record.append(beta_1_SAP)
                                    beta_2_CP_record.append(beta_2_CP)
                                    beta_3_SUP_record.append(beta_3_SUP)
                                    shift_sd_record.append(shift_sd)

                                    ## Full
                                    clf = sklearn.ensemble.RandomForestRegressor()
                                    max_depth = [5, 10]
                                    max_depth.append(None)
                                    min_samples_leaf = [10, 20, 50]
                                    min_samples_split = [2, 5, 10]
                                    n_estimators = [100]
                                    max_features = [None, 0.25, 0.5, 0.75]

                                    random_grid_full = {'max_features': max_features,
                                                    'max_depth': max_depth,
                                                    'min_samples_leaf': min_samples_leaf,
                                                    'n_estimators':n_estimators}

                                    clf_full = GridSearchCV(estimator = clf,
                                                        param_grid = random_grid_full,
                                                        cv = 3, verbose=0,
                                                        n_jobs = 2)

                                    random_grid_restricted = {'max_features': [None],
                                        'max_depth': max_depth,
                                            'min_samples_leaf': min_samples_leaf,
                                                'n_estimators':n_estimators}

                                    clf_restricted = GridSearchCV(estimator = clf,
                                                                      param_grid = random_grid_restricted,
                                                                      cv = 3, verbose=0,
                                                                      n_jobs = 2)
                                    # Full
                                    clf_full.fit(df_train[sum([SAP_cols, ['CP'], ['SUP']], [])],
                                                 df_train.y)
                                    y_predict = clf_full.predict(df_train[sum([SAP_cols, ['CP'], ['SUP']], [])])
                                    rmse_full_DT_train.append(sqrt(mean_squared_error(df_train.y, y_predict)))

                                    y_predict_test = clf_full.predict(df_test[sum([SAP_cols, ['CP'], ['SUP']], [])])
                                    rmse_full_DT_test.append(sqrt(mean_squared_error(df_test.y, y_predict_test)))




                                    ## Restricted
                                    clf_restricted.fit(df_train[SAP_cols],
                                                       df_train.y)
                                    rmse_restricted_DT_train.append(sqrt(mean_squared_error(df_train.y,
                                                          clf_restricted.predict(df_train[SAP_cols]))))
                                    rmse_restricted_DT_test.append(sqrt(mean_squared_error(df_test.y,
                                                                clf_restricted.predict(df_test[SAP_cols]))))
                                                       
                                    ## Proposed
                                    p = np.mean(df_train.SUP==1)
                                    n_SUP1 = np.int(np.round(p * 1000))
                                    n_SUP0 = 1000 - n_SUP1

                                    df_subset_test_proposed_tmp = df_train.copy()
                                    df_subset_test_proposed_tmp.drop('SUP', inplace=True, axis = 1)
                                    df_subset_test_proposed_tmp['SUP'] = 0
                                    yhat0= n_SUP0 * clf_full.predict(df_subset_test_proposed_tmp[sum([SAP_cols, ['CP'], ['SUP']], [])])

                                    df_subset_test_proposed_tmp = df_train.copy()
                                    df_subset_test_proposed_tmp.drop('SUP', inplace=True, axis = 1)
                                    df_subset_test_proposed_tmp['SUP'] = 1
                                    yhat1= n_SUP1 * clf_full.predict(df_subset_test_proposed_tmp[sum([SAP_cols, ['CP'], ['SUP']], [])])
                                    y_predict = (yhat0 + yhat1)/(n_SUP0 + n_SUP1)
                                    rmse_prop_DT_train.append(sqrt(mean_squared_error(df_train.y, y_predict)))

                                    p = np.mean(df_test.SUP==1)
                                    n_SUP1 = np.int(np.round(p * 1000))
                                    n_SUP0 = 1000 - n_SUP1

                                    df_subset_test_proposed_tmp = df_test.copy()
                                    df_subset_test_proposed_tmp.drop('SUP', inplace=True, axis = 1)
                                    df_subset_test_proposed_tmp['SUP'] = 0
                                    yhat0= n_SUP0 * clf_full.predict(df_subset_test_proposed_tmp[sum([SAP_cols, ['CP'], ['SUP']], [])])

                                    df_subset_test_proposed_tmp = df_test.copy()
                                    df_subset_test_proposed_tmp.drop('SUP', inplace=True, axis = 1)
                                    df_subset_test_proposed_tmp['SUP'] = 1
                                    yhat1= n_SUP1 * clf_full.predict(df_subset_test_proposed_tmp[sum([SAP_cols, ['CP'], ['SUP']], [])])
                                    y_predict = (yhat0 + yhat1)/(n_SUP0 + n_SUP1)
                                    rmse_prop_DT_test.append(sqrt(mean_squared_error(df_test.y, y_predict)))



            #print str(k)
        df_results = pd.DataFrame({'RMSE_Full_OLS_train': rmse_full_train,
                                      'RMSE_Proposed_OLS_train':rmse_prop_train,
                                      'RMSE_Restricted_OLS_train':rmse_restricted_train,
                                  'RMSE_Full_OLS_test': rmse_full_test,
                                  'RMSE_Proposed_OLS_test':rmse_prop_test,
                                  'RMSE_Restricted_OLS_test':rmse_restricted_test,
                                      'RMSE_Full_DT_train': rmse_full_DT_train,
                                      'RMSE_Proposed_DT_train':rmse_prop_DT_train,
                                      'RMSE_Restricted_DT_train':rmse_restricted_DT_train,
                                  'RMSE_Full_DT_test': rmse_full_DT_test,
                                  'RMSE_Proposed_DT_test':rmse_prop_DT_test,
                                  'RMSE_Restricted_DT_test':rmse_restricted_DT_test,
                                  'shift_sd_record': shift_sd_record,
                                      'displace_record':displace_record,
                                      'delta_SUP_CP':delta_CP_record,
                                      'Beta1_SAP': beta1_SAP_record,
                                      'Beta2_CP': beta_2_CP_record,
                                      'Beta3_SUP': beta_3_SUP_record})
        df_results.to_excel(writer, index=False, sheet_name=str(k))
writer.save()
