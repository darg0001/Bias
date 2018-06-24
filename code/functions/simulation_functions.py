def num_multiply(dt, SUP_col):
    p = np.mean(dt[SUP_col])
    n_SUP1 = np.int(np.round(p * 1000))
    n_SUP0 = 1000 - n_SUP1
    return(n_SUP0, n_SUP1)

def rmse_predict_fitted_model(model, dt, cols, outcome):
    y_predict = model.predict(dt[cols])
    return(sqrt(mean_squared_error(dt[outcome], y_predict)))

def predict_fitted_model_proposed1(model, dt_tmp, cols, cols_full, outcome):
    dt = dt_tmp.copy()
    dt.drop('SUP', inplace=True, axis = 1)
    dt.rename(columns={'proposed_SUP1':'SUP'}, inplace=True)
    y_predict = model.predict(dt[cols_full])
    return(y_predict)


def predict_fitted_model_proposed0(model, dt_tmp, cols, cols_full, outcome):
    dt = dt_tmp.copy()
    dt.drop('SUP', inplace=True, axis = 1)
    dt.rename(columns={'proposed_SUP0':'SUP'}, inplace=True)
    y_predict = model.predict(dt[cols_full])
    return(y_predict)


def RMSE_results(prediction, dt, outcome):
    return(sqrt(mean_squared_error(dt[outcome], prediction)))



def DT_hyperparameter(model, method, SAP_features, SUP_features, CP_features, outcome, data):
    '''Hyperparameter search for decision trees applied to data.'''
    max_depth = [int(x) for x in np.linspace(2, 10, num = 9)]
    max_depth.append(None)
    random_grid = {'max_depth': max_depth}
    model_tmp = RandomizedSearchCV(estimator = model,
                                   param_distributions = random_grid,
                                   n_iter = 10, cv = 3, verbose=0,
                                   random_state=42, n_jobs = -1)
    if (method =="Full") | (method == "Proposed"):
        model_tmp.fit(data[sum([SAP_features, CP_features, SUP_features], [])],
                                                     data[outcome])
    if (method == "Common"):
        model_tmp.fit(data[sum([SAP_features, CP_features], [])],
                                                     data[outcome])
    if (method == "Restricted"):
        model_tmp.fit(data[SAP_features],data[outcome])
    return(model_tmp)


def DT_hyperparameter_given_grid(method, SAP_features, SUP_features, CP_features, outcome, data):
    '''Hyperparameter search for decision trees applied to data with user-specified hyperparameter grid.'''
    model = sklearn.tree.DecisionTreeRegressor(random_state = 42)
    max_depth = [int(x) for x in np.linspace(2, 10, num = 9)]
    max_depth.append(None)
    min_samples_leaf = [10, 20, 50]
    random_grid = {'max_depth': max_depth,
        'min_samples_leaf': min_samples_leaf}
    
    model_tmp = RandomizedSearchCV(estimator = model,
                                   param_distributions = random_grid,
                                   n_iter = 10, cv = 3, verbose=0,
                                   random_state=42, n_jobs = -1)

    if (method =="Full") | (method == "Proposed"):
        model_tmp.fit(data[sum([SAP_features, CP_features, SUP_features], [])],
                                                     data[outcome])
    if (method == "Common"):
        model_tmp.fit(data[sum([SAP_features, CP_features], [])],
                                                     data[outcome])
    if (method == "Restricted"):
        model_tmp.fit(data[SAP_features],data[outcome])
    return(model_tmp)



def report_DT_RF_results(model, method, SAP_features, SUP_features, SUP_proposed_cols, CP_features, outcome, data):
    '''Decision tree or Random Forest results, assuming fitted model with
        already pre-specified hyperparameters.'''
    if method=="Full":
        y_predict = model.predict(data[sum([SAP_features, CP_features, SUP_features], [])])
    if method=="Proposed":
        predicted_y = []
        for j in SUP_proposed_cols:
            predicted_y.append( model.predict(data[sum([SAP_features, CP_features, [j]], [])]))
        y_predict = np.mean(predicted_y, axis = 0)
    if method=="Common":
        y_predict = model.predict(data[sum([SAP_features, CP_features], [])])
    
    if method=="Restricted":
        y_predict = model.predict(data[SAP_features])
    return([sqrt(mean_squared_error(data[outcome], y_predict)),y_predict])




def generate_proposed_data_setup(data, SUP_features, exact = True, num_synthetic = 1000):
    '''For the P&S/proposed approach, we need to marginalize the SUP. We do that by either an
        exact approach or creating 1k synthetic SUPs based on relative SUP-class proportion.'''
    if exact:
        n_SUP1 = np.sum(data[SUP_features]==1)
        n_SUP0 = np.sum(data[SUP_features]==0)
        SUP1_pseudo = np.array([1 for j in range(n_SUP1)])
        SUP0_pseudo = np.array([0 for j in range(n_SUP0)])
        SUP_pseudo = pd.DataFrame(np.concatenate((SUP1_pseudo, SUP0_pseudo))).T
        SUP_pseudo.columns = ["SUP_" + str(x) for x in map(str,np.array(SUP_pseudo.columns))]
        
        SUP_df = pd.concat([SUP_pseudo]*len(data))
        
        data['id']=np.array(range(len(data)))
        SUP_df['id']=np.array(range(len(SUP_df)))
        
        data = data.merge(SUP_df,
                          on='id',
                          how='left')
        data.drop('id', axis=1, inplace = True)
    else:
        n_SUP1 = np.sum(data[SUP_features]==1)
        n_SUP0 = np.sum(data[SUP_features]==0)
        N = n_SUP1 + n_SUP0
        n_SUP1 = np.round(np.int(n_SUP1*num_synthetic/N))
        n_SUP0 = num_synthetic - n_SUP1
        SUP1_pseudo = np.array([1 for j in range(n_SUP1)])
        SUP0_pseudo = np.array([0 for j in range(n_SUP0)])
        SUP_pseudo = pd.DataFrame(np.concatenate((SUP1_pseudo, SUP0_pseudo))).T
        SUP_pseudo.columns = ["SUP_" + str(x) for x in map(str,np.array(SUP_pseudo.columns))]
        SUP_df = pd.concat([SUP_pseudo]*len(data))
        data['id']=np.array(range(len(data)))
        SUP_df['id']=np.array(range(len(SUP_df)))
        data = data.merge(SUP_df,
                                    on='id',
                                    how='left')
        data.drop('id', axis=1, inplace = True)
    return(data)




def generate_proposed_data_setup_train_test(data_train, data_test, SUP_features, exact = True, num_synthetic = 1000):
    '''Given a train/test split, we generate the synthetic SUPs based on training data and
        apply it to test data.'''
    if exact:
        n_SUP1 = np.sum(data_test[SUP_features]==1)
        n_SUP0 = np.sum(data_test[SUP_features]==0)
        N = n_SUP1 + n_SUP0
        n_SUP1 = np.round(np.int(n_SUP1*num_synthetic/N))
        n_SUP0 = num_synthetic - n_SUP1
        SUP1_pseudo = np.array([1 for j in range(n_SUP1)])
        SUP0_pseudo = np.array([0 for j in range(n_SUP0)])
        SUP_pseudo = pd.DataFrame(np.concatenate((SUP1_pseudo, SUP0_pseudo))).T
        SUP_pseudo.columns = ["SUP_" + str(x) for x in map(str,np.array(SUP_pseudo.columns))]
        
        SUP_df = pd.concat([SUP_pseudo]*len(data_test))
        
        data_test['id']=np.array(range(len(data_test)))
        SUP_df['id']=np.array(range(len(SUP_df)))
        
        data_test = data_test.merge(SUP_df,
                                    on='id',
                                    how='left')
        data_test.drop('id', axis=1, inplace = True)
    return(data_test)


def report_OLS_results(model, method, SAP_features, SUP_features, CP_features, outcome, data):
    '''Fit OLS model and return results.'''
    ## Fit with SAP+CP+SUP
    if method=="Full":
        model.fit(data[sum([SAP_features, CP_features, SUP_features], [])],
                  data[outcome])
        y_predict = model.predict(data[sum([SAP_features, CP_features, SUP_features], [])])
    ## Fit with SAP+CP+SUP and Predict with Marginalizing SUP
    if method=="Proposed":
        ## Fit Full Model
        model.fit(data[sum([SAP_features, CP_features, SUP_features], [])],
                  data[outcome])
        ## Apply Proposed Approach
        data_proposed = data.copy()
        data_proposed[SUP_features] = np.array(np.mean(data_proposed[SUP_features]))[0]#np.mean(data_proposed[SUP_features])
        y_predict = model.predict(data_proposed[sum([SAP_features, CP_features, SUP_features], [])])
        #print y_predict
    ## Fit with SAP+CP-only
    if method=="Common":
        model.fit(data[sum([SAP_features, CP_features], [])],
              data[outcome])
        y_predict = model.predict(data[sum([SAP_features, CP_features], [])])
    ## FIT with SAP-only
    if method=="Restricted":
        model.fit(data[SAP_features],
                            data[outcome])
        y_predict = model.predict(data[SAP_features])
    return([sqrt(mean_squared_error(data[outcome], y_predict)),y_predict,model.intercept_,np.array(model.coef_)[0]])

def report_OLS_results_train_test(model, method, SAP_features, SUP_features, CP_features, outcome, data_train, data_test):
    '''Fit OLS model and return results on train/teest.'''
    ## Fit with SAP+CP+SUP
    if method=="Full":
        model.fit(data_train[sum([SAP_features, CP_features, SUP_features], [])],
                  data_train[outcome])
        y_predict = model.predict(data_test[sum([SAP_features, CP_features, SUP_features], [])])
    ## Fit with SAP+CP+SUP and Predict with Marginalizing SUP
    if method=="Proposed":
        ## Fit Full Model
        model.fit(data_train[sum([SAP_features, CP_features, SUP_features], [])],
                  data_train[outcome])
        ## Apply Proposed Approach
        data_proposed = data_test.copy()
        data_proposed[SUP_features] = np.array(np.mean(data_proposed[SUP_features]))[0]#np.mean(data_proposed[SUP_features])
        y_predict = model.predict(data_proposed[sum([SAP_features, CP_features, SUP_features], [])])
    ## Fit with SAP+CP-only
    if method=="Common":
        model.fit(data_train[sum([SAP_features, CP_features], [])],
              data_train[outcome])
        y_predict = model.predict(data_test[sum([SAP_features, CP_features], [])])
    ## FIT with SAP-only
    if method=="Restricted":
        model.fit(data_train[SAP_features],
                            data_train[outcome])
        y_predict = model.predict(data_test[SAP_features])
    return([sqrt(mean_squared_error(data_test[outcome], y_predict)),y_predict,model.intercept_,np.array(model.coef_)[0]])

