# -*- coding: utf-8 -*-
"""
Created on Tue Feb  5 03:47:28 2019

@author: avaca
"""

from sklearn.ensemble import RandomForestRegressor

import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split

from sklearn.preprocessing import MinMaxScaler

from sklearn.preprocessing import StandardScaler

from sklearn.model_selection import GridSearchCV

data = pd.read_csv("transformados02.csv")

#data.drop('Unnamed: 0', axis = 1, inplace = True)

#data.drop('HY_cod_postal', axis = 1, inplace = True)

datadic = {}

for group in data['groups']:
    
    datadic[str(group)+'_df'] = data[data['groups'] == group]




y = np.array(data['TARGET']).reshape(-1, 1)

X = np.array(data.drop('TARGET', axis = 1))

scaler_y = MinMaxScaler()

y_scaled = scaler_y.fit_transform(y)

X_train, X_test, y_train, y_test = train_test_split(X, y, 
                                                    test_size = 0.2,
                                                    random_state = 7)




def median_absolute_error(y_true, y_pred):
    
    return np.median(np.abs(y_true - y_pred))

y_scaled = np.ravel(y_scaled)

def train_forests():
    
    predicciones = {}
    
    estimadores = [i for i in range(10, 50, 10)]
        

    for df, estimador in zip(datadic, estimadores):
        
        d = pd.DataFrame(datadic[df])
        
        y = np.array(d['TARGET']).reshape(-1, 1)

        X = np.array(d.drop('TARGET', axis = 1))
        
        scaler_y = StandardScaler()
        
        y_scaled = scaler_y.fit_transform(y)
        
        y_scaled = np.ravel(y_scaled)
        
        X_train, X_test, y_train, y_test = train_test_split(X, y_scaled, 
                                                            test_size = 0.2,
                                                            random_state = 7)
        
        
            
        rf = RandomForestRegressor(n_estimators = estimador, 
                                       criterion = "mae",
                                       n_jobs = -1, 
                                       random_state = 7)
            
        rf.fit(X_train, y_train)
            
        pred = rf.predict(X_test)
        
        error = median_absolute_error(y_test, pred)
            
        print('el error con {} estimadores para el grupo {} ha sido de {}'.format(estimador, group, error))
            
        predicciones[str(estimador)+ ' estimadores' + 'grupo' + str(group)] = error
            
        
        #pred_df = pd.DataFrame(predicciones)
        
        #pred_df.to_csv('predicciones_forests_per_number_estimators.csv', encoding = "utf-8")
            
    return predicciones
    

predicciones = train_forests()

param_grid = {
        
        'n_estimators': [i for i in range(10, 1000, 10)],
        'criterion': ['mae'],
        'random_state': [7],
        'max_depth': [i for i in range(1, 1000, 10)],
        'min_samples_split': [i for i in range(2, 10, 1)],
        'min_samples_leaf': [i for i in range(1, 10, 1)],
        'n_jobs': [-1]}

myrf = GridSearchCV(estimator = RandomForestRegressor(),
                    param_grid=param_grid)

myrf.fit(X_train, np.ravel(y_train))

rf_predictions = myrf.predict(X_test)

y_test1 = np.exp(y_test) - 1

rf_pred1 = np.exp(rf_predictions) - 1

mae = median_absolute_error(y_test1, rf_pred1)

print(mae)

