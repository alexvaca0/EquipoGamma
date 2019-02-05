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

data = pd.read_csv("transformados01.csv", encoding = "utf-8")

data.drop('Unnamed: 0', axis = 1, inplace = True)

y = np.array(data['TARGET']).reshape(-1, 1)

X = np.array(data.drop('TARGET', axis = 1))

scaler_y = MinMaxScaler()

y_scaled = scaler_y.fit_transform(y)

X_train, X_test, y_train, y_test = train_test_split(X, y_scaled, 
                                                    test_size = 0.2,
                                                    random_state = 7)




def median_absolute_error(y_true, y_pred):
    
    return np.median(np.abs(y_true - y_pred))

y_scaled = np.ravel(y_scaled)

def train_forests():
    
    predicciones = {}


    estimadores = [i for i in range(10, 1000, 10)]
    
    for estimador in estimadores:
        
        rf = RandomForestRegressor(n_estimators = estimador, 
                                   criterion = "mae",
                                   n_jobs = -1, 
                                   random_state = 7)
        
        rf.fit(X_train, y_train)
        
        pred = rf.predict(X_test)
        
        error = median_absolute_error(y_test, pred)
        
        print('el error con {} estimadores ha sido de {}'.format(estimador, error))
        
        predicciones[str(estimador)] = error
        
    
    pred_df = pd.DataFrame(predicciones)
    
    pred_df.to_csv('predicciones_forests_per_number_estimators.csv', encoding = "utf-8")
        
    return predicciones
    

train_forests()