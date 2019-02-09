# -*- coding: utf-8 -*-
"""
Created on Sat Feb  9 01:47:27 2019

@author: avaca
"""

from sklearn.mixture import GaussianMixture

import pandas as pd

import numpy as np

data = pd.read_csv('transformados02.csv')

target = data['TARGET']

data.drop('TARGET', axis = 1, inplace = True)



def get_clusters(X, components):
            
    bic = 1000000
    
    comp = 0
    
    for i in range(1, components):
        
        model = GaussianMixture(components, covariance_type = 'full', random_state = 7).fit(X)
        
        bic_model = model.bic(X)
        
        if bic_model < bic:
            
            comp = components
            
            bic = bic_model
        
        else:
            
            next
    
    best_model = {str(comp) + 'components_bic': [bic]}
    
    return best_model


        
        
            
        
        
        
        
        
        
        
        
        
        
        
