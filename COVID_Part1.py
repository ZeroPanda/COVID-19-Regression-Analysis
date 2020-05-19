#!/usr/bin/env python
# coding: utf-8

# This is a covid-19 dataset from brazil hospital found on Kaggle.
# https://www.kaggle.com/einsteindata4u/covid19

# In[8]:

# Importing the original dataset
import pandas as pd
from matplotlib.pyplot import figure

dfo = pd.read_excel (r'C:\Users\Shrey\Downloads\COVID-19\dataset.xlsx')
figure(num=None, figsize=(20,30), dpi=100, facecolor='w', edgecolor='k')
# Plotting the graph on values count by columns
dfo.count().sort_values().plot(kind = "barh")


# In[3]:

# Subset of disease dataset
from matplotlib.pyplot import figure
df_disease = dfo[['Patient age quantile' , 'SARS-Cov-2 exam result' , 'Respiratory Syncytial Virus' , 'Influenza B' , 'Influenza A', 'Coronavirus HKU1', 'Parainfluenza 1', 'Parainfluenza 2', 'Metapneumovirus' , 'Bordetella pertussis', 'Inf A H1N1 2009', 'CoronavirusOC43','Coronavirus229E', 'Parainfluenza 4', 'Adenovirus','Chlamydophila pneumoniae','Parainfluenza 3','Rhinovirus/Enterovirus','CoronavirusNL63']]

# Excluding null values from the dataset, here picked by Adenovirus column
df_disease = df_disease[(df_disease['Adenovirus'].notnull())]
figure(num=None, figsize=(10,5), dpi=100, facecolor='w', edgecolor='k')
# Counting values count to varify
df_disease.count().sort_values().plot(kind = "barh")

# Replacing with binary variables
df_disease.iloc[:,:] = df_disease.iloc[:,:].replace({'detected': 1, 'not_detected': 0}, inplace = False)
df_disease.loc[:,"SARS-Cov-2 exam result"] = df_disease.loc[:,"SARS-Cov-2 exam result"].replace({'negative': 0, 'positive': 1}, inplace = False)
# Exporting the dataset with disease.xlsx
df_disease.to_excel(r'C:\Users\Amit R. Amin\Downloads\COVID-19\disease.xlsx')
df_disease.head()


# In[4]:


from matplotlib.pyplot import figure
from sklearn.impute import KNNImputer
import numpy as np

# Subset of condition dataset
df_cond = dfo[['Patient age quantile' , 'SARS-Cov-2 exam result' , 'Hematocrit' , 'Hemoglobin', 'Mean corpuscular hemoglobin (MCH)','Red blood Cells','Lymphocytes','Basophils','Leukocytes','Eosinophils','Mean corpuscular volume (MCV)','Red blood cell distribution width (RDW)','Platelets','Monocytes','Mean corpuscular hemoglobin concentration (MCHC)' , 'Mean platelet volume ','Neutrophils','Proteina C reativa mg/dL']]

# Excluding null values from the dataset, here picked by Red blood cells column
df_cond = df_cond[(df_cond['Red blood Cells'].notnull())]
figure(num=None, figsize=(10,5), dpi=100, facecolor='w', edgecolor='k')
# Counting values count to varify
df_cond.count().plot(kind = "barh")

# Converting to binary variables
df_cond.iloc[:,:] = df_cond.iloc[:,:].replace({'detected': 1, 'not_detected': 0}, inplace = False)
df_cond.loc[:,"SARS-Cov-2 exam result"] = df_cond.loc[:,"SARS-Cov-2 exam result"].replace({'negative': 0, 'positive': 1}, inplace = False)

# Imputing the missing values by K-Nearest Neighbor Imputer
imputer = KNNImputer(n_neighbors=5, weights="uniform")
df_cond_impute = imputer.fit_transform(df_cond)
# naming columns with column values
names = list(df_cond.columns.values)
df_cond_frame = pd.DataFrame(df_cond_impute, columns = names) 

# Exporting the dataset
df_cond_frame.to_excel(r'C:\Users\Amit R. Amin\Downloads\COVID-19\condition.xlsx')


# In[5]:

# Counting values count to re-varify
figure(num=None, figsize=(10,5), dpi=100, facecolor='w', edgecolor='k')
from matplotlib.pyplot import figure
df_cond_frame.count().plot(kind = "barh")
