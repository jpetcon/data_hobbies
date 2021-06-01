#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import pandas as pd
import numpy as np
from sklearn.neighbors import LocalOutlierFactor


# In[ ]:


def run(report_file):
    
    """A small function which takes a dataframe and runs Local Outlier Factor detection to check if the first value in each column in the data frame is an outlier. The function
    returns the result as a table showing the outlier variable as a -1 (outlier) or 1 (in-range) value, along with the name of the column.
    
    Input report_file needs to be in pandas dataframe format with data aranged as weeks descending in rows and categories as columns"""

    output_file = pd.DataFrame()

    for i in range(len(report_file.columns)):

        od_file = report_file.iloc[:,i]

        od_file = pd.DataFrame(od_file)

        od_file = od_file.replace({'%':''}, regex=True).replace({',':''}, regex=True).astype(float)
        od_file = od_file.replace(np.nan,0)

        lof_model = LocalOutlierFactor()

        y_pred = lof_model.fit_predict(od_file)

        od_file['outlier'] = y_pred

        df = pd.DataFrame(data = {'column': [od_file.columns[0]], 'outlier': [od_file['outlier'][0]]})

        output_file = output_file.append(df)
    
        
    return(results_table)

