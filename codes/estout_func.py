#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import itertools
import statsmodels.api as sm


# In[2]:


def estout_ols(modellist, modellist_str, y, caption, label, list_regression_sets, p_values):
    
    roman_numb = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]

    # some dependencies from models-list
    num_models = len(modellist)

    # row generation
    ## Column name row
    num_cols = ""
    col_name = " "
    for i in range(0,num_models):
        num_cols = num_cols + "c"
        col_name = col_name + " & (" + roman_numb[i] + ")"

    ## get standard error types
    cov_types = []
    for model in modellist:
        cov_types.append(model.cov_type)

    cov_types = pd.DataFrame(cov_types, columns=['cluster'])
    cov_types.reset_index(inplace=True)
    cov_types.rename({'index':'model'}, axis=1, inplace=True)

    string_cov_type = f"\\multicolumn{{{num_models +1}}}{{c}}{{\\begin{{footnotesize}}" 
    for model in range(len(modellist_str)):
        cov_types.loc[cov_types['model'] == model, 'model'] = modellist_str[model]
        string_cov_type = string_cov_type + f" {cov_types['model'][model]} has {cov_types['cluster'][model]}, "

    string_cov_type = string_cov_type +  "standard errors in parentheses. \end{footnotesize}}"

    # get list of all regressors used
    variablelist = []
    for model in list_regression_sets:
        variablelist.append(model.columns.to_list())

    variablelist = np.unique(list(itertools.chain.from_iterable(variablelist)))

    # R^2
    string_rsq = f"Adj. $R^2$"
    for model in modellist:
        string_rsq = string_rsq + " & " + f"{np.round(model.rsquared_adj, 4)}"

    # no. observations
    string_nobs = f"Observations"
    for model in modellist:
        string_nobs = string_nobs + " & " + f"{int(model.nobs)}"

    # mse
    string_mse = f"Mean Squared Error"
    for model in modellist:
        string_mse = string_mse + " & " + f"{model.mse_model}"

    # f-stat
    string_f = f"F-statistic"
    for model in modellist:
        string_f = string_f + " & " + f"{model.fvalue}"

    # output for copy-paste in latex
    print(f"\\begin{{table}}[htbp] \caption{{{caption} \label{{{label}}}}}")
    print(f"\\resizebox{{0.9\\textwidth}}{{!}}{{ \centering")
    print(f"\\begin{{tabular}}{{l{num_cols}}} \hline")
    print(f"{col_name} \\\ ")
    print(f"Dependent Variable & \multicolumn{{{num_models}}}{{c}}{{{y}}} \\\ \hline \\vspace{{4pt}} ")
    print("& \\begin{footnotesize}\\end{footnotesize} " * num_models + " \\\ ")
    # To add
    for variable in variablelist:
        string_var = f"{variable}"
        string_se = " \\vspace{4pt}"
        for model in modellist:

            if (variable in model.params):
                string_var = string_var + " & " + f"{round(model.params[variable], 4)}"
                if (p_values == True) & (model.pvalues[variable] <= 0.01):
                    string_var = string_var + "***"
                    
                elif (p_values == True) & (0.01 < model.pvalues[variable] <= 0.05):
                    string_var = string_var + "**"
                    
                elif (p_values == True) & (0.05 < model.pvalues[variable] <= 0.1):
                    string_var = string_var + "*"
                    
                string_se = string_se + " & \\begin{footnotesize}(" + f"{round(model.bse[variable], 4)}" + ") \\end{footnotesize}"
            else:
                string_var = string_var + " & "
                string_se = string_se + " & \\begin{footnotesize} \\end{footnotesize}"

        print(f"{string_var} \\\ ")
        print(f"{string_se} \\\ ")

    print(f"{string_nobs} \\\ ")
    print(f"{string_rsq} \\\ ")
    print(f"{string_f} \\\ ")
    print(f"{string_mse} \\\ \hline")
    print(f"{string_cov_type} \\\ ")
    if p_values == True:
        print(f"\\multicolumn{{{num_models +1 }}}{{c}}{{\\begin{{footnotesize}} *** p$<$0.01, ** p$<$0.05, * p$<$0.1 \end{{footnotesize}}}}")
    print(f"\\end{{tabular}}")
    print(f"}}")
    print(f"\\end{{table}}")


# In[ ]:




