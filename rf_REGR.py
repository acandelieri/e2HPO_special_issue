import os
from codecarbon import EmissionsTracker
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold
import pandas as pd
import sys
import numpy as np



def e_tracker(size, iters, dataset, target_col):

    path = os.getcwd() + "/DATA_REGRESSION/"
    case_study = dataset

    with EmissionsTracker(log_level="error"):
        data = pd.read_csv(path+case_study+".csv")
        X = data.drop(columns=[target_col], axis=1)
        y = data[target_col]

        rf = RandomForestRegressor(n_estimators=n_est,
                                criterion='squared_error',
                                max_depth=None,
                                min_samples_split=2,
                                min_samples_leaf=1,
                                max_features=max_feat,                                
                                bootstrap=True,
                                n_jobs=6)

        kf = KFold(n_splits=10, shuffle=True, random_state=123)
        rmses=[]
        for train_index, test_index in kf.split(X):
            rf.fit(X.iloc[train_index], y.iloc[train_index])
            y_hat = rf.predict(X.iloc[test_index])
            rmse = np.sqrt(np.mean((y_hat - y.iloc[test_index]) ** 2))
            rmses.append(rmse)
        
        rmse_mean = np.mean(rmses)
        
    return rmse_mean


n_est = int(sys.argv[1])
max_feat =  float(sys.argv[2])
data = str(sys.argv[3])
target_col = str(sys.argv[4])

rmse = e_tracker(n_est, max_feat, data, target_col)
#os.system('cls')
print(rmse)
