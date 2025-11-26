import os
from codecarbon import EmissionsTracker
from sklearn.svm import SVR
from sklearn.model_selection import KFold
import pandas as pd
import numpy as np
import sys



def e_tracker(size, iters, dataset, target_col):

    path = os.getcwd() + "/DATA_REGRESSION/"
    case_study = dataset

    with EmissionsTracker(log_level="error"):
        data = pd.read_csv(path+case_study+".csv")
        X = data.drop(columns=[target_col], axis=1)
        y = data[target_col]

        svr = SVR(C=C, gamma=gamma)

        kf = KFold(n_splits=10, shuffle=True, random_state=123)
        rmses = []
        for train_index, test_index in kf.split(X):
            svr.fit(X.iloc[train_index], y.iloc[train_index])
            y_hat = svr.predict(X.iloc[test_index])
            rmse = np.sqrt(np.mean((y_hat - y.iloc[test_index])**2))
            rmses.append(rmse)
        
        rmse_mean = np.mean(rmses)
        
    return rmse_mean


C = float(sys.argv[1])
gamma = float(sys.argv[2])
data = str(sys.argv[3])
target_col = str(sys.argv[4])

rmse = e_tracker(C, gamma, data, target_col)
#os.system('cls')
print(rmse)
