import os
from codecarbon import EmissionsTracker
from sklearn import neural_network
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

        mlp = neural_network.MLPRegressor(hidden_layer_sizes=[size, size//2, size//4],activation="tanh",
                                          solver="adam",
                                          learning_rate="constant",
                                          learning_rate_init=0.01,
                                          max_iter=iters,
                                          shuffle=True,
                                          random_state=42,
                                          verbose=False,
                                          early_stopping=False,
                                          validation_fraction=0)

        kf = KFold(n_splits=10, shuffle=True, random_state=123)
        rmses = []
        for train_index, test_index in kf.split(X):
            mlp.fit(X.iloc[train_index], y.iloc[train_index])
            y_hat = mlp.predict(X.iloc[test_index])
            rmse = np.sqrt(np.mean((y_hat - y.iloc[test_index]) ** 2))
            rmses.append(rmse)
    
        rmse_mean = np.mean(rmses)
    
    return rmse_mean

size = int(sys.argv[1])
iters = int(sys.argv[2])
data = str(sys.argv[3])
target_col = str(sys.argv[4])

rmse = e_tracker(size, iters, data, target_col)
#os.system('cls')
print(rmse)


