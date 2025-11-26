import os
from codecarbon import EmissionsTracker
from sklearn import svm
from sklearn.model_selection import StratifiedKFold
import pandas as pd
import sys


def e_tracker(C, gamma, dataset):
    path = os.getcwd()+"/DATA_CLASSIFICATION/"
    case_study = dataset

    with EmissionsTracker(log_level="error"):
        data = pd.read_csv(path+case_study+".csv")
        X = data.drop(["target"], axis=1)
        y = data.target
        y = y.astype("category")

        svc = svm.SVC(C=C,gamma=gamma)

        kf = StratifiedKFold(n_splits=10)
        scores = list()
        for i, (train_index, test_index) in enumerate(kf.split(X, y)):
            svc.fit(X.loc[train_index], y[train_index])
            y_hat = svc.predict(X.loc[test_index])
            acc = sum(y_hat == y[test_index]) / len(test_index)
            scores.append(acc)

        accuracy = sum(scores)/len(scores)

    return accuracy

C = float(sys.argv[1])
gamma = float(sys.argv[2])
data = str(sys.argv[3])

accuracy = e_tracker(C, gamma, data)
#os.system('cls')
print(accuracy)
