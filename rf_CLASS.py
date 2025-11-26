import os
from codecarbon import EmissionsTracker
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import StratifiedKFold
import pandas as pd
import sys


def e_tracker(n_est, max_feat, dataset):

    path = os.getcwd() + "/DATA_CLASSIFICATION/"
    case_study = dataset

    with EmissionsTracker(log_level="error"):
        data = pd.read_csv(path+case_study+".csv")
        X = data.drop(["target"], axis=1)
        y = data.target
        y = y.astype("category")

        rf = RandomForestClassifier(n_estimators=n_est,
                                criterion='gini',
                                max_depth=None,
                                min_samples_split=2,
                                min_samples_leaf=1,
                                min_weight_fraction_leaf=0.0,
                                max_features=max_feat,
                                max_leaf_nodes=None,
                                min_impurity_decrease=0.0,
                                bootstrap=True,
                                n_jobs=6)

        kf = StratifiedKFold(n_splits=10)
        scores = list()
        for i, (train_index, test_index) in enumerate(kf.split(X,y)):
            rf.fit(X.loc[train_index], y[train_index])
            y_hat = rf.predict(X.loc[test_index])
            acc = sum(y_hat == y[test_index]) / len(test_index)
            scores.append(acc)

        accuracy = sum(scores)/len(scores)

    return accuracy

n_est = int(sys.argv[1])
max_feat =  float(sys.argv[2])
data = str(sys.argv[3])

accuracy = e_tracker(n_est, max_feat, data)
#os.system('cls')
print(accuracy)
