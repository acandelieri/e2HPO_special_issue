import os
from codecarbon import EmissionsTracker
from sklearn import neural_network
from sklearn.model_selection import StratifiedKFold
import pandas as pd
import sys



def e_tracker(size, iters, dataset):

    path = os.getcwd() + "/DATA_CLASSIFICATION/"
    case_study = dataset

    with EmissionsTracker(log_level="error"):
        data = pd.read_csv(path+case_study+".csv")
        X = data.drop(["target"], axis=1)
        y = data.target
        y = y.astype("category")

        mlp = neural_network.MLPClassifier(hidden_layer_sizes=[size, size//2, size//4],
                                           activation="tanh",
                                           solver="adam",
                                           learning_rate="constant",
                                           learning_rate_init=0.01,
                                           max_iter=iters,
                                           shuffle=True,
                                           random_state=42,
                                           verbose=False,
                                           early_stopping=False,
                                           validation_fraction=0)

        kf = StratifiedKFold(n_splits=10)
        scores = list()
        for i, (train_index, test_index) in enumerate(kf.split(X,y)):
            mlp.fit(X.loc[train_index], y[train_index])
            y_hat = mlp.predict(X.loc[test_index])
            acc = sum(y_hat == y[test_index]) / len(test_index)
            scores.append(acc)

        accuracy = sum(scores)/len(scores)

    return accuracy

size = int(sys.argv[1])
iters = int(sys.argv[2])
data = str(sys.argv[3])

accuracy = e_tracker(size, iters, data)
#os.system('cls')
print(accuracy)
