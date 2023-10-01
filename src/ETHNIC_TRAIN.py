
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.neural_network import MLPClassifier
from xgboost import XGBClassifier
from sklearn.model_selection import GridSearchCV, RepeatedStratifiedKFold,StratifiedKFold
import utils
import numpy as np
import pandas as pd
from sklearn.metrics import f1_score, precision_score, recall_score,roc_auc_score,average_precision_score

grid = {
    'LogisticRegression' : {
        'solver': ['newton-cg', 'lbfgs', 'liblinear'],
        'C': [100, 10, 1.0, 0.1, 0.01]
    },
    'SVM': {
        'kernel': ['poly', 'rbf', 'sigmoid'],
        'C': [10, 1.0, 0.1, 0.01],
        'probability':[True]
    },
    'RandomForestClassifier': {
        'n_estimators': [10, 100, 1000],
        'max_features': ['sqrt', 'log2']
    },
    'MLPClassifier':{
        'hidden_layer_sizes': [(10,30,10),(20,)],
        'solver': ['sgd', 'adam'],
        'alpha': [0.0001, 0.05],
        'learning_rate': ['constant','adaptive'],
        
    }
}
model_grid = {
    'LogisticRegression': LogisticRegression,
    'SVM': SVC, 
    'RandomForestClassifier': RandomForestClassifier,

    'MLPClassifier': MLPClassifier
}


def class_train(classifier, xtrain, ytrain,strat):
    cv = RepeatedStratifiedKFold(n_splits=4, n_repeats=2, random_state=0).split(xtrain,strat)
    grid_search = GridSearchCV(
        estimator=model_grid[classifier](), 
        param_grid=grid[classifier], 
        n_jobs=-1,
        cv=cv, 
        scoring='balanced_accuracy',
        error_score=0
    )
    clf = grid_search.fit(xtrain, ytrain)
    return clf.best_params_


def nested(model,seed,race):
  
  x, y = utils.get_XY()
  x = x.to_numpy()
  y = y.to_numpy()
  df = pd.DataFrame()
  return_probs = []
  return_ytrue = []
  other_df= pd.DataFrame()
  ethnicities,strat = utils.ethnicities(),utils.get_strat()
  i = 0
  for kfold, (train, test) in enumerate(StratifiedKFold(n_splits=5, 
        shuffle=True, random_state= seed).split(x,
      strat)):
      e_train = ethnicities[train]
      e_test = ethnicities[test]
      x_train = x[train]
      x_train = x_train[e_train==race]
      y_train = y[train]
      y_train = y_train[e_train==race]
      strat_train = strat[train]
      strat_train = strat_train[e_train==race]

      params = class_train(model,x_train ,y_train,strat_train)
      
      
     
      m = model_grid[model](**params)
      m.fit(x_train, y_train)
      y_true = y[test]
      predictions = m.predict(x[test]) >=.5
      
      temp = utils.fairness_metrics(y_true, predictions, e_test)
      probs =m.predict_proba(x[test])[:,1]
      temp["AUC"] = roc_auc_score(y_true, probs)
      temp["f1"]= f1_score(y_true,predictions)
      temp["precision"] = precision_score(y_true,predictions)
      temp["recall"] = recall_score(y_true,predictions)
      temp["AP"] = average_precision_score(y_true, probs)
      
      for race in ["Asian", "Black", "Hispanic", "White"]:
        temp[f"AP {race}"] = average_precision_score(y_true[e_test==race],probs[e_test==race])

      df = temp if df.empty else df.append(temp)  
      temp2 = pd.DataFrame()
      temp2['y_true'] = y_true
      temp2['probs'] = probs
      temp2['fold'] = i
      other_df = temp2 if other_df.empty else other_df.append(temp2)
      
      i=i+1



  return df, other_df #pd.DataFrame.from_dict({'y_true': return_ytrue, 'probs': return_probs},orient='index')
def experiment(model,race):
    metrics, probs = pd.DataFrame(),pd.DataFrame()

    for seed in range(0,10):
      temp1, temp2 = nested(model, seed,race)
      temp2['run'] = seed
      metrics = temp1 if metrics.empty else metrics.append(temp1)
      probs = temp2 if probs.empty else probs.append(temp2)
      
    return metrics.assign(Race=race), probs.assign(Race=race)
    #return pd.concat([nested(model, seed) for seed in range(1, 10)]).assign(Model=model)
def run_experiments(model): 
    metrics, probs = pd.DataFrame(),pd.DataFrame()

    for race in ["Asian", "Black", "Hispanic", "White"]:
      temp1, temp2 = experiment(model,race)
      metrics = temp1 if metrics.empty else metrics.append(temp1)
      probs = temp2 if probs.empty else probs.append(temp2)
    return metrics, probs

metrics, probs =run_experiments("SVM")
metrics.to_csv("../results/Demographic Split Training Results/justin/svm_metrics.csv")
probs.to_csv("../results/Demographic Split Training Results/justin/svm_probs.csv")

