import utils
from sklearn.metrics import confusion_matrix
feature_sets = [ 'Ttest features with all ethnicities',
                   'Ttest features with asian only',
                     'Ttest features with black only',
                  'Ttest features with hispanic only',
                     'Ttest features with white only', 
                'Gini features with all ethnicities',
                'Significant PBtest features with all ethnicities',
                'Correlated PBtest features with all ethnicities',
                'Ftest features with all ethnicities','all']
#@title dictionary for params
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
#@title yeah
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


def nested(model,seed, feature_set):
  
  
  if feature_set == "all":
    x,y = utils.get_XY()
    x= x.to_numpy()
    y = y.to_numpy()
  else: x, y = utils.get_xy_features(feature_set)
  df = pd.DataFrame()
  return_probs = []
  return_ytrue = []
  other_df= pd.DataFrame()
  ethnicities,strat = utils.ethnicities(),utils.get_strat()
  i = 0
  for kfold, (train, test) in enumerate(StratifiedKFold(n_splits=5, 
        shuffle=True, random_state= seed).split(x,
      strat)):
      params = class_train(model,x[train],y[train],strat[train])
      m = model_grid[model](**params)
      m.fit(x[train], y[train])
      y_true = y[test]
      predictions = m.predict(x[test]) >=.5
      e_test = ethnicities[test]
      temp = utils.fairness_metrics(y_true, predictions, e_test)
      probs =m.predict_proba(x[test])[:,1]
      temp["AUC"] = roc_auc_score(y_true, probs)
      temp["f1"]= f1_score(y_true,predictions)
      temp["precision"] = precision_score(y_true,predictions)
      temp["recall"] = recall_score(y_true,predictions)
      temp["AP"] = average_precision_score(y_true, probs)
      tn, fp, fn, tp = confusion_matrix(y_true,predictions).ravel()
      temp['FPR'] = fp/(fp+tn)
      temp['FNR'] = fn/(fn+tp)
      for race in ["Asian", "Black", "Hispanic", "White"]:
        temp[f"AP {race}"] = average_precision_score(y_true[e_test==race],probs[e_test==race])

      df = temp if df.empty else df.append(temp)  
      temp2 = pd.DataFrame()
      temp2['y_true'] = y_true
      temp2['probs'] = probs
      temp2['fold'] = i
      other_df = temp2 if other_df.empty else other_df.append(temp2)
      
      i=i+1



  return df, other_df 
def experiment(model,feature_set):
    metrics, probs = pd.DataFrame(),pd.DataFrame()

    for seed in range(0,10):
      temp1, temp2 = nested(model, seed, feature_set)
      temp2['run'] = seed
      metrics = temp1 if metrics.empty else metrics.append(temp1)
      probs = temp2 if probs.empty else probs.append(temp2)
      
    return metrics.assign(Feature_Set=feature_set), probs.assign(Feature_Set=feature_set)
    #return pd.concat([nested(model, seed) for seed in range(1, 10)]).assign(Model=model)
def run_experiments(): 
    metrics, probs = pd.DataFrame(),pd.DataFrame()
    model = "SVM"
    for feature_set in feature_sets:
      temp1, temp2 = experiment(model, feature_set)
      metrics = temp1 if metrics.empty else metrics.append(temp1)
      probs = temp2 if probs.empty else probs.append(temp2)
    return metrics, probs
metrics, probs = run_experiments()
metrics.to_csv("../METRICS.csv")
probs.to_csv("../PROBS.csv")
