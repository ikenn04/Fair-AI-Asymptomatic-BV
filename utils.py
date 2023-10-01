import numpy as np
import pandas as pd
import requests
import io
import aif360
import aif360.datasets
import aif360.metrics
import aif360.explainers
import aif360.sklearn.metrics
from sklearn.metrics import balanced_accuracy_score 

from sklearn.model_selection import train_test_split
def get_data():
  df= pd.read_csv("../data/BV Dataset copy.csv")
  df = df.drop([394,395,396], axis = 0)
  return df
def get_XY_split():
  X,y = get_XY()
  return train_test_split(X, y,test_size=0.2, random_state=1)
def get_XY():
  df = get_data().copy()  # make a copy of the original DataFrame
  X = df.iloc[:,:-1]
  y = df["Nugent score"].copy()  # make a copy of the "Nugent score" column
  X=X.drop(labels= ['Ethnic Groupa', 'Community groupc '], axis=1)
  #Normalize 16s RNA data
  X.iloc[:,1::]=X.iloc[:,1::]/100
  X["pH"] = X["pH"]/7
  #Binary y
  y[y<7]=0
  y[y>=7]=1
  return X,y
def get_strat():
    x, y = get_XY()
    y = y.to_numpy()
    return np.array(list(map(str, y))) + ethnicities()
def encodeRace(race):
  if race == "Asian":
    return 0
  elif race == "Black":
    return 1
  elif race == "Hispanic":
    return 2
  elif race == "White":
    return 3
def one_hot_encode(race):
  ret = np.zeros(4)
  ret[race]= 1
  return ret
def get_XY_with_race():
    df = get_data()
    protected = list(np.unique(df['Ethnic Groupa']))
    y= df['Nugent score'] >=7
    df = df.drop(columns = ["Community groupc "])
    df["Race"] = list(map(encodeRace,df["Ethnic Groupa"]))
    df = df.drop(columns = ["Ethnic Groupa"])
    k = np.array(list(map(one_hot_encode,df["Race"])))
    df["Asian"] = k[:,0]
    df["Black"] = k[:,1]
    df["Hispanic"] = k[:,2]
    df["White"] = k[:,3]
    df2 = df.drop(columns = ["Nugent score"])
    df2 = df2/100
    df2["pH"] = df2["pH"] * 100/7
    df2["Asian"] = list(map(int,df2["Asian"] * 100))
    df2["Black"] = list(map(int,df2["Black"] * 100))
    df2["Hispanic"] = list(map(int,df2["Hispanic"] * 100))
    df2["White"] = list(map(int,df2["White"] * 100))
    df2.drop(columns="Race")
    return df2,y
def ethnicities():
    return get_data().to_numpy()[:,0]
def groups():
    return get_data().to_numpy()[:,1]

def feature_selection_df():
  return pd.read_csv("../results/Features in one CSV/Allfeatures_AllTests.csv")
def get_features(features):
  df = feature_selection_df()
  df = df[df["Feature Test"]== features]
  df = df.drop(columns = "Feature Test")
  return df.columns[(df==1).to_numpy()[0,:]].to_numpy()
def get_xy_features(features):
  x,y = get_XY()
  df= get_features(features)
  return x[df].to_numpy(),y.to_numpy()
def printFeatureSelectionMethods():
  print(feature_selection_df()["Feature Test"])
encodeRace = np.vectorize(encodeRace)
def make_binary_dataset(y_values,ethnicities):
  k = np.array(list(map(one_hot_encode,encodeRace(ethnicities))))
  df = pd.DataFrame()
  df["Asian"] = k[:,0]
  df["Black"] = k[:,1]
  df["Hispanic"] = k[:,2]
  df["White"] = k[:,3]
  df["positivity"] = y_values
  dataset =  aif360.datasets.BinaryLabelDataset(
    favorable_label=True,
    unfavorable_label=False,
    df=df,
    label_names=['positivity'],
    protected_attribute_names=["Asian", "Black", "Hispanic", "White"])
  return dataset
def MakeClassificationMetric(y, y_hat, ethnicities,unprivileged_race):
  ground_truth = make_binary_dataset(y,ethnicities)
  predictions = make_binary_dataset(y_hat,ethnicities)
  return aif360.metrics.ClassificationMetric(ground_truth,
                                          predictions,
                                          privileged_groups=[ {unprivileged_race: 0}],
                                          unprivileged_groups=[{unprivileged_race: 1}])
def fairness_metrics(y, y_hat, ethnicities):
  df = pd.DataFrame()

  i = 0
  for race in ["Asian", "Black", "Hispanic", "White"]:
    k = MakeClassificationMetric(y, y_hat, ethnicities, race)
    df[f"FPR {race}"] = [k.false_positive_rate(False)]
    df[f"FNR {race}"]= [k.false_negative_rate(False)]
    df[f"FDR {race}"] = [k.false_discovery_rate(False)]
    df[f"FOM {race}"]= [k.false_omission_rate(False)]
    df[f"EqualOppP {race}"] = [k.equal_opportunity_difference()] # half of equalOpp
    df[f"EquOdds {race}"]= [k.average_odds_difference()] # half of equ oddds
    df[f"Accuracy {race}"]= [k.accuracy(False)]
    df[f"Balanced Accuracy {race}"]= balanced_accuracy_score(y[ethnicities==race],y_hat[ethnicities==race])
    
    
    i= i+1
  df["Accuracy"] = k.accuracy(None)
  df["Balanced Accuracy"] = balanced_accuracy_score(y,y_hat)
  
  
  
  return df
"""
We are using unpriveledged to be one race, ie black, and priveledged to be non black
"""

  
