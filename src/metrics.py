from sklearn.metrics import recall_score, precision_score,f1_score, accuracy_score
import numpy as np
import itertools

import aif360
import aif360.datasets
import aif360.metrics
import aif360.explainers
import aif360.sklearn.metrics
import pandas as pd
import utils

      

def get_acc( y_true,predictions, ethnic):
  ret= np.zeros(4)
  ret[0] = accuracy_score(y_true[ethnic=="Asian"],predictions[ethnic =="Asian"])
  ret[1] = accuracy_score(y_true[ethnic=="Black"],predictions[ethnic =="Black"])
  ret[2] = accuracy_score(y_true[ethnic=="Hispanic"],predictions[ethnic =="Hispanic"])
  ret[3] = accuracy_score(y_true[ethnic=="White"],predictions[ethnic =="White"])
  return ret
encodeRace = np.vectorize(utils.encodeRace)
def make_binary_dataset(y_values,ethnicities):
  k = np.array(list(map(utils.one_hot_encode,encodeRace(ethnicities))))
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
"""
We are using unpriveledged to be one race, ie black, and priveledged to be non black
"""
def fairness_metrics(y, y_hat, ethnicities):
  df = pd.DataFrame()
  accuracies = get_acc(y, y_hat, ethnicities)
  i = 0
  for race in ["Asian", "Black", "Hispanic", "White"]:
    k = MakeClassificationMetric(y, y_hat, ethnicities, race)
    df[f"EqualOppP {race}"] = [k.equal_opportunity_difference()] # half of equalOpp
    df[f"EquOdds {race}"]= [k.average_odds_difference()] # half of equ oddds
    df[f"accuracy {race}"] = accuracies[i]
    i= i+1
  df["GEO all groups"] = [k.between_all_groups_generalized_entropy_index(alpha=2)]
  
  
  return df








