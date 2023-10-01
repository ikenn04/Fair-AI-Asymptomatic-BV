library(tidyverse)
library(reshape)
library(reshape2) 
library(gt)
library(stringr)
library(confintr)

df <- read.csv("METRICS.csv")
df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")

df_long <- dplyr::filter(df_long, variable %in% performance_measures)
df_long <- df_long[str_detect(df_long$Feature_Set, "only", negate = TRUE),]
df_boxstats <- cast(df_long, Feature_Set~variable, quantile)

df_boxstats$Feature_Set = c('All Features','Correlated Point Biserial',
                                         'F Test','Gini','Significant Point Biserial',
                                         'T Test')

colnames(df_boxstats) = c('Feature Set','Asian Min','Asian LQ','Asian Median','Asian UQ','Asian Max',
                       'Black Min','Black LQ','Black Median','Black UQ','Black Max',
                       'Hispanic Min','Hispanic LQ','Hispanic Median','Hispanic UQ','Hispanic Max',
                       'White Min','White LQ','White Median','White UQ','White Max',
                       'Overall Min','Overall LQ','Overall Median','Overall UQ','Overall Max')
stat = t(data.matrix(df_boxstats))
stat = as.data.frame(stat)
colnames(stat) <- c('All Features', 'Correlated Point Biserial','F Test','Gini','Significant Point Biserial','T Test')
stat = stat[-1,]

write.csv(stat, "Plots and Tables/Figure 4a table.csv")




##############################

boxstat <- function(measure){
  df_long <- melt(df, id = "Feature_Set") 
  
  if (measure == 'Balanced Accuracy') {
    overall = TRUE
    performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
  } else if (measure == 'Average Precision'){
    overall = TRUE
    performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
  }else if (measure == 'FPR'){
    overall = FALSE
    performance_measures = c("FPR","FPR.White", "FPR.Black", "FPR.Asian", "FPR.Hispanic")
  }else if (measure == 'FNR'){
    overall = FALSE
    performance_measures = c("FNR","FNR.White", "FNR.Black", "FNR.Asian", "FNR.Hispanic")
  }
  
  df_long <- dplyr::filter(df_long, variable %in% performance_measures)
  df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
  df_boxstats <- cast(df_long, Feature_Set~variable, quantile)
  
  box1 <- df_boxstats[2:5,]
  box2 <- df_boxstats[1,]
  df_boxstats <- rbind(box1,box2)
  
  df_boxstats$Feature_Set = c('Asian Features','Black Features',
                                              'Hispanic Features','White Features', 'All Ethnicities')
  
  colnames(df_boxstats) = c('Feature Set','Asian Min','Asian LQ','Asian Median','Asian UQ','Asian Max',
                            'Black Min','Black LQ','Black Median','Black UQ','Black Max',
                            'Hispanic Min','Hispanic LQ','Hispanic Median','Hispanic UQ','Hispanic Max',
                            'White Min','White LQ','White Median','White UQ','White Max',
                            'Overall Min','Overall LQ','Overall Median','Overall UQ','Overall Max')
  stat = t(data.matrix(df_boxstats))
  stat = as.data.frame(stat)
  colnames(stat) <- c('Asian Features','Black Features',
                      'Hispanic Features','White Features', 'All Ethnicities')
  stat = stat[-1,]
  
  return(stat)
}

df <- read.csv("METRICS.csv")

ba_box = boxstat(measure = "Balanced Accuracy")
ap_box = boxstat(measure = "Average Precision")
fpr_box = boxstat(measure = "FPR")
fnr_box = boxstat(measure = "FNR")

write.csv(ba_box, "Plots and Tables/Figure 4c table.csv")
write.csv(ap_box, "Plots and Tables/Figure 4d table.csv")
write.csv(fpr_box, "Plots and Tables/Figure 5a table.csv")
write.csv(fnr_box, "Plots and Tables/Figure 5b table.csv")
