library(tidyverse)
library(reshape)
library(reshape2) 



boxstat = function(measure){
  df_long <- melt(df, id = "Model") 
  if (measure == 'Balanced Accuracy') {
    overall = TRUE
    performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
  } else if (measure == 'Average Precision'){
    overall = TRUE
    performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
  }else if (measure == 'FPR'){
    overall = FALSE
    performance_measures = c("FPR.White", "FPR.Black", "FPR.Asian", "FPR.Hispanic")
  }else if (measure == 'FNR'){
    overall = FALSE
    performance_measures = c("FNR.White", "FNR.Black", "FNR.Asian", "FNR.Hispanic")
  }
    
  df_long <- dplyr::filter(df_long, variable %in% performance_measures)
  
  df_boxstats <- cast(df_long, Model~variable, quantile)
  
  if (overall == TRUE){
    colnames(df_boxstats) <- c('Model','Asian Min','Asian LQ','Asian Mean','Asian UQ','Asian Max',
                               'Black Min','Black LQ','Black Mean','Black UQ','Black Max',
                               'Hispanic Min','Hispanic LQ','Hispanic Mean','Hispanic UQ','Hispanic Max',
                               'White Min','White LQ','White Mean','White UQ','White Max',
                               'Overall Min','Overall LQ','Overall Mean','Overall UQ','Overall Max')
  }else if (overall == FALSE){
    colnames(df_boxstats) <- c('Model','Asian Min','Asian LQ','Asian Mean','Asian UQ','Asian Max',
                               'Black Min','Black LQ','Black Mean','Black UQ','Black Max',
                               'Hispanic Min','Hispanic LQ','Hispanic Mean','Hispanic UQ','Hispanic Max',
                               'White Min','White LQ','White Mean','White UQ','White Max'
                               )
  }
  
  stat = t(data.matrix(df_boxstats))
  stat = as.data.frame(stat)
  colnames(stat) <- c('Logistic Regression','MLP Classifier','Random Forest', 'SVM')
  stat = stat[-1,]
  return(stat)
  
}

df <- read.csv("metrics.csv")


ba_box = boxstat(measure = "Balanced Accuracy")
ap_box = boxstat(measure = "Average Precision")
fpr_box = boxstat(measure = "FPR")
fnr_box = boxstat(measure = "FNR")

write.csv(ba_box, "Plots and Tables/Figure 1c table.csv")
write.csv(ap_box, "Plots and Tables/Figure 1e table.csv")
write.csv(fpr_box, "Plots and Tables/Figure 2a table.csv")
write.csv(fnr_box, "Plots and Tables/Figure 2b table.csv")

