library(tidyverse)
library(reshape)
library(reshape2) 

boxstat = function(measure){
  df_long <- melt(df, id = "Race") 
  if (measure == 'Balanced Accuracy') {
    performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
  } else if (measure == 'Average Precision'){
    performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
  }
  
  df_long <- dplyr::filter(df_long, variable %in% performance_measures)
  
  df_boxstats <- cast(df_long, Race~variable, quantile)
  colnames(df_boxstats) <- c('Training','Asian Min','Asian LQ','Asian Mean','Asian UQ','Asian Max',
                             'Black Min','Black LQ','Black Mean','Black UQ','Black Max',
                             'Hispanic Min','Hispanic LQ','Hispanic Mean','Hispanic UQ','Hispanic Max',
                             'White Min','White LQ','White Mean','White UQ','White Max',
                             'Overall Min','Overall LQ','Overall Mean','Overall UQ','Overall Max')
  stat = t(data.matrix(df_boxstats))
  stat = as.data.frame(stat)
  colnames(stat) <- c('Asian','Black','Hispanic','White')
  stat = stat[-1,]
  return(stat)
}

df <- read.csv("svm_metrics.csv")

ba_box = boxstat(measure = "Balanced Accuracy")
ap_box = boxstat(measure = "Average Precision")

write.csv(ba_box, "Plots and Tables/Figure 3c table.csv")
write.csv(ap_box, "Plots and Tables/Figure 3e table.csv")