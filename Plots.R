library(tidyverse)
library(reshape)
library(reshape2) 
data = read.csv("svm_metrics.csv")

########SVM Data for Training on all Ethnicities
norm = read.csv("metrics.csv")
norm_long <- melt(norm, id = "Model")
norm_long <- dplyr::filter(norm_long, 
                           variable %in% c("Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian",
                                           "Balanced.Accuracy.Hispanic","AP.White", "AP.Black", "AP.Asian", "AP.Hispanic"))

norm_long <- dplyr::filter(norm_long, Model %in% c('SVM'))


plot_metrics <- function(file_name,metrics, tags, title, value) { 
  df <- read.csv(file_name)
  df_long <- melt(df, id = "Race") 
  df_long <- dplyr::filter(df_long, variable %in% metrics)
 
  
  
  performance_plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Race), outlier.shape = NA)+
    xlab("Ethnicity")+
    ylab(value)+
    scale_x_discrete(labels = tags)+
    scale_fill_discrete(labels=c('Asian Trained','Black Trained','Hispanic Trained','White Trained'))+
    labs(title = title)+
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 15),
      legend.title = element_text(size=20),
      plot.title = element_text(size = 25)
    )
}

input = "svm_metrics.csv"
tags = c('Asian','Black','Hispanic','White', "Overall")

#BAC
performance_measures = c("Balanced.Accuracy","Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
title = c("Balanced Accuracy of SVM Training Subsets")
plot = plot_metrics(input, performance_measures, tags, title, "Balanced Accuracy" )
show(plot)
ggsave('Plots and Tables/Balanced Accuracy Plot.pdf',plot = plot, width = 10, height = 5)

#AUPRC
#AP
tags = c("Overall",'Asian','Black','Hispanic','White')
performance_measures = c( "AP","AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
title = c("Average Precision Score of All SVM Training Subsets")
plot = plot_metrics(input, performance_measures, tags, title, "Average Precision" )
show(plot)
ggsave('Plots and Tables/Average Precision Plot.pdf',plot = plot, width = 10, height = 5)
