library(tidyverse)
library(reshape)
library(reshape2) 
data = read.csv("metrics.csv")

plot_metrics <- function(file_name,metrics, tags, title, value) { 
  df <- read.csv(file_name)
  df_long <- melt(df, id = "Model") 
  df_long <- dplyr::filter(df_long, variable %in% metrics)
  performance_plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Model), outlier.shape = NA)+
    ylim(0,1)+
    xlab("Ethnicity")+
    ylab(label = value)+
    scale_x_discrete(labels = tags)+
    scale_fill_discrete(labels = c("Logistic Regression", 'MLP','Random Forest','SVM'))+
    labs(title = title)+
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 15),
      legend.title = element_text(size=20),
      plot.title = element_text(size = 25)
    )
}

input = "metrics.csv"
tags_a = c('Asian','Black','Hispanic','White','Total')


#BAC
performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
title = c("Balanced Accuracy of All Models for All Ethnicities")
plot = plot_metrics(input, performance_measures, tags_a, title, "Balanced Accuracy" )
show(plot)
ggsave('Plots and Tables/Balanced Accuracy.pdf',plot = plot, width = 10, height = 5)

#AP

df <- read.csv("metrics.csv")
df_long <- melt(df, id = "Model") 
performance_measures = c( "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
df_long1 <- dplyr::filter(df_long, variable %in% performance_measures)
AP <- dplyr::filter(df_long, variable %in% c("AP"))
df_long = rbind(df_long1,AP)


tags = c('Total', 'Asian','Black','Hispanic','White')
plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Model), outlier.shape = NA)+
  ylim(0,1)+
  xlab("Ethnicity")+
  ylab(label = "Average Precision")+
  scale_x_discrete(labels = c('Total', 'Asian','Black','Hispanic','White'))+
  scale_fill_discrete(labels = c('Logistic Regression','MLP','Random Forest','SVM'))+
  labs(title = "Average Precision Score of All Models for All Ethnicities")+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size=20),
    plot.title = element_text(size = 25)
  )


show(plot)
ggsave('Plots and Tables/AUPRC.pdf',plot = plot, width = 10, height = 5)




tags = c('Asian','Black','Hispanic','White')

#FPR
performance_measures = c( "FPR","FPR.White", "FPR.Black", "FPR.Asian", "FPR.Hispanic")
title = c("False Positive Rate of All Models for All Ethnicities")
plot = plot_metrics(input, performance_measures, tags, title, "False Positive Rate" )
show(plot)
ggsave('Plots and Tables/FPR.pdf',plot = plot, width = 10, height = 5)

#FNR
performance_measures = c( "FNR","FNR.White", "FNR.Black", "FNR.Asian", "FNR.Hispanic")
title = c("False Negative Rate of All Models for All Ethnicities")
plot = plot_metrics(input, performance_measures, tags, title, "False Negative Rate" )
show(plot)
ggsave('Plots and Tables/FNR.pdf',plot = plot, width = 10, height = 5)

#FOR
performance_measures = c( "FOM.White", "FOM.Black", "FOM.Asian", "FOM.Hispanic")
title = c("False Omission Rate of All Models for All Ethnicities")
plot = plot_metrics(input, performance_measures, tags, title )
show(plot)
ggsave('Plots and Tables/FOR.pdf',plot = plot, width = 15, height = 10)

#FDR
performance_measures = c("FDR.White", "FDR.Black", "FDR.Asian", "FDR.Hispanic")
title = c("False Discovery Rate of All Models for All Ethnicities")
plot = plot_metrics(input, performance_measures, tags, title )
show(plot)
ggsave('Plots and Tables/FDR.pdf',plot = plot, width = 15, height = 10)
