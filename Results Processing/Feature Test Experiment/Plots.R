library(tidyverse)
library(reshape)
library(reshape2) 
library(stringr)


############Feature selected from total subset
plot_metrics <- function(file_name,metrics, tags, key, title, value) { 
  df <- read.csv(file_name)
  df$Feature_Set[data$Feature_Set == 'SVM'] = c('All Features')
  df_long <- melt(df, id = "Feature_Set") 
  df_long <- dplyr::filter(df_long, variable %in% metrics)
  #df_long <- df_long[str_detect(df_long$Feature_Set, "SVM"),]
  df_long <- df_long[str_detect(df_long$Feature_Set, "only", negate = TRUE),]
  
  performance_plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Feature_Set), outlier.shape = NA)+
    #ylim(0,1)+
    xlab("Ethnicity")+
    ylab(label = value)+
    scale_x_discrete(labels = tags)+
    scale_fill_discrete(name = "Selection Method", labels = key)+
    labs(title = title)+
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 10),
      legend.title = element_text(size=15),
      plot.title = element_text(size = 25)
    )
}





input = "METRICS.csv"
tags = c('Asian','Black','Hispanic','White','Overall')
key = c('All Features','Correlated Point Biserial','F Test','Gini','Significant Point Biserial','T Test')

#BAC

performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
title = c("Balanced Accuracy of all Feature Selection Methods")
plot = plot_metrics(input, performance_measures, tags, key, title, "Balanced Accuracy" )
show(plot)
ggsave('Plots and Tables/Balanced Accuracy All Tests Plot.pdf',plot = plot, width = 10, height = 5)

#AP
tags = c('Overall','Asian','Black','Hispanic','White')
performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
title = c("Average Precision of all Feature Selection Methods")
plot = plot_metrics(input, performance_measures, tags, key, title, "Balanced Accuracy" )
show(plot)
ggsave('Plots and Tables/Average Precision All Tests Plot.pdf',plot = plot, width = 10, height = 5)

###################### Feature selected from each ethnicity, tested against each ethnicity, T test only





plot_metrics <- function(file_name,metrics, tags, key, title, value) { 
  df <- read.csv(file_name)
  df_long <- melt(df, id = "Feature_Set") 
  df_long <- dplyr::filter(df_long, variable %in% metrics)
  df_long <- df_long[str_detect(df_long$Feature_Set, "SVM", negate = TRUE),]
  #df_long <- df_long[str_detect(df_long$Model_Type_with_Feature_Set, "all", negate = TRUE),]
  df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
  
  
  performance_plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Feature_Set), outlier.shape = NA)+
    ylim(0.5,1)+
    xlab("Ethnicity")+
    ylab(label = value)+
    scale_x_discrete(labels = tags)+
    scale_fill_discrete(name = "Selection Method", labels = key)+
    labs(title = title)+
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 10),
      legend.title = element_text(size=15),
      plot.title = element_text(size = 25)
    )
}

input = "METRICS.csv"
tags = c('Asian','Black','Hispanic','White','Overall')
key = c("Combined Features",'Asian Features','Black Features','Hispanic Features','White Features')


#BAC
performance_measures = c("Balanced.Accuracy","Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
title = c("Balanced Accuracy of T Test Feature Models")
plot = plot_metrics(input, performance_measures, tags, key, title, "Balanced Accuracy" )
show(plot)
ggsave('Plots and Tables/Balanced Accuracy T Test Plot.pdf',plot = plot, width = 10, height = 5)

#AP
tags = c('Overall','Asian','Black','Hispanic','White')

performance_measures = c("AP","AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
title = c("Average Precision of T Test Feature Models")
plot = plot_metrics(input, performance_measures, tags, key, title, "Average Precision" )
show(plot)
ggsave('Plots and Tables/Average Precision T Test Plot.pdf',plot = plot, width = 10, height = 5)

###################### Feature selected from each ethnicity, tested against each ethnicity, Correlated Point Biserial only

plot_metrics <- function(file_name,metrics, tags, key, title, value) { 
  df <- data
  df_long <- melt(df, id = "Model_Type_with_Feature_Set") 
  df_long <- dplyr::filter(df_long, variable %in% metrics)
  df_long <- df_long[str_detect(df_long$Model_Type_with_Feature_Set, "SVM"),]
  df_long <- df_long[str_detect(df_long$Model_Type_with_Feature_Set, "all", negate = TRUE),]
  df_long <- df_long[str_detect(df_long$Model_Type_with_Feature_Set, "Significant PBtest"),]
  
  
  performance_plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Model_Type_with_Feature_Set), outlier.shape = NA)+
    #ylim(0,1)+
    xlab("Metrics")+
    ylab(label = value)+
    scale_x_discrete(labels = tags)+
    scale_fill_discrete(name = "Selection Method", labels = key)+
    labs(title = title)+
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 10),
      legend.title = element_text(size=15),
      plot.title = element_text(size = 25)
    )
}

input = "fairness_metric_data.csv"
tags = c('Asian','Black','Hispanic','White','Total')
key = c('Asian Features','Black Features','Hispanic Features','White Features')


#BAC
performance_measures = c("Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
title = c("Balanced Accuracy of Point Biserial Feature Models")
plot = plot_metrics(input, performance_measures, tags, key, title, "Balanced Accuracy" )
show(plot)
ggsave('Plots and Tables/Balanced Accuracy Sig PB.pdf',plot = plot, width = 10, height = 5)

#AP
performance_measures = c("AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
title = c("Average Precision of Point Biserial Feature Models")
plot = plot_metrics(input, performance_measures, tags, key, title, "Average Precision" )
show(plot)
ggsave('Plots and Tables/Average Precision Sig PB.pdf',plot = plot, width = 10, height = 5)



######################################FPR and FNR for T Test


df <- read.csv("METRICS.csv")
df_long <- melt(df, id = "Feature_Set") 
df_long <- dplyr::filter(df_long, variable %in% performance_measures)











plot_metrics <- function(file_name,metrics, tags, key, title, value) { 
  df <- read.csv(file_name)
  df_long <- melt(df, id = "Feature_Set") 
  df_long <- dplyr::filter(df_long, variable %in% metrics)
  df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
  
  
  performance_plot<- ggplot(data = df_long, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Feature_Set), outlier.shape = NA)+
    #ylim(0.5,1)+
    xlab("Ethnicity")+
    ylab(label = "FPR")+
    scale_x_discrete(labels = tags)+
    scale_fill_discrete(name = "Selection Method", labels = key)+
    labs(title = title)+
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 10),
      legend.title = element_text(size=15),
      plot.title = element_text(size = 25)
    )
}

input = "METRICS.csv"
tags = c('Asian','Black','Hispanic','White','Overall')
key = c('Overall Selected Features','Asian Features','Black Features','Hispanic Features','White Features')



#FPR
performance_measures = c("FPR.White", "FPR.Black", "FPR.Asian", "FPR.Hispanic", "FPR")
title = c("False Positive Rate of T Test Feature Models")
plot = plot_metrics(input, performance_measures, tags, key, title, "FPR" )
show(plot)
ggsave('Plots and Tables/FPR T Test Plot.pdf',plot = plot, width = 10, height = 5)



#FNR
performance_measures = c("FNR.White", "FNR.Black", "FNR.Asian", "FNR.Hispanic", "FNR")
title = c("False Negative Rate of T Test Feature Models")
plot = plot_metrics(input, performance_measures, tags, key, title, "FNR" )
show(plot)
ggsave('Plots and Tables/FNR T Test Plot.pdf',plot = plot, width = 10, height = 5)

