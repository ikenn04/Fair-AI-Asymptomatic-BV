rm(list = ls())

library(tidyverse)
library(reshape)
library(reshape2) 
library(gt)
options(digits = 3)

moe <- function(x){
  n <- length(x)
  
  # Find the standard deviation
  standard_deviation <- sd(x)
  
  # Find the standard error
  standard_error <- standard_deviation / sqrt(n)
  
  alpha = 0.05
  degrees_of_freedom = n - 1
  t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
  
  margin_error <- t_score * standard_error
  
  return(margin_error)
}

#BAC

df <- read.csv("metrics.csv")
df_long <- melt(df, id = "Model") 
performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)


df_mean <- cast(df_long, Model~variable, mean)
df_moe <- cast(df_long, Model~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Model = df_table$Model,
                 Asian = df_table$Balanced.Accuracy.Asian , Asian_MOE = df_moe$Balanced.Accuracy.Asian,
                 Black = df_table$Balanced.Accuracy.Black , Black_MOE = df_moe$Balanced.Accuracy.Black,
                 Hispanic = df_table$Balanced.Accuracy.Hispanic , Hispanic_MOE = df_moe$Balanced.Accuracy.Hispanic,
                 White = df_table$Balanced.Accuracy.White , White_MOE = df_moe$Balanced.Accuracy.White,
                 Overall = df_table$Balanced.Accuracy, Overall_MOE = df_moe$Balanced.Accuracy
)
df_table = as.data.frame(df_table)

df_table$Model = c('Logistic Regression','MLP','Random Forest','SVM')


table <- gt(df_table, rowname_col = "Model") %>% 
  tab_header(title = "Balanced Accuracy For Each Model") %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 4)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(6,7),rows = 1)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = 1)
  )   %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 4)
  )%>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Balanced Accuracy Table.pdf')

#AP
df <- read.csv("metrics.csv")
df_long <- melt(df, id = "Model") 
performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
df_mean <- cast(df_long, Model~variable, mean)
df_moe <- cast(df_long, Model~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Model = df_table$Model,
                 Asian = df_table$AP.Asian , Asian_MOE = df_moe$AP.Asian,
                 Black = df_table$AP.Black , Black_MOE = df_moe$AP.Black,
                 Hispanic = df_table$AP.Hispanic , Hispanic_MOE = df_moe$AP.Hispanic,
                 White = df_table$AP.White , White_MOE = df_moe$AP.White,
                 Overall = df_table$AP, Overall_MOE = df_moe$AP
)
df_table = as.data.frame(df_table)

df_table$Model = c('Logistic Regression','MLP','Random Forest','SVM')


table <- gt(df_table, rowname_col = "Model") %>% 
  tab_header(title = "Average Precision For Each Model") %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 2)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(6,7),rows = 2)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = 4)
  )   %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 2)
  )%>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Average Precision Table.pdf')

