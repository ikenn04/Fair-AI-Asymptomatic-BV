library(tidyverse)
library(reshape)
library(reshape2) 
library(gt)
library(stringr)
library(confintr)

data = read.csv("METRICS.csv")
data$Feature_Set[data$Feature_Set == 'SVM'] = c('All Features')

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

############Feature selected from total subset


#BAC

df = data
df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
df_long <- df_long[str_detect(df_long$Feature_Set, "only", negate = TRUE),]
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                Asian = df_table$Balanced.Accuracy.Asian , Asian_MOE = df_moe$Balanced.Accuracy.Asian,
                Black = df_table$Balanced.Accuracy.Black , Black_MOE = df_moe$Balanced.Accuracy.Black,
                Hispanic = df_table$Balanced.Accuracy.Hispanic , Hispanic_MOE = df_moe$Balanced.Accuracy.Hispanic,
                White = df_table$Balanced.Accuracy.White , White_MOE = df_moe$Balanced.Accuracy.White,
                Overall = df_table$Balanced.Accuracy, Overall_MOE = df_moe$Balanced.Accuracy
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('All Features','Correlated Point Biserial','F Test','Gini','Significant Point Biserial','T Test')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "Balanced Accuracy for Each Feature Selection Method")%>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 6)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(6,7),rows = 6)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = 1)
  )   %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 1)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Balanced Accuracy All Tests Table.pdf')

#AP

df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
df_long <- df_long[str_detect(df_long$Feature_Set, "only", negate = TRUE),]
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                 Asian = df_table$AP.Asian , Asian_MOE = df_moe$AP.Asian,
                 Black = df_table$AP.Black , Black_MOE = df_moe$AP.Black,
                 Hispanic = df_table$AP.Hispanic , Hispanic_MOE = df_moe$AP.Hispanic,
                 White = df_table$AP.White , White_MOE = df_moe$AP.White,
                 Overall = df_table$AP, Overall_MOE = df_moe$AP
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('All Features','Correlated Point Biserial','F Test','Gini','Significant Point Biserial','T Test')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "AP for Each Feature Selection Method")%>%
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
    locations = cells_body(columns = c(6,7),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = 1)
  )   %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 3)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Average Precision All Tests.pdf')


#FPR TO BE CONTINUED NEED OVERALL FPR and FNR



df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("FPR", "FPR.White", "FPR.Black", "FPR.Asian", "FPR.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
df_long <- df_long[str_detect(df_long$Feature_Set, "only", negate = TRUE),]
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                 Asian = df_table$FPR.Asian , Asian_MOE = df_moe$FPR.Asian,
                 Black = df_table$FPR.Black , Black_MOE = df_moe$FPR.Black,
                 Hispanic = df_table$FPR.Hispanic , Hispanic_MOE = df_moe$FPR.Hispanic,
                 White = df_table$FPR.White , White_MOE = df_moe$FPR.White,
                 Overall = df_table$FPR, Overall_MOE = df_moe$FPR
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('All Features','Correlated Point Biserial','F Test','Gini','Significant Point Biserial','T Test')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "FPR for Each Feature Selection Method")%>%
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
    locations = cells_body(columns = c(6,7),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = 1)
  )   %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 3)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)


####################Feature selected from each ethnicity, tested against each ethnicity, T Test

#BAC
data = read.csv("METRICS.csv")
df = data
df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("Balanced.Accuracy", "Balanced.Accuracy.White", "Balanced.Accuracy.Black", "Balanced.Accuracy.Asian", "Balanced.Accuracy.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
#df_long <- df_long[str_detect(df_long$Feature_Set, "all", negate = TRUE),]
df_all <- df_long[str_detect(df_long$Feature_Set, "SVM"),]
df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
df_long <- rbind(df_long, df_all)
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                 Asian = df_table$Balanced.Accuracy.Asian , Asian_MOE = df_moe$Balanced.Accuracy.Asian,
                 Black = df_table$Balanced.Accuracy.Black , Black_MOE = df_moe$Balanced.Accuracy.Black,
                 Hispanic = df_table$Balanced.Accuracy.Hispanic , Hispanic_MOE = df_moe$Balanced.Accuracy.Hispanic,
                 White = df_table$Balanced.Accuracy.White , White_MOE = df_moe$Balanced.Accuracy.White,
                 Overall = df_table$Balanced.Accuracy, Overall_MOE = df_moe$Balanced.Accuracy
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('All Features','Overall Selected Features','Asian Features','Black Features','Hispanic Features','White Features')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "Balanced Accuracy for Each Feature Selection Method")%>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 4)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 2)
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
    locations = cells_body(columns = c(10,11),rows = 1)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Balanced Accuracy T Test Table.pdf')

#AP
df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("AP", "AP.White", "AP.Black", "AP.Asian", "AP.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
#df_long <- df_long[str_detect(df_long$Feature_Set, "all", negate = TRUE),]
df_all <- df_long[str_detect(df_long$Feature_Set, "SVM"),]
df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
df_long <- rbind(df_long, df_all)
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                 Asian = df_table$AP.Asian , Asian_MOE = df_moe$AP.Asian,
                 Black = df_table$AP.Black , Black_MOE = df_moe$AP.Black,
                 Hispanic = df_table$AP.Hispanic , Hispanic_MOE = df_moe$AP.Hispanic,
                 White = df_table$AP.White , White_MOE = df_moe$AP.White,
                 Overall = df_table$AP, Overall_MOE = df_moe$AP
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('All Features','Overall Selected Features','Asian Features','Black Features','Hispanic Features','White Features')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "Balanced Accuracy for Each Feature Selection Method")%>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 4)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(6,7),rows = 6)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = 1)
  )   %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 1)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Average Precision T Test Table.pdf')








#FPR
df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("FPR", "FPR.White", "FPR.Black", "FPR.Asian", "FPR.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
#df_long <- df_long[str_detect(df_long$Feature_Set, "all", negate = TRUE),]
df_all <- df_long[str_detect(df_long$Feature_Set, "SVM"),]
df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
df_long <- rbind(df_long, df_all)
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                 Asian = df_table$FPR.Asian , Asian_MOE = df_moe$FPR.Asian,
                 Black = df_table$FPR.Black , Black_MOE = df_moe$FPR.Black,
                 Hispanic = df_table$FPR.Hispanic , Hispanic_MOE = df_moe$FPR.Hispanic,
                 White = df_table$FPR.White , White_MOE = df_moe$FPR.White,
                 Overall = df_table$FPR, Overall_MOE = df_moe$FPR
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('Combined Selected Features','Asian Features','Black Features','Hispanic Features','White Features')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "FPR T Test Table")%>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 5)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 5)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(6,7),rows = 5)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = c(3,5))
  )  %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 5)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Average Precision T Test Table.pdf')


#FNR
df_long <- melt(df, id = "Feature_Set") 
performance_measures = c("FNR", "FNR.White", "FNR.Black", "FNR.Asian", "FNR.Hispanic")
df_long <- dplyr::filter(df_long, variable %in% performance_measures)
#df_long <- df_long[str_detect(df_long$Feature_Set, "all", negate = TRUE),]
df_all <- df_long[str_detect(df_long$Feature_Set, "SVM"),]
df_long <- df_long[str_detect(df_long$Feature_Set, "Ttest"),]
df_long <- rbind(df_long, df_all)
df_mean <- cast(df_long, Feature_Set~variable, mean)
df_moe <- cast(df_long, Feature_Set~variable, moe)

df_mean<- df_mean %>%  mutate_if(is.numeric,round,digits = 3)
df_moe<- df_moe %>% mutate_if(is.numeric,round,digits = 3)

df_table = df_mean
df_table = cbind(Selection_Method = df_table$Feature_Set,
                 Asian = df_table$FNR.Asian , Asian_MOE = df_moe$FNR.Asian,
                 Black = df_table$FNR.Black , Black_MOE = df_moe$FNR.Black,
                 Hispanic = df_table$FNR.Hispanic , Hispanic_MOE = df_moe$FNR.Hispanic,
                 White = df_table$FNR.White , White_MOE = df_moe$FNR.White,
                 Overall = df_table$FNR, Overall_MOE = df_moe$FNR
)
df_table = as.data.frame(df_table)

df_table$Selection_Method = c('Combined Selected Features','Asian Features','Black Features','Hispanic Features','White Features')

table = gt(df_table, rowname_col = "Selection_Method") %>% 
  tab_header(title = "FNR T Test Table")%>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(2,3),rows = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(4,5),rows = 1)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(6,7),rows = 1)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(8,9),rows = c(2,4,5))
  )  %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(10,11),rows = 1)
  ) %>% 
  cols_label(
    Asian_MOE = "+/-",
    Black_MOE = "+/-",
    Hispanic_MOE = "+/-",
    White_MOE = "+/-",
    Overall_MOE = "+/-",
  )

show(table)
gtsave(table, filename = 'Plots and Tables/Average Precision T Test Table.pdf')
