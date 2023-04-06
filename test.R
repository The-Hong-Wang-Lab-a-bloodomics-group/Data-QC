library(dplyr)
data("iris")
data <- iris %>%
  rename(group = "Species") %>%
  select(-group)
data_group <- iris %>%
  rename(group = "Species") %>%
  select(group)
value_colour <- c("setosa" = "#00A087FF",# control group
                  "versicolor" = "#E64B35FF",# Experimental group
                  "virginica" = "#4DBBD5FF",# other group1
                  "other group2" = "#3C5488FF")# other group2
QC_boxplot(data,data_group,value_colour)
QC_heatmap(data,data_group,value_colour)
QC_PCA(data,data_group,value_colour)
