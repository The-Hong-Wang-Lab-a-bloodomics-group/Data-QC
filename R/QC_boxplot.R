QC_boxplot <- function(data,group,colour,filename = NA){
  library(ggplot2)
  library(tidyr)
  source("./R/data_preparation.R")
  data_pre <- data_preparation(data,group)
  data_ggplot <- tidyr::gather(data_pre,key = "key",
                               value = "value",
                               -c("sampleid","group")
                               )
  value_colour <- colour
  data_ggplot <- data_ggplot[order(data_ggplot$group),]
  data_ggplot$sampleid <- factor(data_ggplot$sampleid,levels = unique(data_ggplot$sampleid))
  data_ggplot <- data_ggplot[order(data_ggplot$sampleid),]
  if (!is.factor(data_ggplot$group)) {
    data_ggplot$group <- factor(data_ggplot$group)
    }
  ggplot(data_ggplot,aes(x = sampleid,
                         y = log2(value + 1),
                         fill = group)
         ) +
    geom_boxplot() +
    scale_fill_manual(values = value_colour) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     colour = "black",
                                     size = 10),
          axis.text.y = element_text(hjust = 1,
                                     colour = "black",
                                     size = 10)
          ) +
    labs(x = "")
  # create the figure with pdf and png
  if (!is.na(filename)) {
    ggsave(filename = paste0(filename,".pdf"),
           height = 5,
           width = 5,
           plot = last_plot())
    ggsave(filename = paste0(filename,".png"),
           height = 5,
           width = 5,
           plot = last_plot())
    }
}
