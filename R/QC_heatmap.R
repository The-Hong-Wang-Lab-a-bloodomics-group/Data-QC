#' Title
#'
#' @param data
#' @param data_group
#' @param value_colour
#'
#' @return
#' @export
#'
#' @examples
QC_heatmap <- function(data,data_group,value_colour){
  library(tidyr)
  source("./R/data_preparation.R")
  data_pre <- data_preparation(data,data_group)
  data_heatmap <- data_pre %>%
    subset(.,select = (-c(group,sampleid))) %>%
    t() %>%
    as.data.frame() %>%
    {log2((. + 1))}
  library(pheatmap)
  # annotation_col requirements:
  # 1.the annotation_col must be a data frame
  # 2.the row names of annotation_col == the column names of data_heatmap
  # 3.the column names of annotation_col is the annotation legend name
  #
  # annotation_colors requirements:
  # 1.the annotation_colors must be a list.
  # 2.if the group is more than two, you can add by format.
  annotation_heatmap <- data_pre %>%
    select(group)

  annotation_colors <- list(group = value_colour)
  pheatmap(data_heatmap,
           show_rownames = F,
           annotation_col = annotation_heatmap,
           annotation_colors = annotation_colors)
}
