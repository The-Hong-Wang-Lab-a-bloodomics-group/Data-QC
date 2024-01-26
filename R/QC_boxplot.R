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
#' library(QC)
#' QC_boxplot(data,data_group,value_colour)
QC_boxplot <- function(data,data_group,value_colour){
  library(ggplot2)
  library(tidyr)
  source("./R/data_preparation.R")
  data_pre <- data_preparation(data,data_group)
  data_ggplot <- tidyr::gather(data_pre,key = "key",
                               value = "value",
                               -c("sampleid","group")
                               )
  if (length(value_colour) != length(table(data_ggplot$group))){
    warning("the length of value_colour is not equal to the length of data_group")
  }
  if (length(value_colour) < length(table(data_ggplot$group))){
    stop("the length of value_colour is less than the length of data_group")
  }
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
}


devtools::document()

