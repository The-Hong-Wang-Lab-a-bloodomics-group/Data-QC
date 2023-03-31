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
QC_PCA <- function(data,data_group,value_colour){
  library(FactoMineR)
  library(factoextra)
  library(tidyr)
  source("./R/data_preparation.R")
  data_pre <- data_preparation(data,data_group)
  group_list <- data_pre$group
  dat.pca <- data_pre %>%
    subset(.,select = -c(group,sampleid)) %>%
    PCA(.,graph = FALSE)
  fviz_pca_ind(dat.pca,
               geom.ind = c("text","point"), # show points only (nbut not "text")
               col.ind = group_list, # color by groups
               palette = value_colour,
               addEllipses = TRUE, # Concentration ellipses
               legend.title = "Groups"
  )
}
