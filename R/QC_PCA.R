#' Title
#'
#' @param data A data frame. The columns in the data box are the sample names and the rows are gene.
#' And the column names in the data must match row names in the data_group
#' @param data_group A data frame. The row names in the data_group must match those in the data. The first column name must be id.
#' The second column name must be group.
#' @param value_colour The annotation_col must be a data frame. The row names of annotation_col == the column names of data_heatmap. The column names of annotation_col is the annotation legend name
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(DataQC)
#' data("iris")
#' data <- iris %>%
#'   dplyr::rename(group = "Species") %>%
#'   dplyr::select(-group)
#' data <- as.data.frame(t(data))
#' colnames(data) <- seq(1:150)
#' data_group <- iris %>%
#'  dplyr::rename(group = "Species") %>%
#'  dplyr::select(group)
#' data_group$id <- rownames(data_group)
#' data_group <- subset(data_group,select = c(id,group))
#' value_colour <- c("setosa" = "#00A087FF",# control group
#'                   "versicolor" = "#E64B35FF",# Experimental group
#'                   "virginica" = "#4DBBD5FF",# other group1
#'                   "other group2" = "#3C5488FF")# other group2
#'  QC_PCA(data,data_group,value_colour)

QC_PCA <- function(data, data_group = NULL, value_colour = NULL) {
  # Check if data_group is NULL or an empty data frame
  if (is.null(data_group) || (is.data.frame(data_group) && nrow(data_group) == 0)) {
    data_pre <- as.data.frame(scale(t(data)))
    dat.pca <- FactoMineR::PCA(data_pre, graph = FALSE)
    factoextra::fviz_pca_ind(dat.pca,
                             geom.ind = c("text", "point"), # show points only (but not "text")
                             addEllipses = FALSE # No concentration ellipses
    )
  } else {
    data_pre <- as.data.frame(t(data))
    data_pre$id <- rownames(data_pre)
    data_pre <- merge(data_group, data_pre, by = 'id')
    rownames(data_pre) <- data_pre$id
    group_list <- data_pre$group

    # Check if value_colour is provided
    if (is.null(value_colour)) {
      stop("value_colour must be provided when data_group is not NULL")
    }

    dat.pca <- data_pre %>%
      dplyr::select(-c(group, id)) %>%
      FactoMineR::PCA(graph = FALSE)
    factoextra::fviz_pca_ind(dat.pca,
                             geom.ind = c("text", "point"), # show points only (but not "text")
                             col.ind = group_list, # color by groups
                             palette = value_colour,
                             addEllipses = TRUE, # Concentration ellipses
                             legend.title = "Groups"
    ) +
      ggplot2::theme_classic()
  }
}

# add globalVariables definition
utils::globalVariables(c("group", "id"))
