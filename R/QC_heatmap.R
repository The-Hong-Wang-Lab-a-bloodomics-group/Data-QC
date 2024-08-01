#' Title
#'
#' @param data A data frame. The columns in the data box are the sample names and the rows are gene.
#' And the column names in the data must match row names in the data_group.
#' @param data_group A data frame. The row names in the data_group must match those in the data. The first column name must be id.
#' The second column name must be group.
#' @param value_colour The annotation_col must be a data frame.
#' The row names of annotation_col == the column names of data_heatmap.
#' The column names of annotation_col is the annotation legend name.
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
#'  QC_heatmap(data,data_group,value_colour)
QC_heatmap <- function(data, data_group = NULL, value_colour = NULL) {
  requireNamespace("tidyr")
  requireNamespace("dplyr")
  requireNamespace("pheatmap")

  # Check if data_group is NULL or an empty data frame
  if (is.null(data_group) || (is.data.frame(data_group) && nrow(data_group) == 0)) {
    data_heatmap <- scales::rescale(scale(t(data)), to = c(-2, 2))
    pheatmap::pheatmap(t(data_heatmap),
                       show_rownames = FALSE,
                       scale = "row",
                       treeheight_row = 0,
                       legend_breaks = c(-2, 0, 2),
                       color = colorRampPalette(c("#4DBBD5FF", "white", "red"))(100))
  } else {
    data_pre <- as.data.frame(t(data))
    data_pre$id <- rownames(data_pre)
    data_pre <- merge(data_group, data_pre, by = 'id')
    rownames(data_pre) <- data_pre$id
    data_heatmap <- data_pre %>%
      dplyr::select(-c(group, id)) %>%
      t() %>%
      as.data.frame() %>%
      {log2((. + 1))}

    # Check if value_colour is provided
    if (is.null(value_colour)) {
      stop("value_colour must be provided when data_group is not NULL")
    }

    # annotation_col requirements:
    # 1.the annotation_col must be a data frame
    # 2.the row names of annotation_col == the column names of data_heatmap
    # 3.the column names of annotation_col is the annotation legend name
    #
    # annotation_colors requirements:
    # 1.the annotation_colors must be a list.
    # 2.if the group is more than two, you can add by format.
    annotation_heatmap <- data_pre %>%
      dplyr::select(group)

    annotation_colors <- list(group = value_colour)

    pheatmap::pheatmap(as.matrix(data_heatmap),
                       show_rownames = FALSE,
                       annotation_col = annotation_heatmap,
                       annotation_colors = annotation_colors,
                       scale = "row")
  }
}
# add globalVariables definition
utils::globalVariables(c("group", "id", "."))
