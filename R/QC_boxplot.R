#' Title
#'
#' @param data A data frame. The columns in the data box are the sample names and the rows are gene.
#' And the column names in the data must match row names in the data_group.
#' @param data_group A data frame. The row names in the data_group must match those in the data. The first column name must be id.
#' The second column name must be group.
#' @param value_colour The annotation_col must be a data frame.
#' The row names of annotation_col == the column names of data_heatmap.
#' The column names of annotation_col is the annotation legend name.
#' @param title It is the title of the boxplot.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' library(dplyr)
#' library(DataQC)
#' library(ggplot2)
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
#'  QC_boxplot(data,data_group,value_colour,title = "iris")
QC_boxplot <- function(data,
                       data_group = NULL,
                       value_colour = NULL, title) {
  # Check if data_group is NULL or an empty data frame
  if (is.null(data_group) || (is.data.frame(data_group) && nrow(data_group) == 0)) {
    data_ggplot <- as.data.frame(t(data))
    data_ggplot$id <- rownames(data_ggplot)
    data_ggplot <- tidyr::gather(data_ggplot, key = "key", value = "value", -c("id"))

    ggplot2::ggplot(data_ggplot, ggplot2::aes(x = id, y = log2(value))) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, colour = "black", size = 10),
            axis.text.y = ggplot2::element_text(hjust = 1, colour = "black", size = 10),
            plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::labs(x = "", title = title)
  } else {
    data_ggplot <- as.data.frame(t(data))
    data_ggplot$id <- rownames(data_ggplot)
    data_ggplot <- merge(data_group, data_ggplot, by = 'id')
    data_ggplot <- tidyr::gather(data_ggplot, key = "key", value = "value", -c("id", "group"))
    if (is.null(value_colour)) {
      stop("value_colour must be provided when data_group is not NULL")
    }

    if (length(value_colour) != length(unique(data_ggplot$group))) {
      warning("the length of value_colour is not equal to the length of data_group")
    }
    if (length(value_colour) < length(unique(data_ggplot$group))) {
      stop("the length of value_colour is less than the length of data_group")
    }

    data_ggplot <- data_ggplot[order(data_ggplot$group), ]
    data_ggplot$id <- factor(data_ggplot$id, levels = unique(data_ggplot$id))
    data_ggplot <- data_ggplot[order(data_ggplot$id), ]

    if (!is.factor(data_ggplot$group)) {
      data_ggplot$group <- factor(data_ggplot$group)
    }

    ggplot2::ggplot(data_ggplot, ggplot2::aes(x = id, y = log2(value), fill = group)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_fill_manual(values = value_colour) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, colour = "black", size = 10),
            axis.text.y = ggplot2::element_text(hjust = 1, colour = "black", size = 10),
            plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::labs(x = "", title = title)
  }
}
# add globalVariables definition
utils::globalVariables(c("group", "id", ".","value"))
