QC_heatmap <- function(data,group,colour,filename = NA){
  library(tidyr)
  source("./R/data_preparation.R")
  value_colour <- colour
  data_pre <- data_preparation(data,group)
  data_heatmap <- subset(data_pre,
                         select = -c(group,sampleid)
                         )
  data_heatmap <- as.data.frame(t(data_heatmap)
                                )
  data_heatmap <- log2(data_heatmap + 1)
  library(pheatmap)
  # annotation_col requirements:
  # 1.the annotation_col must be a data frame
  # 2.the row names of annotation_col == the column names of data_heatmap
  # 3.the column names of annotation_col is the annotation legend name
  #
  # annotation_colors requirements:
  # 1.the annotation_colors must be a list.
  # 2.if the group is more than two, you can add by format.
  annotation_heatmap <- group

  annotation_colors <- list(group = value_colour)
  if (!is.na(filename)) {
    pdf(file = paste0(filename,"pdf"),
        height = 5,
        width = 5
    )
    pheatmap(data_heatmap,
             show_rownames = F,
             annotation_col = annotation_heatmap,
             annotation_colors = annotation_colors)
    dev.off()

    png(filename = paste0(filename,"png"),
        height = 2000,
        width = 2000,
        res = 300# ppi
        )
    pheatmap(data_heatmap,
             show_rownames = F,
             annotation_col = annotation_heatmap,
             annotation_colors = annotation_colors)
    dev.off()
    }
  if (is.na(filename)) {
    pheatmap(data_heatmap,
             show_rownames = F,
             annotation_col = annotation_heatmap,
             annotation_colors = annotation_colors)
    }
}
