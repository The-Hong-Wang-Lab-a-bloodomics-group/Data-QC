QC_PCA <- function(data,group,colour,filename = NA){
  library(FactoMineR)
  library(factoextra)
  library(tidyr)
  source("./R/data_preparation.R")
  value_colour <- colour
  data_pre <- data_preparation(data,group)

  data_pca <- subset(data_pre,select = -c(group,sampleid))

  dat.pca <- PCA(data_pca,
                 graph = FALSE)
  if (!is.na(filename)) {
    pdf(file = paste0(filename,"pdf"),
        height = 5,
        width = 5
        )
    fviz_pca_ind(dat.pca,
                 geom.ind = c("text","point"), # show points only (nbut not "text")
                 col.ind = group_list, # color by groups
                 palette = value_colour,
                 addEllipses = TRUE, # Concentration ellipses
                 legend.title = "Groups"
                 )
    dev.off()

    png(filename = paste0(filename,"png"),
        height = 2000,
        width = 2000,
        res = 300# ppi
        )

    fviz_pca_ind(dat.pca,
                 geom.ind = c("text","point"), # show points only (nbut not "text")
                 col.ind = group_list, # color by groups
                 palette = value_colour,
                 addEllipses = TRUE, # Concentration ellipses
                 legend.title = "Groups"
                 )
    dev.off()
    }
  if (is.na(filename)) {
    fviz_pca_ind(dat.pca,
                 geom.ind = c("text","point"), # show points only (nbut not "text")
                 col.ind = group_list, # color by groups
                 palette = value_colour,
                 addEllipses = TRUE, # Concentration ellipses
                 legend.title = "Groups"
                 )
    }
}
