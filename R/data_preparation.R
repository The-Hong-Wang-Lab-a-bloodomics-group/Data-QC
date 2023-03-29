data_preparation <- function(data,group){
  library(tidyr)
  data$sampleid <- rownames(data)
  group$sampleid <- rownames(group)
  merge_data <- merge(data,group,by = "sampleid")
  rownames(merge_data) <- merge_data$sampleid
}
