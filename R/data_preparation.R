data_preparation <- function(data,data_group){
  library(tidyr)
  library(dplyr)
  if (all(row.names(data) != row.names(data_group))) {
    stop("the row names in the data must match those in the group")
  }
  if (dim(data_group)[2] > 1) {
    stop("For this function to run properly, there must be only one group column.")
  }
  if (dim(data_group)[2] == 1) {
    if (colnames(data_group) != "group") {
      warning("suggest to rename the column name of group to 'group'")
      colnames(data_group) <- "group"
    }
  }
  merge_data <- data %>%
    mutate(sampleid = rownames(data))
  merge_group <- data_group %>%
    mutate(sampleid = rownames(data_group)) %>%
    rename(group = group)
  merge_result <- merge(merge_data,merge_group,by = "sampleid")
  rownames(merge_result) <- merge_result$sampleid
  return(merge_result)
}
