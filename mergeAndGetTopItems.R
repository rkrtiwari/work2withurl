##############################################################################################################################################
# Finding the list of files to merge
###############################################################################################################################################
filesToMerge <- function(day0, today) {
  daysAfter0 <- as.numeric(today - day0)
  daysBefore0 <- (7 - daysAfter0)
  
  if (daysAfter0 == 0){
    filesToMergeAfter0 <- NULL
  } else {
    filesToMergeAfter0 <- sprintf("%s%04d", "dailyTop10/top10day",  0:(daysAfter0-1))
  }
  
  filesToMergeBefore0 <- sprintf("%s%04d", "dailyTop10/top10day", 1000 + 1:abs(daysBefore0))
  
  return(list(filesToMergeBefore0, filesToMergeAfter0))
}

##########################################################################################################################################
# database of all items
#########################################################################################################################################
readAllCategoryItems <- function() {
  con <- file("itemCategoryDB/allCategoryItemDB.txt", "r")
  allCategoryItem <- read.table(con, header = FALSE, colClasses = c("character", "character", "character",
                                                                    "character", "numeric"), 
                                col.names = c("cat_1", "cat_2", "cat_3", "item_id", "count"),
                                sep = "\t", strip.white = TRUE, na.strings = "?")
  close(con)
  return(allCategoryItem)
}



##########################################################################################################################################
# Function to actually merge the files 
##########################################################################################################################################
mergeFiles <- function(fileNames) {
  df <- list()
  for (i in seq_along(fileNames)) {
    df[[i]] <- read.table(file = fileNames[i], header = FALSE, 
                          colClasses = c("character", "character", "character", "character", "character", "numeric"), 
                          col.names = c("cat_1", "cat_2", "cat_3", "item_id", "item_url", "count") )
  }
  
  mergedDf <- do.call(rbind, df)
  return(mergedDf)
}


######################################################################################################################################
# This function updates the count so that items sold after the supersale gets higher priority. df[1] is the data frame before the 
# start of the supersale. df[2] is the data frame after the start of the supersale
######################################################################################################################################
updateCount <- function(df) {
  df1 <- df[[1]]
  df2 <- df[[2]]
  df1$count <- 10*df1$count
  if(is.null(df2)){
    df2 <- data.frame(cat_1 = character(0), cat_2 = character(0), cat_3 = character(), 
                      item_id = character(0), item_url = character(0), count = numeric(0))
  } else {
    df2$count <- 1000*df2$count
  }
  return(list(df1,df2))
}

###################################################################################################################################
# This function arranges the items in decreasing order where they are grouped by the categories and return top 10 from each group
###################################################################################################################################
getTopItemsInCategory <- function(df){
  library(dplyr)
  df2 <- df %>% group_by(cat_1, cat_2, cat_3, item_id, item_url) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    group_by(cat_1, cat_2, cat_3) %>%
    arrange(desc(count)) %>%
    filter(row_number() <= 10) %>%
    ungroup()
  return(df2)
}


###########################################################################################################################################
# write the final result
##############################################################################################################################################
writeFinalResult <- function(df){
  con <- file("finalResult.txt", "w")
  write.table(df, con, quote= FALSE, na = "?", row.names = FALSE,
              col.names = FALSE, sep = "\t")

  close(con)
}


# writeTop10ItemsFiles <- function(filename){
#   for (i in seq_along(filename)){
#     con <- file(filename[i])
#     df <- read.table(con, sep = "\t")
#   }
# }

