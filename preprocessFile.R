#########################################################################################################################################
# Renaming files that will make it useful during later stages
##########################################################################################################################################

renameFiles <- function(inputDir, outputDir, day0) {
  library(dplyr)
  fileNames <- list.files(inputDir)
  print(fileNames)
  
  for (i in seq_along(fileNames)) {
    df <- readInputFile(file.path(inputDir, fileNames[i]))
    df$order_date <- strptime(df$order_date, "%d/%m/%Y")
    #   fileDate <- strptime(df$order_date[1], "%d/%m/%Y %H:%M:%S")
    fileDate <- df$order_date[1]
    daysElasped <- floor(as.numeric(fileDate - day0, units = "days"))
    outFileName <- getOutputFileName(daysElasped, outputDir)
    df1 <- getRequiredColumn(df, day0)
    writeToFile(df1, outFileName)
  }
}

#################################################################################################################################################
readInputFile <- function(fileName) {
  con <- file(fileName, 'r')
  df <- read.table(con, header = FALSE, colClasses = c("character", "character", "numeric",
                                                       "numeric", "numeric", "numeric", "character", "character", "character"), 
                   col.names = c("order_date", "item_id", "unit_price", "quantity", "item_total_amount", 
                                 "category_code", "cat_1","cat_2", "cat_3"), sep = "\t", 
                   strip.white = TRUE, na.strings = "?")
  close(con)
  return(df)
} 


################################################################################################################################################### 
getOutputFileName <- function(daysElasped, outputDir) {
  if (daysElasped < 0) {
    fileName <- sprintf("%s%4d", "daysElasped", 1000 + abs(daysElasped))
  } else {
    fileName <- sprintf("%s%04d", "daysElasped", daysElasped)
  }
  
  fullFileName <- file.path(outputDir, fileName)
  return(fullFileName)
} 


####################################################################################################################################################
getRequiredColumn <- function(df, day0){
  library(dplyr)
  df1 <- df %>%
    mutate(daysElasped = floor(as.numeric(df$order_date - day0, units = "days"))) %>%
    select(cat_1, cat_2, cat_3, item_id, order_date, daysElasped)
  return(df1)
}


#####################################################################################################################################################
writeToFile <- function(df, filename) {
  con <- file(filename, 'w')
  write.table(df, con, sep = "\t", eol = "\n", na = "?", 
              quote = FALSE, col.names = FALSE, row.names = FALSE)
  close(con)
}



########################################################################################################################################################
writeTop10 <- function(filename){
  library(stringr)
  library(dplyr)
  for (i in seq_along(filename)){
    con <- file(filename[i], 'r')
    df <- read.table(con, sep = "\t", col.names = c("cat_1", "cat_2", "cat_3", "item_id", "item_url", "date", "daysElasped"),
                     colClasses = c("character", "character", "character", "character",
                                    "character", "character", "numeric"))
    close(con)
    
    df1 <- df %>%
      select(cat_1, cat_2, cat_3, item_id, item_url) %>%
      mutate(count = 1)
    
    df2 <- getTopItemsInCategory(df1)
    
    
    digits <- str_sub(filename[i],-4,-1 )
    outFileName <- str_c(c("dailyTop10/top10day", digits), collapse = "")
    
    con <- file(outFileName, 'w')
    write.table(df2, con, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE, na = "?", eol = "\n")
    close(con)
  }
}


