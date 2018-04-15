#Set working directory
setwd("E:\\Predective Modeling\\Project")

#Removing previous objects
rm(list = ls())


# Loading libraries - Install the packages if not pre-installed
load_lb <- function()
{
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(rpart)
  library(tree)
  library(MASS)
  library(data.table)
  library(Matrix)
  library(Amelia)
  library(mice)
  library(readr)
  library(ggplot2)
  library(caret)
  library(moments)
}

load_lb()

# Import data files - fread to read big size files
Brk <- fread("E:\\Predective Modeling\\Project\\brooklyn_sales_map.CSV", stringsAsFactors = T)


# Data dimension checks
str(Brk)   
dim(Brk)                                      #n=390883 p=29 (n*p)

# Column names
col_name <- colnames(Brk)

# class determination
col_class <- sapply(Brk, function(x) class(x))

## Numeric and factor columns division
col_num <- c()
col_fct <- c()
for(i in 1:length(col_name))
{
  if(class(Brk[[i]]) == "factor") 
  {
    col_fct[i] <- col_name[i] 
  } else
  {
    col_num[i] <- col_name[i]
  }
}
col_num <- col_num[!is.na(col_num)]
col_fct <- col_fct[!is.na(col_fct)]


###########################################################
# Total Columns - 29
# Numeric Columns - 25
# Factor Columns - 4
###########################################################

percent <- function(x, digits = 1, format = "f", ...) 
{
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Function to determine missing and empty values in columns
countMissing <- function(x,y)
{
  ## calculate counts
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  if (mode(x) == "numeric") missing1 = sum(x=="", na.rm=TRUE) else missing1 = 0
  missing <- sum(is.na(x)) + missing1
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat(" #         Column Name: ",y,"\n", sep="")
  cat("#           TOTAL ROWS: ", totalRows, " (", percent(totalRows/NROW(x)), ")\n", sep="")
  cat("# Missing Values (NAs): ", missing, " (", percent(missing/NROW(x)), ")\n", sep="")
  cat("  # Empty Strings (\"\"): ", emptyStrings, " (", percent(emptyStrings/NROW(x)), ")\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, " (", percent(nonMissing/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}

for(i in 1:length(col_name))
{
  countMissing(Brk[[i]],col_name[i])
}

# Columns starting from SchoolDist have same NA values
# Difficult to impute the values for these columns as all predictors are 
# blank for a particular entry
#
# Dropping the observations (22.3%)
Brk_new <- Brk[!is.na(Brk$SchoolDist)]

# Remove "OwnerType" - 86% NA
Brk_new <- Brk_new[,-c('OwnerType')]

# Rounding No of floors as fractions are available
Brk_new$NumFloors <- round(Brk_new$NumFloors,0)

sort(sapply(Brk_new, function(x) (sum(x==""))), decreasing = T)
sort(sapply(Brk_new, function(x) (sum(is.na(x)))), decreasing = T)


