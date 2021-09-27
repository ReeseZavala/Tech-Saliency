## In Collaboration with Alper Gencer
rm(list = ls())

library(tidyverse)
library(stringi)
library(stringr)
library(manifestoR)
library(quanteda)
library(tm)
library(readtext)
library(corpus)
library(googleLanguageR)
library(utils)




##  Create new dataframe that will rule to the rest
newDF <- data.frame(matrix(, nrow = 1, ncol = 2))




## FIND DOCUMENT NAMES ##
fp = tools::list_files_with_exts(dir='C:/Users/rzava/Box Sync/Tech Saliency/Corpora/Small', ext='csv', full.names = F)
## RESET WD ##
setwd("C:/Users/rzava/Box Sync/Tech Saliency/Corpora/Small")

step <- 0
for (t in fp) {
## READ IN THE DOCUMENT TO DF ##
myDF <- read.csv(as.character(t))
print(paste0(as.character(t), "is read"))
  ## FOR EACH ROW K, ITERATE OVER COLUMNS i AND PASTE INTO LIST ##
  for (k in 1:nrow(myDF)){
    print(paste0(as.character(k), "row started"))
    my.list <- as.list(NULL)
    for (i in 3:ncol(myDF)){
      print(paste0(as.character(i), "column started"))
      if (is.na(myDF[k,i]) != T){
        my.list <- c(my.list, myDF[k,i])
        print(paste0(k, "th row, max number col is ", i))
      }
    }
    
    ## ADD NEW LIST OBJECT TO NEWDF ##
    newDF[step + k,1] <- myDF[k,2]
    newDF[step + k,2] <- paste(unlist(my.list), collapse = " ")
    print(paste0(as.character(t)," is done"))
  }
  rm(myDF)
  step <- nrow(newDF)
}

## Check to make sure full docs are in second column
nchar(newDF[,2])



