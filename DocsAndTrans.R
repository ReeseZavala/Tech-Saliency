## In Collaboration with Alper Gencer
rm(list = ls())

library(tidyverse)
library(quanteda)
library(stringi)
library(stringr)
library(tm)
library(manifestoR)
library(quanteda)
library(tm)
library(readtext)
library(corpus)
library(googleLanguageR)
library(utils)


gl_auth("C:/Users/rzava/Box Sync/Tech Saliency/R scripts/GAPI.json")


setwd("C:/Users/rzava/Box Sync/Tech Saliency/Corpora/Small/")
## FOLDER NAMES ##
Mmas <- substr(list.dirs(), 3,4)
## First one is blank for some reason ##
Mmas <- Mmas[2:length(Mmas)]
for (f in Mmas){

  ## FIRST, GET TXT NAMES ##
  fp = tools::list_files_with_exts(dir=f, ext='txt', full.names = TRUE)
  fn = tools::list_files_with_exts(dir=f, ext='txt', full.names = FALSE)
  fn = tools::file_path_sans_ext(fn)
  items = length(fp) # get our iterator length for files
  
  ## CREATE DF WITH LOTS OF EMPTY COLUMNS ##
  myDF <- matrix(ncol = 65, nrow = 1)
  myDF <- data.frame(myDF)
  
  ## GETTING ORIGINAL FILES IN COLUMN 2 ##
  for(i in 1:items) {  # iterate through files
    myDF[i,1] <- fn[i]
    myDF[i,2] <- paste(unlist(readLines(fp[i], warn = F)), collapse =" ")
  }

  ## SPLIT DOCUMENT  ##
  for (i in 1:nrow(myDF)){
    timer <- 1
    charnum <- nchar(myDF[i,2])
    while (charnum >  25000){  
      myDF[i,timer + 2] <- substr(myDF[i,2], 25000*(timer - 1), 25000 * timer)
      charnum <- charnum - 25000
      timer <- timer + 1
    }
    myDF[i,timer + 2] <- substr(myDF[i,2], 25000*(timer - 1), 25000 * timer)
    charnum <- charnum - 25000
    timer <- timer + 1
  }
  
  ## TRANSLATE  ##
  for(i in 1:nrow(myDF)) {  # iterate through files
    for (k in 3:ncol(myDF)){
      if (is.na(myDF[i,k]) == F){
        myDF[i,k] <- gl_translate(myDF[i,k], target = "en")$translatedText
        print("Okay")
      }
    }
  }
  
  myDF[,2] <- NULL 
  write.csv(myDF, as.character(paste0(f, ".csv")))
}

## END FUNCTION ##






