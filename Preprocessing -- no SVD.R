## In Collaboration with Alper Gencer
rm(list = ls())

library(tidyverse)
library(quanteda)
library(stringi)
library(stringr)
library(tm)
library(manifestoR)
library(readtext)
library(corpus)
library(utils)
library(dplyr)
library(tidyr)

source("C:\\Users\\rzava\\Box Sync\\Tech Saliency\\R scripts\\GettingWarmer.R")
### newDF is DF w/ rows of each manifesto from each country.  Col1 is ID, Col 2
### is complete translated manifesto.  colNames: X1, X2

## GET RID OF WHITESPACE ##
preprocessed_data = newDF %>%
  filter(str_detect(X2, "^\\s*$")==FALSE)


## SET ENGLISH STOPWORDS ##
my_sw = stopwords()


## Clean whitespace, numbers, and punctuation, then stem the document. We also dropped the stopwords.
  ## printed strings for mile-markers bug out the function, commented out for now
preprocessed_data_clean = preprocessed_data %>%
  # removing numbers
  mutate(X2 = str_remove_all(X2, "[:digit:]")) %>%
  #print("Done with numbers") %>%
  # remove punctuation 
  mutate(X2 = str_remove_all(X2, "[:punct:]")) %>%
  #print("Done with punctuation") %>%
  # making all lowercase 
  mutate(X2 = quanteda::char_tolower(X2)) %>%
  #print("Done with lower") %>%
  # remove all stopwords
  mutate(X2 = removeWords(X2, my_sw)) %>%
  #print("Done with stopwords") %>%
  # cleaning extra whitespace
  mutate(X2 = str_squish(X2)) #%>%
  #print("Done with squish")


  ### Reduce the size of the DTM by removing words that only occur in three or fewer documents. 
  # Let's see the first example:
  head(preprocessed_data_clean$X2)[1] # thankfuck
  
  
  # Let's create a seperate list:
  preprocessed_text <- corpus(preprocessed_data_clean$X2)
  # While creating the feature matrix, let's also stem the text:
  my_dfm <- dfm(preprocessed_text, stem = TRUE)
  
  # Let's remove words that only occur thrice or less
    ## NOT INCLUDED HERE BECAUSE SMALLER DOCUMENT POOL ##
  # my_dfm <- dfm_trim(my_dfm, min_docfreq = 4, verbose=TRUE)
  
  ### Finally, normalize each row so that it sums to unity, to help account for 
  # different document lengths. (Why?)
  # Let's weight our dfm:
  my_dfm <- dfm_weight(my_dfm, scheme = "prop")
  
  # Now standardize doc names for reference
  rownames(my_dfm) <- preprocessed_data$X1
  rownames(my_dfm)
  
  setwd("C:\\Users\\rzava\\Box Sync\\Tech Saliency")
  write.csv(convert(my_dfm, to = "data.frame"), "weighted_dfm.csv")

  
  
