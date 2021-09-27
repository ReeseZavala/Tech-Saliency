
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
  
  
  
  
  
  
  
           ###  FROM CSS PROJECT  ###
  ## NOW WHAT DIMENSIONS ARE HERE? ##
  
  # Let's extract left singular vectors:
  u <- svd(my_dfm)$u
  dim(u) # See that it is a 556x556 matrix (nMan x nMan).
  # Let's extract right singular vectors:
  v <- svd(my_dfm)$v
  dim(v) # See that it's dimensions are 13032x556.
  ## WORDS LEFT IN DFM ##
  wordsWords <- colnames(my_dfm)
  
  ## wordsWords[856] == 'internet' NOW FIND INTERNET TOPICS
  ## wordsWords[4679] == 'tech' NOW FIND TECH TOPICS
  ## TECH KEYWORD ##
  bigMoney <- v[4679,][order(v[4679,], decreasing = T)][1:10]
  techTopics <- match(bigMoney, v[4679,])
  techTopics[1:5]
  
  ## INTERNET KEYWORD ##
  bigMoney <- v[856,][order(v[856,], decreasing = T)][1:10]
  techTopics <- match(bigMoney, v[856,])
  techTopics[1:5]
  
  
  for (i in techTopics) {
    print(wordsWords[match(c(v[,i][order(v[,i], decreasing = T)][1:20]), v[,i])])  
  }
  ## WE CHOOSE 2 AND 6 ##
  techTopics[c(3,6)]
  
  
  
  
  
  
  
  
  
  
  preprocessed_data$u303 <- u[,303]

  preprocessed_data$u436 <- u[,436]
  preprocessed_data[,c('X1', 'u303', 'u436')]
  
  
  
  which(wordsWords=='tech')

    ## PRETTY COLORS ##
  
  # Let's Create Color Patterns
  preprocessed_data$color_group <- "Other"
  preprocessed_data$color_group <- with(preprocessed_data, ifelse(str_detect(str_to_lower(X1), "pi"),
                                                                  "Pirate Party", color_group))

  preprocessed_data$color_group <- with(preprocessed_data, ifelse(str_detect(str_to_lower(X1), "82953"),
                                                                  "Pirate Party", color_group))
  
  preprocessed_data$color_group <- with(preprocessed_data, ifelse(str_detect(str_to_lower(X1), "41952"),
                                                                  "Pirate Party", color_group))
  
  preprocessed_data$color_group <- with(preprocessed_data, ifelse(str_detect(str_to_lower(X1), "15952"),
                                                                  "Pirate Party", color_group))
  
  
  # Let's draw our graph:
  
  ggplot(preprocessed_data, aes(u340, u436)) + 
    geom_point(data = subset(preprocessed_data, color_group == 'Other'),aes(x = u340, y = u436, color = color_group)) + 
    geom_point(data = subset(preprocessed_data, color_group == 'Pirate Party'), aes(x = u340, y = u436, color = color_group)) + 
    scale_color_manual(values=c("gray", "purple")) +
    labs(colour = "Parties") +
    ggtitle("u340 vs u436", subtitle="Highlighting Pirate Parties")
  

  
