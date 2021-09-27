rm(list = ls())

library(quanteda)
library(dplyr)
library(stringi)
library(stringr)
library(hash)
setwd("C:\\Users\\rzava\\Box Sync\\Tech Saliency")

## CHES Scores for later
CHES <- read.csv("CHES2019.csv")
CHES10 <- read.csv("CHES2010.csv")

## Read in weighted DFM from preprocessing
 # not needed in this script
 # wDfm <- read.csv("weighted_dfm.csv")
## readjust wdfm so rownames are codes and all columns are words
# rownames(wDfm) <- wDfm[,2]
# rownames(wDfm)[132] <- "11PIR_2020"
# wDfm <- wDfm[,3:ncol(wDfm)]


## Coalesce CHES data
            
partyDims <- CHES %>%
  filter(nchar(party_id) == 3 & substr(party_id, 1, 1) == '3' |
         nchar(party_id) == 4 & substr(party_id, 1, 2) %in% c("16", "21", "45")) %>%
  mutate(country = ifelse(substr(party_id, 1,1) == "3", "DE",
                    ifelse(substr(party_id, 1,2) == "16", "SW",
                           ifelse(substr(party_id, 1,2) == "21", "CZ", "IC")))) 

## Cite for populism score
 # https://www.pewresearch.org/global/2021/01/19/us-image-fall-2020-appendix-classifying-european-political-parties/

## Empty columns for aggregated populist and Ideology scores
partyDims$pop_score <- rep(0, nrow(partyDims))
partyDims$id_score  <- rep(0, nrow(partyDims))

for (c in c("CZ", "DE", "SW", "IC")) {
  hldpls <- NULL
  hldpls <- partyDims[which(partyDims$country == c), ]
  for (i in unique(hldpls$party_id)){
    ## Temp variable to hold the locations    
    idLocs <- NULL
    pop <- NULL
    id <- NULL
    
    idLocs <- which(partyDims$party_id == i)
    hldlil <- hldpls[which(hldpls$party_id == i), ] 
    ## populist score from pew, averaging across experts
    pop <- mean(c(mean(hldlil$people_vs_elite, na.rm = T), mean(hldlil$antielite_salience, na.rm = T)))
    ## ideology score, averaging across experts
    id  <- mean(hldlil$lrgen, na.rm = T)
    partyDims$pop_score[idLocs] <- rep(pop, length(idLocs))
    partyDims$id_score[idLocs]  <- rep(id, length(idLocs))
    }
}

partyDims <- partyDims[which(partyDims$id == 1),]
## Note that the Swedish Pirates are not included in the 2019 CHES survey, appending data from the 2010
 # survey.  However, 2010 does not include the same variables for calculating populism, so only using lrgen


mean(CHES$lrgen[which(CHES$party_name == "Piraten")], na.rm = T)


SwPi <- CHES10 %>%
  filter(party_id == 1611) %>%
  mutate(id_score = mean(lrgen, na.rm = T)) %>%
  mutate(pop_score = NA) %>%
  filter(expert_id == 1) %>%
  mutate(country = "SW")
SwPi <- SwPi[1, which(colnames(SwPi) %in% c('country', 'party_name', 'party_id', 'pop_score', 'id_score'))]

##Completing the table, should have country info, party name, party code, and 2 dim score
partyDims_full <- cbind(partyDims$country, partyDims$party_name, partyDims$party_id, partyDims$id_score, partyDims$pop_score)
colnames(partyDims_full) <- c('country', 'party_name', 'party_id', 'id_score', 'pop_score')
partyDims_full <- rbind(partyDims_full, SwPi)

write.csv(partyDims_full, "partyDimensions.csv")



