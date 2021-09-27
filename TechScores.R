rm(list = ls())

library(quanteda)
library(dplyr)
library(stringi)
library(stringr)
library(hash)
library(rgl)
library(scatterplot3d)
setwd("C:\\Users\\rzava\\Box Sync\\Tech Saliency")

## Read in weighted DFM from preprocessing and dimension infor from Adding dimensions
wDfm <- read.csv("weighted_dfm.csv")
partyDims <- read.csv("partyDimensions.csv")
rownames(wDfm) <- wDfm$doc_id
wDfm <- wDfm[,3:ncol(wDfm)]
rownames(wDfm)[132] <- "11PIR_2020"


## First, let's rename the rows to make interpretation easier
# Next we will line those codes up with CHES data for ID and Pop scores
# Method: dictionary for each country, key code off of country number, then 
# translate to party name, then append w/ year

## Start w/ country dics

# 82 --> CZ

CZCodes <-c(  "110", "220", "320", "413",   "430",     "523",     "530",   "610",  "710",     "720",   "721", "952", "953") 
CZNames <-c("Green", "KSCM", "CSSD", "ODS", "ANO2011", "KDU-CSL", "TOP09", "STAN", "SPR-RSC", "USVIT", "SPD", "VV",  "Pirates")
CzechDic <- hash()
.set(CzechDic, keys = CZCodes, values = CZNames)

# 41 --> DE

DECodes <-c(  "113", "221", "222",   "223",   "320", "420", "521", "952",       "953") 
DENames <-c("GRUNEN", "CSU", "LINKE", "LINKE", "SPD", "FDP", "CDU", "Piraten",  "AfD")
DeuchDic <- hash()
.set(DeuchDic, keys = DECodes, values = DENames)

# 15 --> IC

ICCodes <-c(  "111", "328", "420", "430", "440", "450", "620", "810", "952", "953") 
ICNames <-c("Graen", "Sam", "FF",  "Bo",  "BF",  "V",   "Sj",  "F",   "Pi",  "FIF")
IclndDic <- hash()
.set(IclndDic, keys = ICCodes, values = ICNames)            


# 11 --> SW

SWCodes <-c(  "110", "220", "320", "420", "520", "620", "710", "810", "PIR") 
SWNames <-c("Green", "SKP", "SAP", "FP/L",  "KD",  "MP", "SD",  "C",   "PIRAT")
SwednDic <- hash()
.set(SwednDic, keys = SWCodes, values = SWNames)  

## Setup Family dics

PartyCodes <-c(  #CZ
                 "CZ_KSCM", "CZ_CSSD", "CZ_ODS", "CZ_ANO2011", "CZ_KDU-CSL", "CZ_TOP09", 
                 "CZ_STAN", "CZ_SPD", "CZ_Pirates", "CZ_Green", "CZ_SPR-RSC", "CZ_USVIT",
                 "CZ_VV",
                 #DE
                 "DE_GRUNEN", "DE_CSU", "DE_LINKE", "DE_SPD", "DE_FDP", "DE_CDU", 
                 "DE_AfD","DE_Piraten", 
                 #IC
                 "IC_Graen", "IC_Sam", "IC_V", "IC_Sj", "IC_F", "IC_Pi", "IC_FIF",
                 "IC_FF", "IC_Bo",
                 #SW
                 "SW_SAP", "SW_FP/L", "SW_KD", "SW_MP", "SW_SD", "SW_C") 

PartyFams  <-c(  #CZ
                 "Communist", "Soc Dem", "Conservative", "Liberal", "Christ Dem",  
                 "Conservative", "Liberal", "Soc Dem", "Pirate", "Green", "Far Right", 
                 "Far Right", "Liberal",
                 #DE
                 "Green", "Christ Dem", "Communist", "Soc Dem", "Liberal", "Christ Dem", 
                 "Far Right", "Pirate",
                 #IC
                 "Green", "Soc Dem", "Conservative", "Conservative", "Agrarian",
                 "Pirate", "Communist", "Liberal", "Conservative",
                 #SW
                 "Soc Dem", "Liberal", "Christ Dem", "Green", "Far Right", "Agrarian")


partyDic <- hash()
.set(partyDic, keys = PartyCodes, values = PartyFams) 

## Rename rows
 # subset to parties containing Pi
 # average each wt into new vector
 # sort in desc order, pick top 20 words
 # We'll say tech score is avg weight of those 20 words
 # Gonna do crappy for loop because fuck I can't think rn
 # Unconsolidated if/else statements to make it more readable, but longer



recoded <- NULL
for (i in 1:nrow(wDfm)){
  r <- rownames(wDfm)[i]
  if (substr(r, 1, 2) == '82'){
    # Code Structure: "Country_Party_Year"
    recoded[i] <- paste("CZ_", CzechDic[[substr(r, 3,5)]], substr(r, 6, 10), sep = "")
  }
  else if (substr(r, 1, 2) == '41'){
    recoded[i] <- paste("DE_", DeuchDic[[substr(r, 3,5)]], substr(r, 6, 10), sep = "")
  }
  else if (substr(r, 1, 2) == '15'){
    recoded[i] <- paste("IC_", IclndDic[[substr(r, 3,5)]], substr(r, 6, 10), sep = "")
  }
  else if (substr(r, 1, 2) == '11'){
    recoded[i] <- paste("SW_", SwednDic[[substr(r, 3,5)]], substr(r, 6, 10), sep = "")
  }
}

rownames(wDfm) <- recoded


## Need to average word weights across parties
 # Find highest weighted words for pirates
 # Start by subsetting to wdf w/ only pirates

piWT <- t(wDfm[which(substr(rownames(wDfm), 4,6) == "Pi_"),])
Means <- sort(rowMeans(piWT, na.rm = T), decreasing = T)
Means <- as.data.frame(Means)
bigwords <- rownames(Means)[1:20]
bigwords
## Manually checking the list.  Removing 'pirat' and 'iceland' as inaccurate proxies
bigwords <- bigwords[which(bigwords != "pirat" & bigwords != "iceland")]

## Now subset the dfm to tech words, average, and shaboom -- tech score
techWords <- wDfm[,which(colnames(wDfm) %in% bigwords)]
techWords$tech_score <- rowMeans(techWords, na.rm = T)

#### NEXT STEPS ####
 #   coordinate partyDims scores w/ corresponding party tech scores
 #   fuck em up with the figure

pop_score <- NULL
id_score  <- NULL
family    <- NULL
for (i in 1:nrow(techWords)) {
  pName <- regexpr("_.*_", rownames(techWords)[i])
  ## Placeholders for extracted country code and party name
  hCou <- substr(rownames(techWords)[i], 1,2)
  hPar <- regmatches(rownames(techWords)[i], pName)
  hPar <- substr(hPar, 2, nchar(hPar) - 1)
  ## Now extracting 2dim info
  Loc <- which(partyDims$country == hCou & partyDims$party_name == hPar)
  family[i] <- partyDic[[paste0(hCou, "_", hPar)]]
  if (is.na(Loc[1] == T)){
    pop_score[i] <- NA
    id_score[i]  <- NA
  }
  else{
    pop_score[i] <- partyDims$pop_score[Loc]
    id_score[i]  <- partyDims$id_score[Loc] 
  }
}

## Techwords now the complete doc with all scores
techWords$pop_score <- pop_score
techWords$id_score  <- id_score
techWords$family    <- family
techWords <- techWords[complete.cases(techWords),]



##  PLOT  ##
# Show order of parties to pick which color
unique(sort(family))
colors <- c( "tan4", "gold", "mediumaquamarine", "orange3", "red3", "green4", "dodgerblue", "purple4",  "mediumorchid2")
color <- colors[as.factor(techWords$family)]

#plot
plot3d(x=techWords$id_score, y=techWords$tech_score, z=techWords$pop_score, 
        col = color,
        type = 's', 
        radius = .1,
        xlab="Ideological", ylab="Tech", zlab="Populist",
        main="Party Scores Across 3 Dimensions")
#legend("right", legend = techWords$family,
#       col =  color)

## Show me which colors go to which party
cbind(unique(techWords$family), unique(color))

