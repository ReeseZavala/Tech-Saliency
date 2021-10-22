library(ggplot2)
library(readr)
library(dplyr)

setwd("C:/Users/rzava/Box Sync/Tech Saliency/")
DiD <- read.csv("DiD_Scores.csv")

## First split dataset into pre/post treated/untreated
preT <- DiD %>%
  filter(Treated == 1, Period == 0)
postT<- DiD %>%
  filter(Treated == 1, Period == 1)
preU <- DiD %>%
  filter(Treated == 0, Period == 0)
postU<- DiD %>%
  filter(Treated == 0, Period == 1)

p = ggplot() + 
  geom_jitter(aes(DiD$Year[DiD$Treated == 1],DiD$Score[DiD$Treated == 1]), colour="Green") + 
  geom_jitter(aes(DiD$Year[DiD$Treated == 0],DiD$Score[DiD$Treated == 0]), colour="Black") + 
  geom_smooth(aes(preT$Year,preT$Score), method=lm, se=FALSE, color = "Green") +
  geom_smooth(aes(postT$Year,postT$Score), method=lm, se=FALSE, color = "Green") +
  geom_smooth(aes(preU$Year,preU$Score), method=lm, se=FALSE, color = "Black") +
  geom_smooth(aes(postU$Year,postU$Score), method=lm, se=FALSE, color = "Black") +
  geom_vline(xintercept=c(2009), linetype="dotted") +
  labs(x = "Year", y = "Tech Score") 

p + theme(legend.position = c(2001, .008))


