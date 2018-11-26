#Collect these datasets biotch

#housekeeping
setwd("/Users/robertthompson/Desktop/Comp Sci/Fantasy Football Draft 2018")
library(dplyr)
library(readxl)
library(tidyr)

#download
TScorePPR <- read_excel("TScore PPR Ranks 8.31.18.xlsx")
TScoreSTD <- read_excel("TScore Standard Ranks 8.31.18.xlsx")
ESPNADP <- read_excel("ESPN ADP 8.31.18.xlsx")

TScorePPR<-select(TScorePPR, -POS., TEAM)
TScoreSTD<-select(TScoreSTD, -POS.) 
TScoreMerged<-merge(TScorePPR,TScoreSTD)

ESPNADP <- separate(ESPNADP,`PLAYER, TEAM`, into= c("PLAYER", "TEAM"), sep=",")
ESPNADP <- select(ESPNADP, -TEAM)
AllRanks<- merge(ESPNADP, TScoreMerged)

AllRanks<-mutate(AllRanks,TScoreAvg = (`TSCORE RANK PPR` + `TSCORE RANK STD`)/2)
AllRanks<-arrange(AllRanks, TScoreAvg)
AllRanks<-mutate(AllRanks, TScoreRound = ((TScoreAvg/12) - ((TScoreAvg/12)%%1) +1 + (TScoreAvg/12)%%1*.12))
AllRanks<-select(AllRanks, -`TSCORE RANK PPR`, -`TSCORE RANK STD`)

AllRanks<-mutate(AllRanks, LengthAvailable = `ESPN ADP RANK` - TScoreAvg)