#Collect datasets and analyze

rm(list=ls())

#housekeeping
setwd("/Users/robertthompson/Desktop/Comp Sci/Fantasy Football Draft 2018")
library(dplyr)
library(readxl)
library(tidyr)

#download leaders
x<-2007:2017
for (i in x) {
  year<-toString(i)
  name<-paste("leaders",year)
  path<-paste("fantasy-football-leaders-", year,".csv", sep ="")
  eval(call("<-", as.name(name),tbl_df(read.csv(toString(path) ))))
}

#download ADPs
x<-2007:2017
for (i in x) {
  year<-toString(i)
  name<-paste("ADP",year)
  path<-paste("Fantasy ADP ", year,".xlsx", sep ="")
  eval(call("<-", as.name(name),read_excel(toString(path) )))
}

#Remove kickers, defenses, QBs (ADPs)
x<-2007:2017
for (i in x) {
  year<-toString(i)
  name<-paste("ADP",year)
  eval(call("<-", as.name("temp"),as.name(name)))
  temp<-filter(temp,!(Position=="PK"|Position=="DEF" | Position=="QB"))
  eval(call("<-", as.name(name),temp ))
}

#Remove QBs (leaders)
for (i in x) {
  year<-toString(i)
  name<-paste("leaders",year)
  eval(call("<-", as.name("temp"),as.name(name)))
  temp<-filter(temp,!(Position=="QB"))
  eval(call("<-", as.name(name),temp))
}

#Add Half PPR (probably isn't necessary)
for (i in x) {
  year<-toString(i)
  name<-paste("leaders",year)
  eval(call("<-", as.name("temp"),as.name(name)))
  temp<-mutate(temp,HalfPPRScore=PassingTouchdowns*4 + RushingYards/10 + RushingTouchdowns*6 + Receptions/2 + ReceivingYards/10 + ReceivingTouchdowns *6 - FumblesLost *2)
  eval(call("<-", as.name(name),temp))
}

#change ADP to just rank, average, name, team
for (i in x) {
  year<-toString(i)
  name<-paste("ADP",year)
  eval(call("<-", as.name("temp"),as.name(name)))
  temp<-select(temp, Name, Rank, ADP, Team)
  eval(call("<-", as.name(name),temp ))
}

#change leaders to just name, score, 1/2PPR score, position, team
for (i in x) {
  year<-toString(i)
  name<-paste("leaders",year)
  eval(call("<-", as.name("temp"),as.name(name)))
  temp<-select(temp, "Name", "Position", "FantasyPoints", "HalfPPRScore", "Team")
  eval(call("<-", as.name(name),temp ))
}

# Get the dataset for a given year. Should have written this a lot sooner!
getProfileYear <- function (yearTemp1)
{
  yearString<-toString(yearTemp1)
  name<-paste("Player Profiles",yearString)
  eval(call("<-",as.name("functionTemp"),as.name(name)))
  return(functionTemp)
}

replaceProfileYear <- function (yearTemp3,tempDataSet)
{
  temp<-tempDataSet
  year<-toString(yearTemp3)
  name<-paste("Player Profiles",year)
  eval(call("<-",as.name(name),as.name("temp")))
}

# create one dataset with ADPs and points
for (i in x) {
  year<-toString(i)
  name1<-paste("leaders",year)
  name2<-paste("ADP",year)
  name3<-paste("Player Profiles",year)
  eval(call("<-", as.name("temp1"),as.name(name1)))
  eval(call("<-", as.name("temp2"),as.name(name2)))
  temp1<-mutate(temp1, mergecol=paste(Name,Team))
  temp2<-mutate(temp2, mergecol=paste(Name,Team))
  temp3<-merge(temp1,temp2, by.x="mergecol",by.y="mergecol")
  temp3<-select(temp3, Name=Name.x, Position, Team=Team.x, FantasyPoints, HalfPPRScore, ADPRank=Rank, ADPAvg=ADP)
  temp3<-arrange(temp3,ADPRank)
  eval(call("<-", as.name(name3),temp3 ))
  #rm(name1, name2)
}

#add column for Score rank in PPR, Standard
for (i in x) {
  year<-toString(i)
  name<-paste("Player Profiles",year)
  eval(call("<-", as.name("temp"),as.name(name)))
  temp<-arrange(temp,desc(HalfPPRScore))
  temp<-mutate(temp,HalfPPRRank=HalfPPRScore, STDRank=FantasyPoints)
  for (j in 1:137)
  {
    if (j>length(temp$Name))
      break
    temp$HalfPPRRank[j]<-j
    
  }
  temp<-arrange(temp,desc(FantasyPoints))
  for (j in 1:137)
  {
    if (j>length(temp$Name))
      break
    temp$STDRank[j]<-j
    
  }
  eval(call("<-", as.name(name),temp))
}

 #remove superfluous tibbles, values
#rm(list=ls()[c(1:26,39:43)])



#find average values of picks, but stored in a vector
x<-2007:2017
y<-1:137
totalscorestd<-1:200 - 1:200
totalscorehppr<-1:200 - 1:200
numoberservations<-1:200 - 1:200
for (i in x)
{
  name2<-paste("Player Profiles",i)
  eval(call("<-",as.name("temp"),as.name(name2)))
  temp<-arrange(temp,ADPRank)
  for (j in y)
  {
    if (length(temp$Name)<j)
    {
      break
    }
    rank<-temp$ADPRank[j]
    totalscorestd[rank]<-totalscorestd[rank]+temp$FantasyPoints[j]
    totalscorehppr[rank]<-totalscorehppr[rank]+temp$HalfPPRScore[j]
    numoberservations[rank]<-numoberservations[rank]+1
  }
}
AvgPickValueStd<-totalscorestd/numoberservations
AvgPickValueHPPR<-totalscorehppr/numoberservations


#Remove ranks with low observation counts
copiednumoberservations<-numoberservations
x<-1:202
for (i in x)
{
  if (is.na(numoberservations[i])| numoberservations[i]<3)
  {
    AvgPickValueStd[i]<-NA
    copiednumoberservations[i]<-NA
  }
}

copiednumoberservations <-copiednumoberservations/copiednumoberservations*1:202

#Create linear regressed model for pick values
line<-line(copiednumoberservations,AvgPickValueStd)
intercept<-line$coefficients[1]
slope<-line$coefficients[2]
LinearRegressedPickValue<-1:202*slope+intercept

#find average values of picks, but stored in a vector, and averaged with two adjacent values
x<-2:199
y<- -1:1
AvgPickValueSmoothedStd<-1:200-1:200
AvgPickValueSmoothedHPPR<-1:200-1:200
for (i in x) 
{
  tempHPPR <- 0
  tempSTD <- 0
  for (j in y)
  {
    if (!is.na(AvgPickValueHPPR[i+j]))
    {
      AvgPickValueSmoothedHPPR[i]=AvgPickValueSmoothedHPPR[i]+AvgPickValueHPPR[i+j]
    }
    if (!is.na(AvgPickValueStd[i+j]))
    {
      AvgPickValueSmoothedStd[i]=AvgPickValueSmoothedStd[i]+AvgPickValueStd[i+j]
    }
  }
}
AvgPickValueSmoothedHPPR=AvgPickValueSmoothedHPPR/3
AvgPickValueSmoothedStd=AvgPickValueSmoothedStd/3
AvgPickValueSmoothedHPPR[1]=AvgPickValueHPPR[1]
AvgPickValueSmoothedStd[1]=AvgPickValueStd[1]

#add a points above expected column to every data table
x<-2007:2017
y<-1:137
for (i in x) 
{
  temp<-getProfileYear(i)
  temp<-mutate(temp, ValueAboveAverageStd = 0)
  for (j in y)
  {
    if (length(temp$Name)<j)
    {
      break
    }
    rowFantasyPoints = temp[j,]$FantasyPoints
    rowExpectedFantasyPoints = AvgPickValueSmoothedStd[temp[j,]$ADPRank]
    temp[j,]$ValueAboveAverageStd = rowFantasyPoints - rowExpectedFantasyPoints
  }
  year<-toString(i)
  name<-paste("Player Profiles",year)
  eval(call("<-",as.name(name),as.name("temp")))
}

#Find outliers
x<-2007:2017
y<-1:136
studs <-"studs:"
breakouts <- "breakouts:"
busts <- "busts:"
valuePicks <- "value picks:"
for (i in x)
{
  name<-paste("Player Profiles",i)
  eval(call("<-",as.name("temp"),as.name(name)))
  temp<-arrange(temp,ADPRank)
  for (j in y)
  {
    if (length(temp$Name)<j)
    {
      break
    }
    STDscore <- temp$FantasyPoints[j]
    HPPRscore <- temp$HalfPPRScore[j]
    rank <- temp$ADPRank[j]
    valueAboveExpected <- temp$ValueAboveAverageStd[j]
    expected1 <- LinearRegressedPickValue[rank]
    expected2 <- AvgPickValueStd[rank]
    expected3 <- AvgPickValueSmoothedHPPR
    expected4 <- AvgPickValueSmoothedStd
    if (HPPRscore>300)
    {
      profile<-paste(temp$Name[j],i,temp$Position[j],"score:",STDscore, "ADP:", rank, "valueAboveExpected:", valueAboveExpected)
      studs<-c(studs,profile)
    }
    if (HPPRscore>250 & rank>50)
    {
      profile<-paste(temp$Name[j],i,temp$Position[j],"score:",STDscore, "ADP:", rank, "valueAboveExpected:", valueAboveExpected)  
      breakouts<-c(breakouts,profile)
    }
    if (STDscore<70 & j<21 | valueAboveExpected< -100)
    {
      profile<-paste(temp$Name[j],i,temp$Position[j],"score:",STDscore, "ADP:", rank, "valueAboveExpected:", valueAboveExpected)   
      busts<-c(busts,profile)
    }
    if(valueAboveExpected>100)
    {
      profile<-paste(temp$Name[j],i,temp$Position[j],"score:",STDscore, "ADP:", rank, "valueAboveExpected:", valueAboveExpected)
      valuePicks<-c(valuePicks,profile)
    }
  }
}






# find a player's season given a name and a year
findProfile <-function(name, yearTemp2)
{
  temp<-getProfileYear(yearTemp2)
  x<-1:137
  for (i in x)
  {
    if (temp[i,]$Name == name)
    {
      return (temp[i,])
    }
  }
}



print("Player Profiles have most of what you'll need")
print("AvgPickValueStd stores avg production in Standard Scoring")
print("LinearRegressedPickValue is a linear regressed model for what each pick should return, in Standard Scoring. A far from perfect model (middles rounds vastly overestimated)")
print("AvgPickValueSmoothed Std and HPPR simply take average value of a pick and two adjacents. My best estimate")
print("outliers shows every player who scored at least 300 HPPR fantasy points")
print("breakouts has the profiles of players of ADP 50 or lower who scored 250 HPPR fantasy points")
print("busts shows every player in the top 20 who score less than 70 standard points")
print("valuePicks are players who scored at least 100 points better than expected")

