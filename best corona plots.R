library(tidyverse)
library(gridExtra)
library(dslabs)

setGrowth= function(sick){
  growth=vector("numeric")
  growth[1]=1
  for(i in 2:length(sick)){
    growth[i]=sick[i]/sick[i-1]
  }
  growth
}

setLength= function(vec,length){
  for(i in 1:length){
    if(is.na(vec[i])){
      vec[i]=NA
    }
  }
  print(vec)
  vec
}


israelSick=c(97,109,143,193,213,298,337,433,677,705,883,1071,1442,1930,2369,2693,3035,3619
             ,4247,4695,5358,6092,6857,7428,7851,8430,8904,9248,9404,9968,10408,10743,11145,
             11586,12046,12501,12758,12982,13265,13491,13713,13942,14498,14803,15058,15298,
             15443,15555,15728,15834,15946)
lastIsrael=length(israelSick)

italySick=c(157,229,323,470,655,889,1128,1701,2036,2502,3089,3858,4636,
            5883,7375,9172,10149,12462,15113,17660,21157,24747,27980,31506,
            35713,41035,47021,53578,59138,63927,69176,74386,80589,86498,92472,
            97689,101739,105792,110574,115242,119827,124632,128948,132547,135586,
            139422,143626,147577,152271,156363,159516,162488,165155,168941,172434,175925,
            178972,181228,183957,187327,189973,192994,195351,197675,199414,201505,203591,205463)
lastItaly=length(italySick)

usaSick=c(100,124,158,221,319,435,541,704,994,1301,1630,2183,
          2770,3617,4604,6357,9317,13898,19551,24418,33840,44189,55398,
          68905,86379,105217,124788,144980,168177,193353,220295,250708,
          283477,317994,343747,375348,409225,441569,475515,509604,539942,
          567708,594693,621953,652474,682454,714822,743901,770014,798145,
          824229,854385,886274,925232,960651,987160,1010356,1035765,1064194,1095023)
lastUsa=length(usaSick)

lengths=c(length(israelSick),length(italySick),length(usaSick))
maxLength=max(lengths)

israelSick=setLength(israelSick,maxLength)
italySick=setLength(italySick,maxLength)
usaSick=setLength(usaSick,maxLength)


israelGrowth= setGrowth(israelSick)
israelAvgGrowth=mean(israelGrowth,na.rm = TRUE) 
israelSickPerMil=israelSick/8655535*1000000 #israel total cases per milion people

italyGrowth=setGrowth(italySick)
italyAvgGrowth=mean(italyGrowth,na.rm=TRUE)
italySickPerMil=italySick/60461826*1000000

usaGrowth=setGrowth(usaSick)
usaAvgGrowth=mean(usaGrowth,na.rm=TRUE)
usaSickPerMil=usaSick/331002651*1000000

avgGrowthVec=c(israelAvgGrowth,italyAvgGrowth,usaAvgGrowth)
names(avgGrowthVec)=c("Israel","Italy","USA")
avgGrowthVec
growthAvg=mean(avgGrowthVec)
growthAvg

maxGrowth=max(c(israelGrowth,usaGrowth,italyGrowth),na.rm=TRUE)
maxGrowth

#data frame to hold data for labels
countries=c("Israel","Italy","USA")
x=c(lastIsrael,lastItaly,lastUsa)
y=c(israelSick[lastIsrael],italySick[lastItaly],usaSick[lastUsa])
colors=c("blue","green","red")
names(colors)=countries
labels=data.frame(country=countries,x=x,y=y,colors=colors)

sick=data.frame(day=1:maxLength,Israel=israelSick,Italy=italySick,USA=usaSick)
sick %>% ggplot(aes(x=day))+
  xlab("Days since hundred cases")+ylab("Total Cases (logarithmic scale)")+
  ggtitle("Coronavirus cases")+
  geom_line(aes(y=Israel),color="blue")+
  geom_line(aes(y=USA),color="red")+
  geom_line(aes(y=Italy),color="green")+
  ylim(min=0,max=max(sick$Italy))+scale_y_log10()+
  geom_text(data=labels,aes(x=x+1,y=y-1,label=country),color=colors)

#change the labels to new plot
labels=mutate(labels,y=c(israelGrowth[lastIsrael],italyGrowth[lastItaly],usaGrowth[lastUsa]))
growth=data.frame(day=1:maxLength,Israel=israelGrowth,Italy=italyGrowth,USA=usaGrowth)
growth %>% ggplot(aes(x=day))+
  xlab("Days since hundred cases")+ylab("Every day's growth from the previous day")+
  ggtitle("Coronavirus Growth")+
  geom_line(aes(y=Israel),color="blue")+
  geom_line(aes(y=USA),color="red")+
  geom_line(aes(y=Italy),color="green")+
  ylim(min=1,max=maxGrowth)+
  geom_text(data=labels,aes(x=x,y=y,label=country),color=colors)

labels=mutate(labels,y=c(israelSickPerMil[lastIsrael],italySickPerMil[lastItaly],usaSickPerMil[lastUsa]))
#spm=Sick Pre Million
spm=data.frame(day=1:maxLength,Israel=israelSickPerMil,Italy=italySickPerMil,USA=usaSickPerMil)
spm %>% ggplot(aes(x=day))+
  xlab("Days since hundred cases")+ylab("Total Cases pre Million people (logarithmic scale)")+
  ggtitle("Coronavirus cases per million people")+
  geom_line(aes(y=Israel),color="blue")+
  geom_line(aes(y=USA),color="red")+
  geom_line(aes(y=Italy),color="green")+
  ylim(min=0,max=max(usaSickPerMil))+scale_y_log10()+
  geom_text(data=labels,aes(x=x+1,y=y-1,label=country),color=colors)
