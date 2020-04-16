source("loadData.R")  
#this loads the requirements and the definitions also. 

#for publication #MSM,Vincent,
graphRegiolist <- list(WestvsEast,bigEarth, middleEarth, smallEarth,
                       microEarth,bigEurope,middleEurope,smallEurope,microEurope,
                       NAmerica,USS, SouthWestAsia,SouthEastAsia, MENA,
                       Africa,SAsiaIO,EastAsia,CIS,SAmerica,Caribbean,Oceania,
                       ChinaP,bigThree,"World", CanadaP)
# yesterday's graphs: 
makehistory(Sys.Date()-1,Sys.Date()-1)  #bug graph 9 crashes when recovery not present at all. (CanadaP)
#
#makehistory(Sys.Date()-10,Sys.Date()-1,regioList=list(bigEarth,smallEarth,middleEarth,microEarth))

#makehistory()
