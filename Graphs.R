tryCatch(source("loadData.R"),
error=function(e){
   source("definitions2.R")
  source("loadData.R")})


#for publication
graphRegiolist <- list(MSM,Vincent,WestvsEast,bigEurope,smallEurope,NAmericaS, SouthWestAsia,SouthEastAsia, MENA,Africa,SAsiaIO,EastAsia,CIS,SAmerica,Caribbean,OceaniaP,ChinaP)

writeRegiograph(graphRegiolist,nrs=3)  #c(1,2,3,4,5,6,7))
if(!is.null(dev.list())) dev.off() #sometimes the device hangs and we cannot access the file outside R
if(!is.null(dev.list())) dev.off()
graphs(USS,"US states")
graphs("Europe","Europe", lpdf=Eurotots)

graphs("US","US",lpdf=UStots,nrs=1:3)
graphs("World","World",id="CRPS",lpdf= Worldtots)
graphs(c("Europe","US","China","Canada","Australia"),"Biggies",id="CRPS",lpdf=regtots)

##################tests
