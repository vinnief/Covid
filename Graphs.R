try(source("loadData.R"))
catch(e) {message(e)
   source("definitions2.R")
  source("loadData.R")}


#for publication
#USS<-setdiff(USS ,c("US","USA"))
graphRegiolist<- list(MSM,Vincent,WestvsEast,bigEurope,smallEurope,middleEurope,microEurope,USS,NAmericaS, SouthWestAsia,SouthEastAsia, MENA,Africa,SAsiaIO,EastAsia,CIS,SAmerica,Caribian,OceaniaP,ChinaP)
writeRegiographs(graphRegiolist)
graphs("Europe","Europe")

   ######
   ######
   ######
#   tryouts
writeRegiographs(list(bigEurope,middleEurope,smallEurope,microEurope))
graphs(EU,"EU")
graphs(Europe,"Europe")

graphs("Europe","Europe")
graphs (WestvsEast,"WvsE")
graphs(Benelux,"Benelux")
graphs(c(EFTA,"United Kingdom"),"EFTA plus UK")
graphs(c("USA","US"),"USA",600) #(two lines, nearly equal. one is totals of province numbers, the other is from world data)
graphs(USS,"USS",500)
graphs(NAmerica,"NAm")
graphit5("Diamond Princess",1,id="Province.State",yvars=c("confirmed",'deaths'))
### tests
### 
writeRegiographs(list(Vincent))
writeRegiographs(list(China))
writeRegiographs(list(NAmericaS))
writeRegiographs(list(NAmerica))

writeRegiographs(list(OceaniaP))
graphit5("New South Wales",100,yvars="confirmed",id="CRPS")
View(covid19[covid19$Province.State=="New York",])
View(covid19[covid19$Country.Region=="USA",])

