source("definitions2.R")
alldata<- (makeGroups( mklpdf()))

graphit2(c("San Marino","vatican","andorra",'Monaco','Malta','Liechtenstein' ),1,size=4)
WestvsEast<- c("Italy","Iran","Korea","Germany","France, France","Spain","Norway","Hubei","Belgium","Netherlands","Singapore","Japan","Shanghai","denmark")

#for publication
graphit2(WestvsEast,500,size=3,legend=TRUE,savename="West vs East")
graphit2(WestvsEast,10,logy=FALSE,size=3,legend=TRUE,savename="West.vs East")
graphit2(MENA,20,varnames=c("confirmed","recovered"), size=1,needfuzzy=FALSE)
graphit2(c(Africa),3,size=2,varnames="confirmed",size=3,legend=TRUE,savename="Africa")
graphit2(c(Africa,"Japan","Italy"),3,size=2,varnames="confirmed",size=3,legend=TRUE,savename="Africa+Japan+Italy")
graphit2(smallEurope,50,size=2,varnames=c("confirmed","deaths"),size=3,legend=TRUE,savename="Europe, less touched countries" )
smallEurope <- c("Poland","Belgium","Netherlands","Austria","Romani","Hunga","Ireland","Sweden","Denmark","Finland","Bulgaria","Portugal","Greece","Croatia","Slovakia","Slovenia","Czechia","Estonia","Lithuania","Latvia","Malta","Luxembourg","Cyprus","United K","Swit","Norway","Iceland")
graphit2("Australia",10,size=2,legend=TRUE,savename="Australia" )
graphit(SAmerica,10,size=3,legend=TRUE,savename="South America" )
graphit2(c(", US"),100,varname=c("confirmed"), ID="CRPS",needfuzzy=TRUE,legend=TRUE,savename="USA states") #,"deaths","recovered"
graphit(c(", US"),100,varname=c("confirmed"), ID="CRPS",needfuzzy=TRUE,legend=TRUE,savename="USA states") #,"deaths","recovered"
graphit2(c(", US","Hubei"),100,varname=c("confirmed"),size=.7,needfuzzy=TRUE)

graphit2(c("France", "Belgium","Netherlands","Italy","Germany","United Kingdom"),minval=100, size=2,ID="Country.Region", varnames=c("confirmed" , "deaths"))#,logy=FALSE)
#####


graphit2(EU,50,logy=FALSE)
graphit2(EU,100,size=3,varnames=c("confirmed","deaths"))
graphit2(EU,500,varnames="confirmed",size=2)
graphit(EU,500,varname="confirmed",size=2)
graphit2(EU,10,varnames="deaths")
graphit2(EU,20,varnames="recovered")
graphit2(EU,0,varnames="recoveredOverDead",logy=FALSE)
graphit2(EU,0,varnames="recoveredOverConfirmed",logy=FALSE)
graphit2(smallEurope,50,size=2,varnames=c("confirmed","deaths"))
graphit2(c("Germany","France","Italy","Netherlands","Belgium","Japan", "Korea, South"),ID="Country.Region", varnames=c("recovered","deaths"),size=2)#,logy=FALSE)

graphit2(c("Netherlands, Ne","Belg", "France, Fra","Germany","Italy"),100,size=2)
graphit2("France",varnames=c("confirmed","deaths","recovered"))
lpdf=totals(rows="France", id="Country.Region")
names(lpdf)
lpdf <- addcounterfrommin(10,
                          datasel("France",minval=0,id=, lpdf=lpdf,varname=varnames[1],fuzzy=FALSE)
                          ,id=ID,counter=countname)
graphit2("France",100)
graphit2("France",100,lpdf=lpdf,ID="Country.Region")
graphit2("France",ID="Country.Region",lpdf=totals(rows="France", id="Country.Region"))
graphit2(c("Belgium","Netherlands"),minval=100,varnames=c("confirmed","deaths"),ID="CRPS",needfuzzy=TRUE)

graphit2(testcountries, ID="Country.Region",varnames=c("confirmed","deaths","recovered"),needfuzzy=TRUE)
graphit2(EU,minval=100,varnames="confirmed",logy=FALSE,size=2)
graphit2(Europe,minval=100,varnames="confirmed",logy=FALSE,size=2)

graphit2(WestvsEast,100,size=1,varnames="confirmed")
graphit2(CIS,10,varnames=c("confirmed"))




graphit2(WestvsEast,0,size=1,varnames=c("confirmed"))
graphit2(WestvsEast,0,size=1,varnames=c("confirmed", "deaths"))
graphit2(WestvsEast,100,size=1,varnames=c("confirmed", "recovered"))

graphit2(CAsia,10,size=2)

graphit2("china", 1000,size=1)
graphit2("china", 1000,size=2)

graphit("china",0,varname="recoveredOverDeaths")
graphit2("china",10,varname="recovered", logy=FALSE)
names(alldata)
graphit("china",0,varname="recoveredOverConfirmed",logy=FALSE,legend=TRUE)
graphit(EU,0.25,varname="recoveredOverConfirmed",logy=FALSE,legend=TRUE)
####### Canada
graphit2(c("Canada"),20)


##### USA

graphit2(c("US","China"),10,varname=c("confirmed"), ID="Country.Region",size=.5,needfuzzy=FALSE) #,"deaths","recovered"
graphit2(c(", US","Hubei"),100,varname=c("confirmed","deaths","recovered"),size=.7,needfuzzy=TRUE)
names(totals(rows="US", id="Country.Region"))
graphit2("California", minval=1, lpdf=totals(rows="California", id="State"), ID="State")
graphit2(c("California","New York"),lpdf=totals(rows="US", id="CRPS"),ID="State")
graphit2("US",ID="Country.Region",lpdf=totals(rows="US", id="Country.Region"))


#findIDnames(SAsiaIO)
graphit2(SAsiaIO,8, size=2)
graphit2(CIS,10,varnames=c("confirmed"))
graphit2(MENA,20,varnames=c("confirmed","recovered"), size=1,needfuzzy=FALSE)
graphit2(c(Africa),3,size=2,varnames="confirmed")
graphit2(c(Africa,"Japan","Italy"),3,size=2,varnames="confirmed")
graphit2("Italy" , minval=500)
graphit2("Italy" ,minval=10, varname="deaths")
graphit2("Italy" ,minval=500, varname="recovered")
graphit2(CIS,minval=10)
