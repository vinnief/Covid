source("definitions2.R")
alldata<- (makeGroups( mklpdf()))
writewithcounters(alldata)
graphit2(c("San Marino","vatican","andorra",'Monaco','Malta','Liechtenstein' ),1,size=4)

#for publication
graphit3(WestvsEast,500,savename="West vs East")
graphit3(WestvsEast,100,varnames="deaths",logy=FALSE,savename="West vs East")
graphit3("Belgium",100,varnames=c("confirmed","recovered","deaths"),logx=TRUE,savename="Belgium")
graphit3(EU,100,savename="EU")
graphit3(EU,500,varnames=c("confirmed","deaths"),savename="EU")
graphit3(EU,100,varnames=c("deaths"),savename="EU")
graphit3(smallEurope,50,varnames=c("confirmed","deaths"),savename="Europe, less touched countries" )
graphit3(MENA,20,varnames=c("confirmed","recovered"), size=1,needfuzzy=FALSE,savename="MENA")
graphit3(c(Africa),100,size=3,varnames="confirmed",savename="Africa")
graphit3(c(Africa),10,size=3,varnames="confirmed",savename="Africa")
graphit3(c(Africa,"Japan","Italy"),3,varnames="confirmed",savename="Africa+Japan+Italy")
graphit3(SAsiaIO,50,savename="South Asia and Indian Ocean")
graphit3(c("italy",SouthEastAsia),50,varnames=c("confirmed","recovered","deaths"),savename="South East Asia")
graphit3(CAsia,100,savename="West and Central ASia")

graphit3(CIS,10,varnames=c("confirmed"))
graphit3(CIS,10,savename="CIS and Georgia(s)")
graphit3("Australia",10,savename="Australia" )
graphit3(SAmerica,10,size=3,savename="South America" )
graphit3("US",needfuzzy=FALSE)

#############
graphit2(c(", US"),100,varname=c("confirmed"), ID="CRPS",needfuzzy=TRUE,legend=TRUE,savename="USA states") #,"deaths","recovered"
graphit(c(", US"),100,varname=c("confirmed"), ID="CRPS",needfuzzy=TRUE,legend=TRUE,savename="USA states") #,"deaths","recovered"
graphit2(c(", US","Hubei"),100,varname=c("confirmed"),size=.7,needfuzzy=TRUE)

graphit2(c("France", "Belgium","Netherlands","Italy","Germany","United Kingdom"),minval=100, size=2,ID="Country.Region", varnames=c("confirmed" , "deaths"))#,logy=FALSE)
##### scatter plot of two vars: 

graphit3(EU,logy=FALSE,logx=FALSE)
graphit3(EU,xvar="new_confirmed", varnames=c("new_recovered"),logy=TRUE,logx=TRUE)

graphit3(SouthEastAsia,varnames=c("confirmed","recovered"),logy=FALSE)
graphit3(SouthEastAsia,xvar="confirmed",varnames=c("deaths","recovered"),logy=FALSE,logx=FALSE)
graphit3(c("italy",SouthEastAsia),xvar="confirmed",varnames=c("deaths"),logy=TRUE,logx=TRUE,savename="Italy & SE Asia")
##############################################
graphit2(testcountries, ID="Country.Region",varnames=c("confirmed","deaths","recovered"),needfuzzy=TRUE)





graphit2("china", 100)

graphit("china",0,varname="recoveredOverDeaths")
graphit2("china",10,varname="recovered", logy=FALSE)
names(alldata)
graphit2("china",0,varname="recoveredOverConfirmed",logy=FALSE,legend=TRUE)
graphit2(EU,0.25,varname="recoveredOverConfirmed",logy=FALSE,legend=TRUE)
####### Canada
graphit2(c("Canada"),20)

##### USA

graphit2(c("US","China"),10,varname=c("confirmed"), ID="Country.Region",size=.5,needfuzzy=FALSE) #,"deaths","recovered"
graphit2(c(", US","Hubei"),100,varname=c("confirmed","deaths","recovered"),size=.7,needfuzzy=TRUE)
names(totals(rows="US", id="Country.Region"))
graphit2("California", minval=1, lpdf=totals(rows="California", id="State"), ID="State")
graphit2(c("California","New York"),lpdf=totals(rows="US", id="CRPS"),ID="State")
graphit2("US",ID="Country.Region",lpdf=totals(rows="US", id="Country.Region"))


graphit2("Italy" , minval=500)
graphit2("Italy" ,minval=10, varname="deaths")
graphit2("Italy" ,minval=500, varname="recovered")
graphit2(CIS,minval=10)
