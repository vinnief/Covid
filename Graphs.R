source("definitions2.R")
alldata<- (makeGroups( mklpdf()))
graphit4(c("San Marino","vatican","andorra",'Monaco','Malta','Liechtenstein' ),1,size=3)

#for publication
graphit4(WestvsEast,500,savename="West vs East4")

graphit4(WestvsEast,100,yvars="deaths",logy=FALSE,savename="West vs East")
graphit4("Belgium",100,yvars=c("confirmed","recovered","deaths"),logx=TRUE,savename="Belgium")
graphit4(EU,100,savename="EU")
graphit4(EU,500,yvars=c("confirmed","deaths"),savename="EU")
graphit4(EU,100,yvars=c("deaths"),savename="EU")

graphit4(EU,1,xvar="confirmed",logx=TRUE,yvars=c("deaths"),savename="EU")
graphit4(smallEurope,50,yvars=c("confirmed","deaths"),savename="Europe, less touched countries" )
graphit4(MENA,20,yvars=c("confirmed","recovered"), size=1,needfuzzy=FALSE,savename="MENA")
graphit4(c(Africa),100,size=3,yvars="confirmed",savename="Africa")
graphit4(c(Africa),10,size=3,yvars="confirmed",savename="Africa")
graphit4(c(Africa,"Japan","Italy"),3,yvars="confirmed",savename="Africa+Japan+Italy")
graphit4(SAsiaIO,50,savename="South Asia and Indian Ocean")
graphit4(c("italy",SouthEastAsia),50,yvars=c("confirmed","recovered","deaths"),savename="South East Asia")
graphit4(c("hubei","italy",SouthEastAsia),10,xvar="confirmed",logx=TRUE,yvars=c("recovered","deaths"),savename="South East Asia+Italy, Hubei")

graphit4(CAsia,100,savename="West and Central ASia",ID="Country.Region")

graphit4(CIS,10,yvars=c("confirmed"))
graphit4(CIS,10,savename="CIS and Georgia(s)")
graphit4("Australia",10,savename="Australia" )
graphit4(SAmerica,10,size=3,savename="South America" )
graphit4("US",needfuzzy=FALSE)

#############  USA?
graphit4(c(", US"),100,yvars=c("confirmed"), ID="CRPS",needfuzzy=TRUE,legend=TRUE)#,savename="USA states") #,"deaths","recovered"
graphit4(c("US"),100,yvars=c("confirmed"), ID="CRPS",needfuzzy=TRUE,legend=TRUE)#,savename="USA states") #,"deaths","recovered"



graphit4(c("France", "Belgium","Netherlands","Italy","Germany","United Kingdom"),minval=100,ID="CRPS", yvars=c("confirmed" , "deaths"))#,logy=FALSE)
##### scatter plot of two vars: 

graphit4(EU,logy=FALSE,logx=FALSE)
graphit4(EU,xvar="new_confirmed", yvars=c("new_recovered"),logy=TRUE,logx=TRUE)

graphit4(SouthEastAsia,yvars=c("confirmed","recovered"),logy=FALSE)
graphit4(SouthEastAsia,xvar="confirmed",yvars=c("deaths","recovered"),logy=FALSE,logx=FALSE)
graphit4(c("italy",SouthEastAsia),xvar="confirmed",yvars=c("deaths"),logy=TRUE,logx=TRUE,savename="Italy & SE Asia")
##############################################
graphit4(testcountries, ID="Country.Region",yvars=c("confirmed","deaths","recovered"),needfuzzy=TRUE)
View(graphit4)



################   China
graphit4("china", 100,ID="CRPS")
graphit4("china",0,yvar="recoveredOverDeaths",ID='CRPS')
graphit4("china",10,yvars="recovered", logy=FALSE,ID='CRPS')
names(alldata)
graphit4("china",0,"CRPS", yvars="recoveredOverConfirmed",logy=FALSE,putlegend=TRUE)
graphit4(EU,0.25,"CRPS",yvars="recoveredOverConfirmed",logy=FALSE,putlegend=TRUE)



####### Canada
graphit4(c("Canada"),2,"CRPS") #empty after 20200325
graphit4(c("Australia"),2,"CRPS")
##### USA

graphit4(c("US","China"),10,ID="CRPS",yvars=c("confirmed"), needfuzzy=FALSE) #,"deaths","recovered"
graphit4(c(", US","Hubei"),100,"CRPS",c("confirmed","deaths","recovered"),needfuzzy=TRUE)
names(totals(rows="US", id="Country.Region"))

###########################  d vs c
graphit4(c("France", "Belgium","Luxembourg"), 0, yvars=c("deathsOverConfirmed"), xvar="confirmed",logy=FALSE ,logx=TRUE)
graphit4(c("France", "Belgium","Luxembourg"), 100,"CRPS", yvars=c("deaths","confirmed"),logy=FALSE )

graphit4(c("Italy","Indonesia","Germany","France", "Belgium","Luxembourg"), 100, yvars=c("deaths","recovered"), xvar="confirmed",logx=TRUE )
graphit4(c("Belgium","Netherlands"),minval=100,xvar= "confirmed",yvars=c("confirmed","deaths"),ID="CRPS",needfuzzy=TRUE,logx=TRUE)
graphit4(c("Indonesia", "Belgium","Netherlands"),ID="CRPS",yvars=c("recoveredOverDeaths"),xvar= "confirmed",needfuzzy=TRUE,logx=TRUE)



#################