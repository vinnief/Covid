source("definitions2.R")
alldata<- (makeGroups( mklpdf()))
graphit5(c("San Marino","vatican","andorra",'Monaco','Malta','Liechtenstein' ),1,size=3,area=FALSE)
graphit5(WestvsEast,100,xvar="Date",logy=FALSE,until="2020-03-15",area=FALSE) #as.Date("2020-02-01",format="%Y-%m-%d"))
graphit4(WestvsEast,100,yvars=c("new_confirmed"),xvar="Date",logy=FALSE)
graphit5(WestvsEast,100,yvars=c("new_confirmed"),xvar="Date",logy=FALSE,area=FALSE)
graphit4(WestvsEast,10,yvars=c("new_deaths"),xvar="Date",logy=FALSE)


#for publication
#1 Conf - inf - recovered, deaths 
#2 active+recovered+deaths 
#3 new conf , new rec, new deaths
Benelux<- c("Belgium","Netherlands","Luxembourg")
bigEur<- c('Italy','Spain','France','Germany','United Kingdom')
groups<- list(expression(Benelux),expression(bigEUr), expression(smallEurope), expression(CIS), expression(MENA), expression(SAsiaIO), expression(SAmerica), expression("Canada"))
groups<- list((Benelux),(bigEur), (smallEurope), (CIS), (MENA), (SAsiaIO), (SAmerica), ("Canada"))
groupnames<- c('Benelux','bigEur','smallEurope','CIS','MENA','SAsiaIO','SAmerica','Canada')
eval(groupnames[1])
deparse((groups[[1]]))
for (group in groups){
  print(findIDnames(eval(group),id='Country.Region',lpdf=alldata, fuzzy=FALSE))
  print(deparse(substitute(group)))
}
graphs<- function(group,name="",ID="CRPS"){
  graphit5(group, 100, ID='CRPS',xvar='day', yvars=c("confirmed", 'active', 'recovered','deaths'),
           fuzzy=FALSE, logy=TRUE,savename= name,facet="CRPS")
  graphit5(group,0,ID='CRPS', xvar="Date",yvars=c('new_confirmed','new_recovered','new_deaths'),
           fuzzy=FALSE, logy=TRUE,savename= name,facet="CRPS") 
  graphit5(group,0,ID='CRPS', xvar="Date",yvars=c('active','recovered','deaths'),
           fuzzy=FALSE, logy=TRUE,savename= name,area=TRUE,facet="CRPS") 
  graphit5(group,0,ID='CRPS', xvar="Date",yvars=c('recoveredOverConfirmed'),
           fuzzy=FALSE, logy=TRUE,savename= name )
  graphit5(group,100,ID="Country.Region", xvar='confirmed',yvars=c('recovered','deaths'),fuzzy=FALSE,
           logy=TRUE, logx=TRUE,savename=name)
  }
graphs(Benelux,"Benelux")
graphs(bigEur,'BigEurope')
graphit5(WestvsEast,500,xvar="day",yvars=c("confirmed", "recovered" ) , savename="West vs East",area=FALSE)
graphit5(WestvsEast,00,ID='CRPS',xvar="Date",yvars=c("active", "recovered","deaths" ) , savename="West vs East cum",area=TRUE,facet="CRPS")
graphit5(WestvsEast,100,xvar="confirmed",yvars="deaths",logy=FALSE,savename="West vs East")
graphit5("Belgium",100,yvars=c("confirmed","recovered","deaths"),logx=TRUE,savename="Belgium")
graphit5(EU,100,"CRPS",savename="EU")
graphit5(EU,100,"CRPS","confirmed",xvar="Date",savename="EU")
graphit5(EU,100,"CRPS","deathsOverRecovered",savename="EU")
graphit5(EU,500,"CRPS",yvars=c("confirmed","deaths"),savename="EU")
graphit5(EU,100,yvars=c("deaths"),savename="EU")

graphit5(EU,1,xvar="confirmed",logx=TRUE,yvars=c("deaths"),savename="EU")
graphit5(smallEurope,50,yvars=c("confirmed","deaths"),savename="Europe, less touched countries" )
graphit5(MENA,20,yvars=c("confirmed","recovered"), size=1,fuzzy=FALSE,savename="MENA")
graphit5(c(Africa),100,size=3,yvars="confirmed",savename="Africa")
graphit5(c(Africa),10,size=3,yvars="confirmed",savename="Africa")
graphit5(c(Africa,"Japan","Italy"),3,yvars="confirmed",savename="Africa+Japan+Italy")
graphit5(SAsiaIO,50,savename="South Asia and Indian Ocean")
graphit5(c("italy",SouthEastAsia),50,yvars=c("confirmed","recovered","deaths"),savename="South East Asia")
graphit5(c("hubei","italy",SouthEastAsia),10,xvar="confirmed",logx=TRUE,yvars=c("recovered","deaths"),savename="South East Asia+Italy, Hubei")

graphit5(CAsia,100,savename="West and Central ASia",ID="Country.Region")

graphit5(CIS,10,yvars=c("confirmed"))
graphit5(CIS,10,savename="CIS and Georgia(s)")
graphit5("Australia",10,savename="Australia" )
graphit5(SAmerica,10,size=3,savename="South America" )
graphit5("US",1,xvar="Date",yvars= c("confirmed","recovered","deaths"),fuzzy=FALSE)
graphit5("US",1,xvar="Date",yvars= c("active","recovered","deaths"),fuzzy=FALSE)


###### Interesting
graphit5(WestvsEast,0,ID="CRPS", yvars=c("new_deaths","deaths"),xvar="Date",logy=FALSE,facet='varname')
graphit5(WestvsEast,10,ID="CRPS", yvars=c("new_deaths"),xvar="Date",logy=FALSE,area=FALSE,fuzzy=FALSE,facet='CRPS')
WestvsEast
graphit5("Belgium",0,yvars=c("active","recovered","deaths"),xvar="Day",logy=FALSE,area=TRUE,fuzzy=FALSE)
graphit5("Belgium",1,yvars=c("active","recovered","deaths"),xvar="Day",logy=FALSE,area=TRUE,fuzzy=FALSE)
graphit5("Netherlands",10,ID="CRPS",yvars=c("active","recovered","deaths"),xvar="Day",logy=FALSE,area=TRUE,fuzzy=FALSE)
graphit5(c("Belgium","Netherlands"),10,ID="CRPS",yvars=c("new_confirmed","new_recovered","new_deaths"),xvar="Day",logy=FALSE,area=TRUE,fuzzy=FALSE,facet='CRPS')

graphit5("China",1,ID='CRPS',yvars=c("active","recovered","deaths"),xvar="Day",logy=FALSE,area=TRUE,facet='CRPS')
graphit5(c("Italy","Hubei"),10,ID='CRPS',yvars=c("active","recovered","deaths"),xvar="Date",logy=FALSE,area=TRUE,fuzzy=TRUE,facet='varname')



#############  USA?
graphit4(c(", US"),100,yvars=c("confirmed"), ID="CRPS",fuzzy=TRUE,putlegend=TRUE)#,savename="USA states") #,"deaths","recovered"
graphit4(c("US"),100,yvars=c("confirmed"), ID="CRPS",fuzzy=TRUE,putlegend=TRUE)#,savename="USA states") #,"deaths","recovered"



graphit4(c("France", "Belgium","Netherlands","Italy","Germany","United Kingdom"),minval=100,ID="CRPS", yvars=c("confirmed" , "deaths"))#,logy=FALSE)
##### scatter plot of two vars: 

graphit4(EU,logy=FALSE,logx=FALSE)
graphit4(EU,xvar="new_confirmed", yvars=c("new_recovered"),logy=TRUE,logx=TRUE)

graphit4(SouthEastAsia,yvars=c("confirmed","recovered"),logy=FALSE)
graphit4(SouthEastAsia,xvar="confirmed",yvars=c("deaths","recovered"),logy=FALSE,logx=FALSE)
graphit4(c("italy",SouthEastAsia),xvar="confirmed",yvars=c("deaths"),logy=TRUE,logx=TRUE,savename="Italy & SE Asia")
##############################################
graphit4(testcountries, ID="Country.Region",yvars=c("confirmed","deaths","recovered"),fuzzy=TRUE)
View(graphit4)



################   China
graphit5("china", 100,ID="CRPS",facet="CRPS")
graphit4("china",0,yvar="recoveredOverDeaths",ID='CRPS')
graphit4("china",10,yvars="recovered", logy=FALSE,ID='CRPS')
names(alldata)
graphit4("china",0,"CRPS", yvars="recoveredOverConfirmed",logy=FALSE,putlegend=TRUE)
graphit4(EU,0.25,"CRPS",yvars="recoveredOverConfirmed",logy=FALSE,putlegend=TRUE)



####### Canada
graphit4(c("Canada"),2,"CRPS") #empty after 20200325
graphit4(c("Australia"),2,"CRPS")
##### USA

graphit4(c("US","China"),10,ID="CRPS",yvars=c("confirmed"), fuzzy=FALSE) #,"deaths","recovered"
graphit4(c(", US","Hubei"),100,"CRPS",c("confirmed","deaths","recovered"),fuzzy=TRUE)
names(totals(rows="US", id="Country.Region"))

###########################  d vs c
graphit4(c("France", "Belgium","Luxembourg"), 0, yvars=c("deathsOverConfirmed"), xvar="confirmed",logy=FALSE ,logx=TRUE)
graphit4(c("France", "Belgium","Luxembourg"), 100,"CRPS", yvars=c("deaths","confirmed"),logy=FALSE )

graphit4(c("Italy","Indonesia","Germany","France", "Belgium","Luxembourg"), 100, yvars=c("deaths","recovered"), xvar="confirmed",logx=TRUE )
graphit4(c("Belgium","Netherlands"),minval=100,xvar= "confirmed",yvars=c("confirmed","deaths"),ID="CRPS",fuzzy=TRUE,logx=TRUE)
graphit4(c("Indonesia", "Belgium","Netherlands"),ID="CRPS",yvars=c("recoveredOverDeaths"),xvar= "confirmed",fuzzy=TRUE,logx=TRUE)



#################