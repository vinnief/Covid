source("requirements.R")
source('definitions.R')



addSimVars <- function(lpti,countries,ext = '_sim',...){ # doublingDays=-1,pop=0,...){
  if (!missing(countries)) countries = findIDnames(lpti,countries,searchID = 'PSCR',fuzzy = FALSE)
  else {
    countries = unique(lpti$PSCR)
    if (verbose >= 3) print( "AddSimVars: no country given, simulating:" % % paste(countries,collapse="/"))
  }
  if (!('confirmed' %+% ext %in% names(lpti))) {lpti[,paste(c('confirmed','active','recovered','deaths'),ext,sep="")] <-NA}
  map(countries, 
      (function(country){
          lpti[lpti$PSCR==country,]<- addSimVarsOneCountry(lpti[lpti$PSCR==country,], country,ext,...)
          lpti}))
  lpti
}


curGraph('GR', myfolder1 = 'June and beyond', from='2020-06-01', lpdf = JHH, regions = JHHRegios, graphlist=myGraphNrs)
curGraph('GR', myfolder1 = 'May and beyond', from='2020-05-01', lpdf = JHH, regions = JHHRegios, graphlist=myGraphNrs)
curGraph('GR', myfolder1 = 'May', from='2020-05-01', until="2020-05-31", lpdf = JHH, regions = JHHRegios, graphlist=myGraphNrs)

JHH0B <- JHH0[JHH0$PSCR=='Belgium',]
JHHB <- JHH0[JHH0$PSCR=='Belgium',] %>% estimateDoublingDaysOneCountry("Belgium")#
JHHB <- JHH0 %>% addDoublingDaysperCountry() %>% view
  #%>% [JHH0$PSCR%in%c('Belgium','Netherlands','France'),]
  addSimVars('Belgium',minVal=100,ext='_backsim')#%>% 
  view(JHH[JHH$PSCR=='Belgium',])
  view(ECDC[ECDC$PSCR=='Belgium',])
#test missing parameters
d=23;r=2
test<- function(d,r){
  print(myPath)
  if (missing (r)&!missing(d)) r <- 2^(1/d)
  if(missing(d)&!missing (r)) d  <- -log2(r)
  if (missing (d)) print('d still missing ='% %d) else print('d not missing'% %d)
  if (missing (r)) print('r still missing ='% %r) else print('r not missing'% %r) 
  myPath <- myPath %//% 'current'
  poop<- 'oo'
  f()
  graphit( ECDC,ECDCRegios$`ECDC world6`,yline=100, savename = 'test')
  print(myPath)
}
f <- function () print (poop)
test(d=5)
myPath
test(r=1.14)
test()

#why is spain gone? 
view(graphit(ECDC,c('USA',"Spain"),facet='PSCR')) #,"France"
View(graphit(ECDC,c('USA',"Belgium","Spain",'Italy'),facet='PSCR')) #,"France" #spain disappears! 
graphit(ECDC,c("Belgium","Spain",'Italy',"France")) #,"France" #spain disappears! 
graphit(JHH,c("Belgium","Spain",'Italy',"France"),facet='PSCR') #,"France"
graph6Dardcra_fiyl(ECDC,c("Belgium","Netherlands")) #,'USA'

JHH[JHH$PSCR== 'Belgium'& JHH$confirmed>=100 & 
    JHH$Date>="2020-03-06" & JHH$Date<='2020-06-03',] %>% nrow
JHH[JHH$PSCR== 'Belgium'& JHH$confirmed>=100 & 
      JHH$Date>="2020-03-06" & JHH$Date<=Sys.Date(),] %>% nrow
JHH[JHH$PSCR== 'Netherlands'& JHH$confirmed>=100 & 
      JHH$Date>="2020-03-06" & JHH$Date<=Sys.Date(),] %>% nrow


isdouble <- function (country,regiolist) {
  sum(unlist(llply(regiolist, function (charvect) country %in% charvect)))
}
isdouble('US',regios)
checkdouble <- function(country,regiolist){
  which(unlist(llply(regiolist, function (x) country%in% x)))}
checkdouble('USA',JHHRegios)

checkdouble(c('USA','Netherlands'),JHHRegios)
isdouble(c('Belgium','Netherlands'),regios)
length(JHHRegios)

addCountryTotals<- function(lpdf=JHH){
  varnames=c("confirmed","recovered", "deaths","population")
  existingTotals<- c("China","Australia","Canada",'US')
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit US as country and US states. 
  lpti<- lpdf %>%
    filter(!( PSCR %in% existingTotals )) #
  rbind(lpdf, 
        lpti%>% totals(c("China","Australia", "Canada",'US'),
                       ID="Country.Region", varnames= varnames))
  
}
addRegionTotals<- function(lpdf=JHH,totRegions=""){
  existingTotals<- c("South America", "Asia", "Africa", "Europe","China","Australia","Canada",'US','North America',"World")
  #just to be sure, that if i do it twice i dont get double counts. 
  #And omit US as country and US states. 
    lpti<- lpdf %>%
    filter(!(Country.Region =="US" | PSCR%in% existingTotals )) #we have US and USA
  if (totRegions[1]=="") totRegions<- c(regios)
  if (verbose>=3) {
    print('world totals include the following countries: ')
    print(paste(World,collapse=","))}
  varnames=c("confirmed","recovered", "deaths","population")
  for(myRegion in totRegions){
    lpdf<- rbind(lpdf, 
            lpti%>%total(regios[[myRegion]],ID=ID,newrow=myRegion[1], varnames= varnames))
  }
}
tail(JHH0%>%  addPopulation()%>% addCountryTotals %>% addRegionTotals%>% filter(Date==max(Date)),10)
profvis(JHH%>% addSimVarsOneCountry("Belgium",minDate="2920-05-01", ext='_endsim'))
#check colors
graph3Dard_fia (JHH,regios$continents)

graph6Dardcra_fiyl(JHH,"Idaho,US")
graphDddp_fyl(JHH,countries="New York,US")
graphDccp_fyl(JHH,countries="Belgium")
graphDccprr_fyl()
verbose=2
graphDccprr_fyl(JHH,"Netherlands")
graphDddp_fyl(JHH,"Netherlands")
graphit(ECDC,regios$Vincent,"deaths missed", yvars="deaths",xvar='day')%>% View

#from loaddata: trial to make the loading only happen if it gives new data
#while((Sys.time()>Sys.Date()% % "22:00:00")| max(JHH$Date)<Sys.Date()-1 ) {
#   source("loaddata.R")
#   if (max(JHH$Date)< Sys.Date()-1)     {
#    print( "failed to get yesterday's values ") 
#   for (i in 1:12){
#    print(Sys.time())
#   Sys.sleep(600)
#}   } }


total.tibble<- function(lpt=JHH,ID=Country.Region,
                        varnames=c('confirmed',#deaths,
                                   'recovered')){
  lpttot<-lpt%>%
    group_by({{ID}},Date)
  lpttot<- cbind(
    #lpttot%>% summarize(newPSCR:=aggreg(PSCR),
    #          newCountry.Region=aggreg(Country.Region),
    #          newProvince.State=aggreg(Province.State)), #!! mean_name := mean(!! expr)
    lpttot%>% summarize_at(.vars=!!!varnames, .funs=colSums)
  )
  #confirmed:=sum(confirmed),
  #active:=sum(active),
  #recovered=sum(recovered),
  #deaths=sum(deaths),
  #colSums(.[,])
  #if (!!ID=="PSCR")lpttot%>% rename(Coun)
}
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(PSCR)%>% view()
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(Country.Region)%>% view()
