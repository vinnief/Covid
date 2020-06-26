source("requirements.R")
source('definitions.R')
#


a <- merge(ECDC, testing, by.x = 'countryterritoryCode', by.y = 'ISO code', all.x = TRUE,
           sort = FALSE)

addCountryTotals <- function(lpdf = JHH){
  varnames = c("confirmed","recovered", "deaths","population")
  existingTotals <- c("China","Australia","Canada",'US')
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
