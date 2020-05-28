source("requirements.R")
source('definitions.R')


length(JHHRegios)

#check colors
graph3Dard_fia (JHH,JHHRegios$continents)

dev.off()
demoDoubling(doublingDays=1)
demoDoubling(doublingDays=2)

graph6Dardcra_fiyl(JHH,"Idaho,US")
graphDddp_fyl(JHH,countries="New York,US")
graphDccp_fyl(JHH,countries="Belgium")

verbose=2
writeRegioGraph(ECDC,ECDCRegios)$simulations! 
graph
writeRegioGraph(ECDC,ECDCRegios,graphlist = myGraphNrs)
graphDrccrrp_fyl(JHH,"Netherlands")
graphit(ECDC,regios$Vincent,"deaths missed", yvars="deaths",xvar='day')%>% View

simulGrow(lpt=ECDC,'Belgium',start=100,doublingDays=5,nrRows=1000,deathRate=.05,pop=1.1e7,lagrc=42,lagdc=36) %>% 
  graph6Dardcra_yfil("25 days") #%>% 
  View

2^(doublingdays(1)/LAGRC)
#%>%

#check imputations

JHHdifimp<- unique(JHH%>% filter(active != active_imputed)%>% .$PSCR )
JHH%>% makehistory(regions=JHHdifimp, graphlist = c('graphDrr_fia','graphDaa_fia','graphDaa_yfil')  ) 


# pmin(confirmed-deaths, 
#      pmax(0,recovered, 
#       lag(confirmed,lagrc)- lag(deaths,lagrd), na.rm=TRUE),
#      na.rm=FALSE)

JHH%>% filter(PSCR %in% "Wyoming,US") %>% select(PSCR,confirmed,active_imputed,recovered_imputed,deaths)%>%View

##############
#break labels in graph across lines to facet better: 
#for readable labels on facet titles, split them into lines with
#reformat <- function(x,splitt=", ",lab="\n"){ sapply(x, function(c){ paste(unlist(strsplit(as.character(c) , split=splitt)),collapse=lab) }) }
#dataset$variable <- factor(dataset$variable, labels=reformat(dataset$variable, lab='\n')
#And upon facetting, all labels will be very readable:
# ggplot(data=dataset, aes(x,y)) + geom_point() + facet_grid(. ~ variable)
#that is for factor labels. we need it for variable lables. 

J
total.tibble<- function(lpt=JHH,ID=Country.Region,varnames=c('confirmed',#deaths,
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
