source("requirements.R")


JHH%>% filter(is.na(population)&Date=='2020-05-19')%>% view()

# check imputation in wyoming is not equal to conf-deaths without lag.
#check imputations
JHHdifimp<- unique(JHH%>% filter(active != active_imputed)%>% .$PSCR )
JHH%>% makehistory(regions=JHHdifimp, graphlist = c('graphDrr_fia','graphDaa_fia','graphDaa_yfil')  ) 


JHH%>% filter(PSCR %in% "Wyoming,US") %>% 
  select(PSCR,confirmed,active_imputed,recovered,recovered_imputed,deaths)%>% 
  mutate(r_i=pmin(confirmed-deaths,
                  pmax(0,
                       recovered,
                       dplyr::lag(confirmed,42)-dplyr::lag(deaths,36),
                       na.rm=TRUE),
                  na.rm=FALSE))%>% 
  View
JHH0%>%filter(PSCR %in% "Wyoming,US") %>% imputeRecovered2() %>%
  select(PSCR,confirmed,recovered,recovered_imputed,deaths)%>%
  View
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

JHH%>% filter (Country.Region=='Netherlands') %>% totals2 ("","PSCR")%>% head()
JHH%>% filter (Country.Region=='Netherlands') %>% totals ("","Country.Region")%>% tail()%>% select(1:13) #prov becomes netherlands.
JHH%>% filter (Country.Region=='Netherlands')%>% total ("","Country.Region")%>% tail()%>% select(1:10) # prov has accretion of all prov names. 


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
