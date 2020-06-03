source ("requirements.R")#load libraries
source("definitions.R")# make sure we have our definitions. 
#execute the data loading, make the long panel data frame, 
#with id=PSCR, time= Date, totals for continents and world, 
#and (in future) sorted by confirmed (desc)
JHH<- makeJHH(force=TRUE) #load data
JHH0<- JHH

regios<- c(list(World=c('World', unique(JHH[['PSCR']]))),provincializeJHH(),regios) #regiosP?

verbose=2

JHH <-  JHH0 %>%
  addPopulation() %>% addTotals2 %>% addRegions( Regiolist= regios)%>% 
  imputeRecovered()%>% extravars() %>% addDoublingDaysAllCountries %>%
  addSimVars(minval=100)
  
#profvis(JHH %>% addSimVars())

writewithcounters(JHH,name="Covid19JHH") #no factors!

#same with ECDC
ECDC0 <- makeECDC()
ECDC <- ECDC0 %>% addTotals3%>% imputeRecovered()%>%  extravars() %>% 
            addDoublingDaysAllCountries %>% addSimVars(minval=100)
writewithcounters(ECDC,name= "Covid19ECDC")

# end now run Graphs.R