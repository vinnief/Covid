source ("requirements.R")#load libraries
source("definitions2.R")# make sure we have our definitions. 
#execute the data loading, make the long panel data frame, 
#with id=PSCR, time= Date, totals for continents and world, 
#and (in future) sorted by confirmed (desc)
JHH<- makeJHH(force=TRUE) #load data
JHH0<- JHH
#JHH$PSCR<- sortIDlevels(JHH)
regios<- c(provincializeJHH(),regios)

JHH <-  JHH0 %>%
  addPopulation() %>%
  addtotals2 %>%
  makeGroups( Regiolist= regios)%>% 
  imputeRecovered2()%>% #correct=TRUE) %>%
  extravars2() 
writewithcounters(JHH,name="Covid19JHH") #no factors!
print( "totals exist for")
print(unique(JHH[JHH$PSCR %in% c("World", "Australia", "Canada","China","USA","US","Asia","Africa","Europe"),][["PSCR"]]))
#%>% filter(PSCR %in% c('World',"Europe","Asia")))  #continents, Ca,Aus,China)
#4 warnings: Unknown or uninitialised column: `mygroup`.
#
# 36 Warning messages:
# In mutate_impl(.data, dots, caller_env()) :
#Vectorizing 'ts' elements may not preserve their attributes

#same with ECDC
ECDC0 <- makeECDC()
ECDC <- ECDC0 %>% imputeRecovered2()%>%  extravars2()
writewithcounters(ECDC,name= "Covid19ECDC")

# end now run Graphs.R