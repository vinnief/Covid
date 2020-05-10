source ("requirements.R")#load libraries
source("definitions2.R")# make sure we have our definitions. 
#execute the data loading, make the long panel data frame, with id=CRPS, time= Date
##with totals for continents and world, and (future) sorted by confirmed (desc)
JHH<- makeJHH(force=TRUE) #load data
#lpdf$CRPS<- sortCRPS(lpdf)
JHH0<- JHH
regios<- c(provincializeJHH(),regios)
JHH <-  JHH0 %>%
          makeGroups( Regiolist= regios)%>% 
          addPopulation() %>%
          imputeRecovered2(redo=TRUE) %>%
          extravars2() %>%
  #view(  JHH%>%   
           addtotals
           #%>% filter(CRPS %in% c('World',"Europe","Asia")))  #continents, Ca,Aus,China)
#4 warnings: Unknown or uninitialised column: `mygroup`.
#
# 36 Warning messages:
# In mutate_impl(.data, dots, caller_env()) :
#Vectorizing 'ts' elements may not preserve their attributes

writewithcounters(JHH,name="Covid19JHH") #no factors!
print( "totals exist for")
print(unique(JHH[JHH$CRPS %in% c("Australia", "Canada","China","USA","US","Asia","Africa","Europe"),][["CRPS"]]))


#same with ecdc
ecdc0 <- makeecdc()
ecdc <- ecdc0%>% imputeRecovered2()%>%  extravars2()
writewithcounters(ecdc,name="Covid19ECDC")
unique(ecdc$CRPS)

# end now run Graphs.R

