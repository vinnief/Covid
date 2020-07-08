# Resulting graphs and cleaned and prepared data can be found at 
# https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing
#for publication 
rm(list = setdiff(ls(), c('ECDC0', 'JHH0', 'JHH', 'ECDC', 'JHHRegios', 'ECDCRegios','testing')))
options(warn = 0)
source("loadData.R")  #also loads the requirements and the definitions 

#make all graphs
tim = Sys.time()
#curGraph(lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs, ordre = 'GR')
walkThrough(lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG')
reportDiffTime('ECDC graphs',tim, units = 'mins')

tim = Sys.time()
#curGraph( lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, ordre = 'RG')
walkThrough( lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG')
reportDiffTime('JHH graphs',tim,units = 'mins')

#do a month
walkThrough(lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, from = "2020-01-01", to = '2020-01-31', myFolderDate = 'January', ordre = 'RG')
walkThrough(lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, from = "2020-04-01", to = '2020-04-30', myFolderDate = 'April', ordre = 'RG')
walkThrough(lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, from = "2020-06-01", to = '2020-06-30', myFolderDate = 'June', ordre = 'RG')

startDate = "2020-01-01"
fromDates <- as.character(seq(as.Date(startDate), length = 12, by = "1 month"))
toDates <- as.character(seq(as.Date(fromDates[2]), length = 12, by = "1 month") - 1)
  
makeHistoryGraphs(ECDC,ECDCRegios, graphlist = myGraphNrs, 
                    fromDates = fromDates, toDates = toDates)

makeHistoryGraphs(JHH,JHHRegios, graphlist = myGraphNrs, 
                  fromDates = fromDates, toDates = toDates)
writeWithCounters(ECDC,name = "Covid19ECDC")
writeWithCounters(JHH,name = "Covid19JHH") 
#check what graphs are defined 
#graphCodes()
myGraphListbyDate
myGraphListbyDay
myGraphNrs
myGraphList

verbose = 2
#look at growth rates and growth paths in first days (Synchronized)
ECDC %>% byRegionthenGraph(regions = ECDCRegios,graphlist = c("graph1dnr_iyl","graph2dac_iyl"))

#simulate deaths and confirmed   
ECDC %>% byRegionthenGraph(ECDCRegios,ext = '_sim', graphlist = c('graphDccprr_fiyl','graphDddp_fyl'))  #sims
ECDC %>% byRegionthenGraph(ECDCRegios,ext = '_endsim',graphlist = c('graphDccprr_fiyl','graphDddp_fyl')) #simulations 
graphDddp_fyl(JHH,regios$Vincent,savename  = "deaths missed") 

if ( weekdays( Sys.Date() , abbreviate = FALSE) == "Friday") ECDC %>% 
  byRegionthenGraph(ECDCRegios, graphlist = myGraphListbyDate)

JHH %>% byRegionthenGraph(regions = JHHRegios,graphlist = 'graphDccprr_fyl')
JHH %>% byRegionthenGraph(regions = JHHRegios[1:2],graphlist = 'graphDccprr_fyl')
  #byRegionthenGraph(regions=JHHRegios[1:2],graphlist = 'graphDccprr_fyl')


verbose = 3

#do standard graphs for all past dates
JHH %>% makehistory(regions = JHHRegios, dates = seq(Sys.Date() - 0,Sys.Date() - 250,-10))  
ECDC %>% makehistory(regions = ECDCRegios, dates = seq(Sys.Date() - 250,Sys.Date() - 0,30))  

