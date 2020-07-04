#for publication 
rm(list = setdiff(ls(), c('ECDC0', 'JHH0', 'JHH', 'ECDC', 'JHHRegios', 'ECDCRegios','testing')))
options(warn = 0)
source("loadData.R")  #also loads the requirements and the definitions 

#make all graphs
tim = Sys.time()
curGraph('GR', lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs)
reportDiffTime('ECDC graphs',tim, units = 'mins')

tim = Sys.time()
curGraph('GR', lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs)
reportDiffTime('JHH graphs',tim,units = 'mins')


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

