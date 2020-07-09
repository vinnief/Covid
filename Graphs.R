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


writeWithCounters(ECDC,name = "Covid19ECDC")
writeWithCounters(JHH,name = "Covid19JHH") 
