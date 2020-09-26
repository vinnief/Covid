# Resulting graphs and cleaned and prepared data can be found at 
# https://drive.google.com/drive/folders/1Yo0IW4awCIvWMP6jYVKpn3kLgEolse0e?usp=sharing
#for publication 
rm(list = setdiff(ls(), c('ECDC0', 'JHH0', 'JHH', 'ECDC', 'JHHRegios', 'ECDCRegios','testing')))
options(warn = 0)
if (!exists('JHH')) source('loadData.R') else 
  if (max(JHH$Date) < Sys.Date() - 1) {source('loadData.R') 
    }else if (!exists('graphit')) source('definitions.R')

#also loads the requirements and the definitions 

#make all graphs
tim = Sys.time()
walkThrough(lpdf = ECDC, regions = ECDCRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG')
reportDiffTime('ECDC graphs',tim, units = 'mins')

tim = Sys.time()
walkThrough( lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, myFolderDate  = 'current', ordre = 'RG')
reportDiffTime('JHH graphs',tim,units = 'mins')

writeWithCounters(ECDC,name = "Covid19ECDC")
writeWithCounters(JHH,name = "Covid19JHH") 
