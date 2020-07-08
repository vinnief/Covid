source("requirements.R")
source('definitions.R')

#traceback() # to trace the nested calls leading to an error. 
#suppressWarnings() to suppresswarnings of the functioninside. does not stop the warning: "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?"
options(warn = 2)
verbose <- 7
verbose <-  1

withVisible(graph6Dardcra_fiMnyl(ECDC,ECDCRegios$`ECDC world3`, from = '2020-02-01', to = '2020-02-28'))# ,logy = FALSE) # ) # no more geom_path warnings! 


#check addSimVars
lpti <- ECDC[ECDC$PSCR %in% c("Cases_on_an_international_conveyance_Japan", "Cayman_Islands", "Central_African_Republic"),]
minVal <- 100; minDate = '2019-12-31'; maxDate = Sys.Date()

 for (country in countries <- unique(lpti$PSCR)) {
   
   if (any(is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))) { 
     print(country % % 'nrows' % % nrRows % % newnrRows) 
     print(which((is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))))}
 }
country <- 'Cayman_Islands' 
print(which((is.na(lpti[lpti$PSCR == country ,c('confirmed','Date')]))))
lpti[lpti$PSCR == country & lpti$confirmed >= minVal & lpti$Date >= minDate & lpti$Date <=  maxDate,] %>% view

lpti %>% view
lpti %>% addSimVars(minVal = 100) #error. 
lpti <- ECDC[ECDC$PSCR %in% c("Cases_on_an_international_conveyance_Japan", "Cayman_Islands"),]
lpti %>% addSimVars(minVal = 100) #no error. 
lpti[lpti$confirmed_doublingDays < 0, ] %>% view #39 totally NA rows 
lpti[is.na(lpti$confirmed_doublingDays), ] %>% view # 3172 rows
ECDC[is.na(ECDC$PSCR),] %>% view()   #0 rows. 
E10 <- ECDC %>% addSimVars(minVal = 100) #error with Cayman_Islands
#






lpti <- ECDC[ECDC$PSCR == 'Europe',]
lpti %>% addSimVars(minDate = Sys.Date() - 10, ext = "_endsim2") %>% view #one row less. 
options(warn = 2)
E10 <- ECDC %>% addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") # europe -1 extra row generated this is ok. it was because Spain delivers data later. 
# 2020-07-05 data:  UK has 7 out of 11 rows. 2020-07-03: 10 out of 11
#
## also, NAs produced by rpois (42 times) and 
# In `[<-.data.frame`(`*tmp*`, lpti$PSCR == country, , value = structure(list( ... :
#provided 51 variables to replace 47 variables

#check which graph gives 
#"geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?" 
graph2dac_iyl(JHH,JHHRegios$`JHH World13`, from = '2020-06-01', to = '2020-07-01') #gives error because no data with minVal = 100
options(warn = 2)
curGraph('GR', lpdf = JHH, regions = JHHRegios, graphlist = myGraphNrs, saveit = FALSE)

# integrate testing into ECDC & JHH
a <- merge(ECDC, testing, by.x = c('ISOcode','Date'), by.y = c('ISOcode','Date'), all.x = TRUE,
           sort = FALSE)
names(testing)

#make gifs
#from loaddata: trial to make the loading only happen if it gives new data
#while((Sys.time()>Sys.Date()% % "22:00:00")| max(JHH$Date)<Sys.Date()-1 ) {
#   source("loaddata.R")
#   if (max(JHH$Date)< Sys.Date()-1)     {
#    print( "failed to get yesterday's values ") 
#   for (i in 1:12){
#    print(Sys.time())
#   Sys.sleep(600)
#}   } }


devtools::install_github("dgrtwo/gganimate")# and install image_magick. 
# see http://www.ggtern.com/2017/07/23/version-2-2-1-released/
# add  frame = Date  to aes of ggplot, then
# gganimate(myplot, "filename.gif" )  # or mp4, html, swf. 
# p3 <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
#geom_path(aes(cumulative = TRUE, group = country)) +
#  scale_x_log10() +
#  facet_wrap(~continent)
#12 ggplot extensions: ggcorplot, 
#https://mode.com/blog/r-ggplot-extension-packages
