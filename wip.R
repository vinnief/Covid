source("requirements.R")
source('definitions.R')

#traceback() # to trace the nested calls leading to an error. 
#suppressWarnings() to suppresswarnings of the functioninside. does not stop the warning: "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?"

options(warn = 2)
verbose <- 7
verbose <-  1

#alternative version which does not work. because of metaprogramming issues with arrange. 
sortIDlevels1 <- function(lpdf, varname = confirmed, ondate = ""){
  varname <- enquo(varname)
  if (ondate  == "") {ondate = max(lpdf$Date) } else 
    if (nrow(lpdf[lpdf$Date  ==  ondate, ]  == 0)) {
      stop("Cannot sort on values of a date which is not present in the Data")}
  PSCRlevels <- lpdf  %>%  select(c(PSCR, Date, !!varname))  %>%  
    filter(Date  == ondate)  %>%  
    arrange(-eval(parse(text = substitute(!!varname))), PSCR, .by_group  = FALSE) 
  arrange(desc({{varname}}), PSCR, .by_group  = FALSE) 
  #arrange(-(!!varname), PSCR, .by_group  = FALSE) 
  lpdf <- lpdf %>% ungroup %>%  mutate(PSCR = factor(lpdf$PSCR, levels = PSCRlevels$PSCR)) %>% 
    group_by(PSCR)
  lpdf$PSCR
} #the desc eval parse substitute !! should have been desc !! according to the manuals. but desc does not respect unquo. 


#from graphit: 
#
switch(labmeth,
       'none' = {},
       'dl_polygon' = {myplot = myplot + geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),
                 method  = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last.polygons", cex  = 1.2))
       xexpand = 0.25} ,
       'dl_bumpup' = {myplot = myplot + geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),
                method = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last.bumpup", cex  = 1.2))
       xexpand = 0.25} ,
       'label_repel' = {myplot = myplot + geom_label_repel(data = lpdf[lpdf[[xvar]] == max(lpdf[[xvar]]),], 
       aes(label = mygroup), label.size = 0.1, box.padding = 0.1, label.padding = 0.2, size = 3)
       xexpand = 0.35} ,
       'text_repel' = {myplot = myplot + geom_text_repel(data = lpdf[lpdf[[xvar]] == max(lpdf[[xvar]]),], 
        aes(label = mygroup), box.padding = 0.1, size = 3)
       xexpand = 0.4} ,
       {myplot = myplot + 
         geom_dl(aes_string(x = xvar, y = "count",  label = 'mygroup'),   #default
                method = list(dl.trans(x  = x + 0.1 , y = y + 0.1), "last.points", cex  = 1.2)) 
         xexpand = 0.35} 
)




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



#12 ggplot extensions: ggcorplot, 
#https://mode.com/blog/r-ggplot-extension-packages
