#Unittests #
#check testing ISOcodes and PSCR
setdiff(unique(testing$PSCR),unique(ECDC$PSCR))
setdiff(unique(ECDC$PSCR),unique(testing$PSCR))

setdiff(unique(testing$ISOcode),unique(ECDC$ISOcode))
setdiff(unique(ECDC$ISOcode),unique(testing$ISOcode))

#check def of diff.sl
stopifnot(all(is.na(JHH[JHH$Date == "2020-01-22", "new_confirmed"])) )
#see where imputations are made
JHH %>% byRegionthenGraph(  regions = c('really imputed',unique(JHH %>% filter(active != active_imputed) %>% .$PSCR )), 
 graphlist = c('graphDrr_fia','graphDaa_fia','graphDaa_fiyl') ,myfolder1 = 'imputationcheck') 
JHH %>% filter(PSCR %in% "Wyoming,US") %>% select(PSCR,confirmed,active_imputed,recovered_imputed,deaths) %>% View

# check total and totals vs totals2 and old total

options(warn = 2)
makelpdfUS()
JHH0 %>%  addPopulation() %>% addCountryTotals %>% addRegionTotals %>% 
  group_by(PSCR) %>% 
  filter(PSCR == 'Netherlands') %>% tail

JHH0 %>%  addPopulation() %>% addTotals2() %>% #addCountryTotals %>% #addRegionTotals %>% 
  group_by(PSCR) %>% filter(Date == max(Date)) %>% tail(.,10)
JHH %>% # addPopulation() %>% addCountryTotals %>% #addRegionTotals %>% 
  group_by(PSCR) %>% filter(PSCR %in% c("China","Australia","Canada",'US') & Date == max(Date)) %>% tail(.,5)

# check addtotalsregion and country
lptivalid <- JHH[ JHH$confirmed >=  100 & JHH$Date >=  Sys.Date()-5 ,]
lptivalid %>% filter(PSCR %in% c('China','Asia','Australia', 'Western Australia,Australia','Europe')) %>% view
lptivalid <- ECDC[ ECDC$confirmed >=  100 & ECDC$Date >=  Sys.Date()-5 ,]
#View some graphs:
graph3Dard_fina(JHH,c("Kazakhstan","Belgium","Netherlands","France", "Ontario,Canada"),from = "2020-06-10")
graph3Dard_fina(ECDC,regios$MSM,from = Sys.Date()-7) 
graph6Dardcra_finyl(ECDC,regios$MSM,from = Sys.Date()-7)
graph6Dardcra_finyl(JHH, c("Kazakhstan","Belgium","Netherlands","France"),from = Sys.Date()-10)

verbose = 7
# sorting: 
stopifnot( (sortIDlevels(ECDC) %>% unique %>% NROW) == NROW(unique(ECDC$PSCR) ))#should have Spain in it ! is ok. 
sortIDlevels(ECDC,ondate = '2020-12-31') %>% unique # shouldstop
sortIDlevels(ECDC,ondate = Sys.Date()) %>% unique # should warn no data
'Spain' %in% (sortIDlevels(ECDC, ondate = Sys.Date()-1) %>% unique ) # FALSE and warning. 
setdiff((sortIDlevels(ECDC, ondate = Sys.Date()-2) %>% unique ) ,(ECDC$PSCR %>% unique))# 2 countries 
setdiff((sortIDlevels(ECDC) %>% unique ) ,(ECDC$PSCR %>% unique))# TRUE 
NROW (sortIDlevels(JHH, ondate = Sys.Date()-1) %>% unique ) == NROW (JHH$PSCR %>% unique) # TRUE

graph6Dardcra_fiyl(ECDC,ECDCRegios$`ECDC world3`) # still missing !
graph6Dardcra_fiyl(ECDC,c('Spain','Italy','United Kingdom'))


##test if mac does not reduce the amounts too much (after all we only average above a threshhold)
rs<- function(n=200,s=50) {
  x<- rnorm(100,n,s)
  round(rowSums( rbind(x,
                       mac.1=mac.(x,sides=1),ma1=ma(x,sides=1),
                       mac.2=mac.(x,sides=2),mac.=mac.(x),ma=ma(x)))
        /sum(x)*100,2)
}

#test addSimVars
#

names(ECDC)
summary(ECDC$confirmed_growthRate)
options(warn=2)
E10 <- ECDC %>% addSimVars(minDate = Sys.Date() - 10, ext = "_endsim") # europe -1 extra row generated
## also, NAs produced by rpois (42 times) and 
# In `[<-.data.frame`(`*tmp*`, lpti$PSCR == country, , value = structure(list( ... :
#provided 51 variables to replace 47 variables
E10 <- ECDC %>% addSimVars(minVal = 100)

# check simulations. 
view( simulGrow(JHH[JHH$PSCR =='Belgium',] , country = 'Belgium',minVal = 100, nrRows= 2) )
View(addSimVars(JHH[JHH$PSCR =='Belgium',] , countries = 'Belgium',minVal = 100, nrRows = 2) )

#check paths
graphit( ECDC, ECDCRegios$`ECDC world6`, yline = 100, savename = 'test')
#check data limitations
curGraph('GR', myfolder1 = 'June and beyond', from='2020-06-01', lpdf = JHH, regions = JHHRegios, graphlist=myGraphNrs)
curGraph('GR', myfolder1 = 'May and beyond', from='2020-05-01', lpdf = JHH, regions = JHHRegios, graphlist=myGraphNrs)
curGraph('GR', myfolder1 = 'May', from='2020-05-01', until="2020-05-31", lpdf = JHH, regions = JHHRegios, graphlist=myGraphNrs)

JHH0B <- JHH0[JHH0$PSCR=='Belgium',]
JHHB <- JHH0[JHH0$PSCR=='Belgium',] %>% estimateDoublingDaysOneCountry("Belgium")#
JHHB <- JHH0 %>% addDoublingDaysperCountry() %>% view
# %>% [JHH0$PSCR%in%c('Belgium','Netherlands','France'),]
addSimVars('Belgium',minVal=100,ext='_backsim')# %>% 
view(JHH[JHH$PSCR=='Belgium',])
view(ECDC[ECDC$PSCR=='Belgium',])
#test missing parameters
d=23;r=2
test<- function(d,r){
  print(myPath)
  if (missing (r)&!missing(d)) r <- 2^(1/d)
  if(missing(d)&!missing (r)) d  <- -log2(r)
  if (missing (d)) print('d still missing ='% %d) else print('d not missing'% %d)
  if (missing (r)) print('r still missing ='% %r) else print('r not missing'% %r) 
  myPath <- myPath %//% 'current'
  poop<- 'oo'
  f()
}
f <- function () print (poop)
test(d=5)
myPath
test(r=1.14)
test()

# test countries appearing in several regions
isdouble <- function (country,regiolist) {
  sum(unlist(llply(regiolist, function (charvect) country %in% charvect)))
}
isdouble('US',regios)
checkdouble <- function(country,regiolist){
  which(unlist(llply(regiolist, function (x) country %in% x)))}
checkdouble('USA',JHHRegios)

checkdouble(c('USA','Netherlands'),JHHRegios)
isdouble(c('Belgium','Netherlands'),regios)
length(JHHRegios)

#Check colors and palettes
profvis(JHH %>% addSimVarsOneCountry("Belgium",minDate = "2920-05-01", ext = '_endsim'))
#check colors
graph3Dard_fia (JHH,regios$continents)

graph6Dardcra_fiyl(JHH,"Idaho,US")
graphDddp_fyl(JHH,countries="New York,US")
graphDccp_fyl(JHH,countries="Belgium")
graphDccprr_fyl()
verbose=2
graphDccprr_fyl(JHH,"Netherlands")
graphDddp_fyl(JHH,"Netherlands")
graphit(ECDC,regios$Vincent,"deaths missed", yvars = "deaths",xvar = 'day') %>% View
