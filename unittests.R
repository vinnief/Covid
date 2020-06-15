#Unittests #
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