source("requirements.R")
source('definitions.R')
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
