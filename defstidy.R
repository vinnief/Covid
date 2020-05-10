source("requirements.R")
source("definitions2.R")

ecdc0 <- makeecdc()
verbose=2
ecdc<- ecdc0 %>% imputeRecovered2() %>% ungroup()%>% extravars2

View(ecdc)

#ecdcdata$CRPS<- ecdcdata$CRPS2
ecdcdata$CRPS2<- sortCRPS(as.data.frame(ecdcdata) )
ecdcdata$CRPS1<- sortCRPS1(ecdcdata)
ecdc[is.na(ecdc$CRPS),]
ecdc[ecdc$CRPS2!=ecdcdata$CRPS1,c("CRPS",'CRPS1','CRPS2','confirmed' )]

ecdtest<- ecdcdata  %>% 
  filter( is.na(CRPS2) | (CRPS1!= CRPS2)) %>% 
  select(CRPS, CRPS1,CRPS2, Date,confirmed)#  %>% 
ecdtest%>%  summary("CRPS")


eWorldCRPSList<- unique(ecdcdata$CRPS)
eWorldCRPSList  # from here bugs exist
graph0("France", lpdf=as.data.frame(ecdcdata))

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
#
#
#
# dynamic varnames in select: ## !!varname := 
# !!varname does not work the same as confirmed in arrange(desc(!!varname))
# arrange( desc(eval(parse(text=substiture(!!varname)))))  # works. 
# CRAZY!!
# {{varname}} might work
