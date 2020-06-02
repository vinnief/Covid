source("requirements.R")
source('definitions.R')
#from loaddata: trial to make the loading only happen if it gives new data
#while((Sys.time()>Sys.Date()% % "22:00:00")| max(JHH$Date)<Sys.Date()-1 ) {
#   source("loaddata.R")
#   if (max(JHH$Date)< Sys.Date()-1)     {
#    print( "failed to get yesterday's values ") 
#   for (i in 1:12){
#    print(Sys.time())
#   Sys.sleep(600)
#}   } }

#why is spain gone? 
view(graphit(ECDC,c('USA',"Spain"),facet='PSCR')) #,"France"
View(graphit(ECDC,c('USA',"Belgium","Spain",'Italy'),facet='PSCR')) #,"France" #spain disappears! 
graphit(ECDC,c("Belgium","Spain",'Italy',"France")) #,"France" #spain disappears! 
graphit(JHH,c("Belgium","Spain",'Italy',"France"),facet='PSCR') #,"France"
graph6Dardcra_fiyl(ECDC,c("Belgium","Netherlands")) #,'USA'



isdouble <- function (country,regiolist) {
  sum(unlist(llply(regiolist, function (charvect) country %in% charvect)))
}
isdouble('US',regios)
checkdouble <- function(country,regiolist){
  which(unlist(llply(regiolist, function (x) country%in% x)))}
checkdouble('USA',JHHRegios)

checkdouble(c('USA','Netherlands'),JHHRegios)
isdouble(c('Belgium','Netherlands'),regios)
length(JHHRegios)

#check colors
graph3Dard_fia (JHH,regios$continents)


demoDoubling(doublingDays=1)
demoDoubling(doublingDays=2)

graph6Dardcra_fiyl(JHH,"Idaho,US")
graphDddp_fyl(JHH,countries="New York,US")
graphDccp_fyl(JHH,countries="Belgium")
graphDccprr_fyl()
verbose=2
graphDccprr_fyl(JHH,"Netherlands")
graphDddp_fyl(JHH,"Netherlands")
graphit(ECDC,regios$Vincent,"deaths missed", yvars="deaths",xvar='day')%>% View


2^(doublingdays(1)/LAGRC)




#check new version of grapit
graphit(ECDC,c('Spain',"Kazakhstan","Belgium","Netherlands","France"),facet='PSCR' , putlegend=TRUE)

graphit2(ECDC,c('Spain',"Kazakhstan","Belgium","Netherlands","France"),facet='PSCR' , putlegend=TRUE)

graphit2 <- function(lpti, countries, minval=1, ID="PSCR", xvar="Date", 
                    yvars=c("active", "recovered","deaths","confirmed"), 
                    fuzzy=FALSE, logx=FALSE, logy=FALSE, 
                    myfolder="",savename="", putlegend=TRUE, size=2,
                    returnID="PSCR", area=FALSE,position='stack',facet=FALSE, 
                    sorted=TRUE, until=Sys.Date()){
  
  lpdf<- as.data.frame(lpti)
  if (verbose>=4)print(lpti)
  if (typeof(until)=="character") until=as.Date(until,format="%Y-%m-%d")
  lastdate<- min(max(lpdf$Date),until)
  if (missing(countries)) {
    countries<- unique(lpdf[[returnID]])
    if(length(countries> 40)) return(print('too many countries, you wont see anything. Please select less countries'))
  }
  
  countries<- lpdf%>% findIDnames(testIDnames=countries,searchID=ID,fuzzy=fuzzy,returnID=returnID) #}
  ID<- returnID
  lpdf <- lpdf%>% filter((confirmed>=minval)&(PSCR %in% countries)&Date<=until)  
  
  y_lab <- paste(sort(yvars),collapse=" & ")% %ifelse(logy,"(log)","")
  if (str_length(y_lab)>80) 
    y_lab<- paste(initials(sort(yvars)),collapse="&") % % 
    ifelse(logy,"(log)","")
  mytitle<- format(lastdate,format="%Y%m%d")% %"C19"% % savename% % y_lab% %
    "by"% %xvar% %"for"% % minval%+%"+"% %"confirmed"
  
  if (nrow(lpdf)==0 ) {if (verbose>=4) {print('graphit' % %mytitle% %" No data")}
    return()  }
  lpdf <- lpdf%>%
    dataprep(ID=ID,minval=minval, xvar=xvar,yvars=yvars,logx=logx,logy=logy, sorted=sorted)
  if (verbose >=5){print ('graphit columns left');print( names(lpdf))}
  if (nrow(lpdf)==0| all(is.na(lpdf[,xvar]))|all(is.na(lpdf[,yvars])))
    return(if (verbose>=5) print ('graphit' % % paste(mytitle, "Too little data to graph. Maybe lower the mininum value, take more regions?")))
  
  lpdf<- lpdf%>% 
    melt(lpdf ,id=c(ID,xvar),measure.vars=yvars,
         variable.name="variable", value.name="count")%>%
    mutate ( mygroup=PSCR %, % variable,
             variable=factor(variable, levels = yvars))%>% drop_na()#
  if (verbose>=7) {print('graphit summary pdf:');print(summary(lpdf))}
  
  if (facet=='variable') lpdf$mygroup<- lpdf[[ID]] 
  else if (facet==ID) lpdf$mygroup <- lpdf$variable
  nrgroups<- length(unique(lpdf$mygroup))
  if (verbose>=5) print( 'graphit' % % parse(text=substitute(xvar))% %"from"% % 
                           min(lpdf[,xvar])% % "to"% %max(lpdf[,xvar]) %, % 
                           "group by "% % lpdf$mygroup[1]%, % "facets" % % facet)
  len<- length(unique(lpdf[,ID]))
  myplot<- ggplot(lpdf, aes_string(y="count",x=xvar,group='mygroup',
                                   color= ifelse(len==1,  
                                                 'variable' , 
                                                 ifelse(facet==ID,'variable',ID))
  ),na.action=na.omit)
  
  if(area){posalpha<- ifelse(position=='identity', 0.4, 1)
  myplot<- myplot + geom_area(aes_string(
    color=ifelse(len==1 |facet==ID, 'variable' ,'mygroup'),
    fill=ifelse(len==1|facet==ID,  'variable' , 'mygroup')), 
    position = position,alpha= posalpha)
  myscale_fill<- scale_fill_manual(values = c("red", "green","black","darkorange","lawngreen"))
  if (nrgroups<=2) scale_f<- scale_fill_manual(values = c("lawngreen", "cyan"))#,"black","darkorange","lawngreen"))
  myplot<- myplot+myscale_fill+  
    scale_color_manual(values = c("red", "green","black","darkorange","lawngreen"))
  }else {
    
    myplot<- myplot+  #line plot
      geom_line(alpha=0.3,size=size*0.7)+
      geom_point(size=size, aes_string(   shape='variable'))+
      if(!putlegend| facet==FALSE) 
        geom_dl(aes_string(x=xvar,y="count",  label='mygroup'),      
                           method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 1.2))
    if (length(unique(lpdf$variable))<=6 ) 
      myplot<- myplot + scale_shape_manual(values = c(0,1,3,2,10,5,6)) #shape="\u2620"
    if (nrgroups<=6){
      myscale<- scale_color_manual(
        values=c("red", "green", "black","orange","lawngreen","tomato"), #darkorange darkgreen
        guide= ifelse(putlegend,"legend",FALSE))
    }else if(nrgroups<13) {
      palette=ifelse (nrgroups <8, "Dark2","Paired") #Spectral Set2  
      myscale<- scale_color_brewer(palette=palette)
    } else myscale<-scale_color_discrete(guide= ifelse(putlegend,"legend",FALSE))
    myplot<- myplot +  myscale 
  }  
  
  if (!isFALSE(facet)) {
    myplot<- myplot+ facet_wrap(as.formula(paste("~",facet)), strip.position="bottom")}
  if(xvar=="Date") myplot<- myplot+scale_x_date(labels = date_format("%d-%m"))
  myplot<-myplot +  ylab(y_lab)+
    xlab(paste(xvar, ifelse(logx,"(log scale)","")))+ 
    ggtitle(mytitle) +  theme_tufte()+      #change
    guides(col = guide_legend(nrow=30, ncol = min(2,(nrgroups-1) %/% 30+1)))  
  
  breaks <- 10^(-10:10)
  minor_breaks <- rep( 1:5, 21)*(10^rep(-10:10, each=5))
  if(logy) myplot<- myplot+scale_y_continuous(trans='log10',breaks = breaks, minor_breaks = minor_breaks)+
    annotation_logticks() 
  if(logx) myplot<- myplot+scale_x_continuous(trans='log10',breaks = breaks, minor_breaks = minor_breaks)
  myplot<- myplot+ theme(
    axis.text = element_text(color = "blue",angle = 45,
                             hjust = 1,vjust = 0.5 , size = rel(.8)), #change 
    strip.background =element_rect(fill="white"),    
    strip.text = element_text(color = 'black'))
  
  if (savename!="") {
    if(facet==FALSE) savename<-  paste(savename,"all-in-one")
    if(area) savename<- paste(savename,"area plot")
    if (myfolder=="") { myfolder<- sort(initials(yvars))% %'by'% %xvar}
    if(area)myfolder<- myfolder % % "area plot"
    if (logy) myfolder <- myfolder % %'log scale'
    if(facet==FALSE) myfolder<-  paste(myfolder,"all-in-one")
    if (verbose>= 3) print("graphit making plot" % % myfolder %+% "/" %+% mytitle)
    myplot<- myplot+ theme(text=element_text(size=20))
    mypath<- paste("G:/My Drive/Covid19_plots",lastdate,sep="/") 
    if(myfolder!="") mypath<- paste(mypath,myfolder,"",sep="/")
    if (!dir.exists(mypath)) dir.create(mypath,recursive=TRUE)
    png(filename=mypath%+% mytitle %+%".png", width=1600,height=900)
    on.exit(while(!is.null(dev.list())) dev.off() )
    
    print(myplot)
    #)
    dev.off()
  }else {
    #tempwarn<- getOption("warn")
    #options(warn=-1)
    #on.exit(options(warn=tempwarn))
    print(myplot)}#+theme(title = element_text(size = 11)))}
  invisible(lpti)
}# 







total.tibble<- function(lpt=JHH,ID=Country.Region,
                        varnames=c('confirmed',#deaths,
                                   'recovered')){
  lpttot<-lpt%>%
    group_by({{ID}},Date)
  lpttot<- cbind(
    #lpttot%>% summarize(newPSCR:=aggreg(PSCR),
    #          newCountry.Region=aggreg(Country.Region),
    #          newProvince.State=aggreg(Province.State)), #!! mean_name := mean(!! expr)
    lpttot%>% summarize_at(.vars=!!!varnames, .funs=colSums)
  )
  #confirmed:=sum(confirmed),
  #active:=sum(active),
  #recovered=sum(recovered),
  #deaths=sum(deaths),
  #colSums(.[,])
  #if (!!ID=="PSCR")lpttot%>% rename(Coun)
}
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(PSCR)%>% view()
#JHH[JHH$Country.Region==c('Netherlands'),]%>%total.tibble(Country.Region)%>% view()
