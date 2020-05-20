#deprecated functions


##make sure we have the right column names. This version not used. 
multigrep<- function( searchlist,inlist,ignorecase=FALSE){
  unlist(llply(searchlist,function(a) grep(a,inlist, value=TRUE,ignore.case=ignorecase)))
}
multigrep(testcountries,unique(alldata$Country.Region),ignorecase=TRUE)
#this one finds exact names when needed


imputeRecovered<- function(lpdf=JHH,lagrc=22,lagrd=15,dothese=FALSE,redo=FALSE){
#lpdf<- pdata.frame(lpdf,index=c("PSCR", "Date"),stringsAsFactors=FALSE)
if(!('recovered' %in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
if(any(dothese)) lpdf$recovered_old<- lpdf$recovered
if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
rowstodo<- drop(is.na(lpdf$recovered)|dothese|(redo&lpdf$imputed))
if (verbose>=2) print("Imputing recovered for:"%+%
                        paste(unique(lpdf[rowstodo,][["Country.Region"]]),collapse=" / "))
if (sum(rowstodo)==0)return(lpdf)
#lpdf[rowstodo,"imputed"]<- TRUE
attr(lpdf,"imputed")<- "lagrc=22, lagdc=15"
lpdf<- lpdf%>% group_by(PSCR)%>% 
  mutate_cond( rowstodo,imputed=TRUE )%>%      
  mutate_cond( rowstodo,recovered = #max(recovered,
                 dplyr::lag(confirmed,lagrc)-  dplyr::lag(deaths,lagrd)) ##{{}} ipv !! ? 
#,na.rm=TRUE)
#lpdf[rowstodo,"recovered"]<-  dplyr::lag(lpdf[rowstodo,"confirmed"],lagrc)- 
#                           dplyr::lag(lpdf[rowstodo,"deaths"],lagrd)
#lpdf
}

addtotals<- function(lpdf=JHH,ID='PSCR'){
  lpt<- 
    #just to be sure, that if i do it twice i dont get double counts. 
    #And omit USA as country, as we have the individual states already. 
    lpdf[! lpdf[[ID]] %in% c("South America", "Asia", "Africa", "Europe","China","Australia","Canada","USA","US","World"),] 
  print("Deprecated. use extravars first, then addtotals2, which does not call extravars several times. ")
  World<- unique(lpt[[ID]])
  varnames=c("confirmed","recovered", "deaths","population",'recovered_imputed',"active_imputed") #use this function AFTER imputing!
  rbind(lpdf, 
        lpt%>% total(World ,ID=ID,newrow="World", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Europe,ID=ID,newrow="Europe", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Africa,ID=ID,newrow="Africa", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$Asia,ID=ID,newrow="Asia", varnames= varnames)%>%
          extravars2,
        lpt%>% total(regios$SAmerica,ID=ID,newrow="South America",varnames= varnames)%>%
          extravars2,
        lpt %>% totals(c("US","China","Australia", "Canada"),
                       ID="Country.Region", varnames= varnames)%>% extravars2
  )
}


ma.diff.lpdf<- function(lpdf,id="PSCR", varnames=c("confirmed","active","recovered", "deaths"),prefix="new_",n=3){
  ans<- ddply(lpdf, id, 
              function (lpdf){
                data.frame(llply(lpdf[,varnames], 
                                 function(a){c(NA,ma(base::diff(a),n))}
                ))
              }
  )
  ans[,id]<- NULL
  names(ans)<- paste(prefix,varnames,sep="")
  ans
}



addrownrs<-function(lpdf,counter="day", sortby="") {
  if (sortby!="") lpdf[order(lpdf[,sortby]),]
  lpdf[,counter]<-as.numeric(row.names(lpdf))
  return(lpdf)
}# two problems: if rownames change to chr, errors occur. if they get out of order because of a sort, the graphs will be a mess. 
addcounter<-function(lpdf=alldata,id="PSCR",counter="day"){
  lpdf[,counter] <- 0 #just to add a column first
  lpdf<- ddply(lpdf,id, function(lpdf){lpdf[,counter]<- lpdf[,counter]<-as.numeric(row.names(lpdf));lpdf} )
  
  return(lpdf)
}

#JHH <-  JHH0 %>%
#          makeGroups( Regiolist= regios)%>% 
#          addPopulation() %>%
#          imputeRecovered2()%>% #correct=TRUE) %>%
#          extravars2() %>%
#          addtotals #(not efficient!)

#*Now plot* note graphit is just here for legacy reasons. it is deprecated. graphit2 will become graphit soon. 
#
#```{r}
graphit <- function(countries=unique(alldata$PSCR), minval=1, ID="PSCR", varname="confirmed",
                    lpdf=alldata, countname="counter", needfuzzy=TRUE,  logy=TRUE,
                    saveit=FALSE, legend=FALSE, size=1){
  countries<- findIDnames(countries,ID,lpdf,needfuzzy)
  lpdf<- addcounter(
    datasel(countries,minval,var=varname,id=ID, lpdf=lpdf, fuzzy=FALSE),
    ID,countname)
  #for (varname in varnames)
  lpdf[lpdf[,varname]==0,varname]<- NA
  myplot<- ggplot(lpdf,aes_string(x=countname,y=varname,color=ID,group=ID)) +  
    geom_line(size=size, alpha=0.2)+geom_point(size=0.7*size,shape=1)+
    geom_dl(aes_string(label = ID) , method = list(dl.trans(x = x + 0.2),
                                                   "last.points", cex = 0.8))+
    ylab(paste(varname, ifelse(logy," (log scale)","")))+
    xlab("day")+
    ggtitle(paste("Covid-19 evolution after the first",minval,varname)) +
    theme_light() + #
    theme(plot.title = element_text(size = 12, face="bold"))
  
  if (length(countries)<12) 
    myplot<- myplot + scale_color_brewer(palette="Spectral",guide = ifelse(legend,"legend",FALSE)) 
  else myplot<- myplot + scale_color_discrete(guide = ifelse(legend,"legend",FALSE))
  if(logy) myplot<- myplot+scale_y_continuous(trans='log2')
  #if (saveit) savePlot(paste("plots/",varname,format(Sys.Date(),format="%Y%m%d")," in ( ", 
  #                     paste(findIDnames(countries,ID,lpdf,needfuzzy),collapse=", " )," )"),
  #                    type= "png")
  return(myplot)
}
#scale_color_brewer(palette="Set3",guide = ifelse(legend,"legend",FALSE)) #Dark2
#geom_text(data = lpdf, aes_string(label = ID,color = ID,x =Inf,y =max(value) ), hjust = -10) 
#
# extravars extract 
if (FALSE){#"pdata.frame" %in% class(lpdf)){
  lpdf$new_confirmed<- base::diff(lpdf$confirmed)
  lpdf$new_deaths<-    base::diff(lpdf$deaths)
  lpdf$new_recovered<- base::diff(lpdf$recovered)
}else{
  warning(paste(quote(lpdf) ,"Not PLM, first date of each ID is going to have wrong 'new_' counts."))
  lpdf$new_confirmed<- diff.lpdf(lpdf$confirmed)
  lpdf$new_deaths<- diff.lpdf(lpdf$deaths)
  lpdf$new_recovered<- diff.lpdf(lpdf$recovered)
  }

#graphit(5) extract: 
  #svg(filename=paste("plots/",mytitle, ifelse(logy,", log scale",""),
  #                  ".svg",sep=""), width=16,height=9) #in inches?
  #print(myplot);dev.off()

#multi lines, without melt, works but has wrong legends, and colors per id only. ! 
graphit3 <- function(countries=NULL, minval=1, id="Country.Region", xvar="day", 
                     yvars=c("confirmed", "recovered"), 
                     lpdf=covid19, fuzzy=TRUE,logx=FALSE, logy=TRUE, savename="", 
                     putlegend=TRUE, size=3,returnid="PSCR"){
  lpdf<- dataprep1(countries,minval,id,xvar,yvars,lpdf,fuzzy,logx,logy,returnid=returnid)
  id<-returnid
  extratext<- paste("by",xvar, "for ", paste(minval,"+ ",yvars[1],sep=""))
  myplot<- ggplot(lpdf) 
  for (varname in yvars) {myplot<- myplot +  
    geom_line(aes_string(x=xvar,y=varname,color=c(id),group=c(id)),alpha=0.2,size=size)+
    geom_point(aes_string(x=xvar,y=varname,color=c(id),group=c(id)),
               size=0.7*size,shape=match(varname,yvars))+
    geom_dl(aes_string(x=xvar,y=varname,color=id,label = id) , 
            method = list(dl.trans(x = x+0.1 ,y=y+0.1),"last.points", cex = 1.2))
  }  
  mytitle<- paste("Covid-19",format(Sys.Date(),format="%Y%m%d"), 
                  savename, paste(yvars,collapse="&"),extratext)
  if (length(countries)<13) {
    myscale<- scale_color_brewer(palette="Paired",guide = ifelse(putlegend,"legend",FALSE)) #"Spectral
  }  else myscale<- scale_color_discrete(guide = ifelse(putlegend,"legend",FALSE))
  myplot<-myplot + 
    ylab(paste(paste(yvars,collapse=", "), ifelse(logy,"(log scale)","")))+
    xlab(paste(xvar,                       ifelse(logx,"(log scale)","")))+ 
    ggtitle(mytitle) +
    theme_light() + theme(plot.title = element_text(size = 20, face="bold")) +
    myscale
  if(logy) myplot<- myplot+scale_y_continuous(trans='log2')
  if(logx) myplot<- myplot+scale_x_continuous(trans='log2')
  if (savename!="") {
    png(filename=paste("plots/",
                       mytitle, ifelse(logy,", log scale",""),
                       ".png",sep=""),
        width=1600,height=900)
    print(myplot);dev.off()
    #svg(filename=paste("plots/",mytitle, ifelse(logy,", log scale",""),
    #                   ".svg",sep=""), width=16,height=9) #in inches?
    #print(myplot);dev.off()
    #print(paste("Plot saved:",mytitle))
  }else return(myplot)
}
writeRegiographs<- function(Regionlist,minval=100,nrs=1:4, id="PSCR",lpdf=covid19, until=Sys.Date()){
  for (i in 1:(length(Regionlist))){
    if (verbose>=2 ) print(paste(Sys.time(),"Region",i,Regionlist[[i]][1]))
    ids<-(findidnames((Regionlist[[i]]),searchid=id,lpdf=lpdf, fuzzy=FALSE,returnid="PSCR"))
    graphs(ids,Regionlist[[i]][1],minval,nrs, id,lpdf,until=until)
  }
}