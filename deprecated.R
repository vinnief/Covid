#deprecated functions




correctgeos <-function(wpdf){ #get rid of the counties on the dataset before 20200325
  wpdf$County <- substr(wpdf$Province.State , 1,regexpr(",",wpdf$Province.State)-1)
  wpdf$PS<-( sub("[A-Z,a-z' ]+, ", "",wpdf$Province.State)) #as factor or ordered
  ps<-unique( wpdf[!grepl(",",wpdf$Province.State,fixed=TRUE &wpdf$Province.State!=""),"Province.State"])
  ps[1]==""
  ps<- rbind(US,data.frame(State=ps,Code=ps))
  wpdf <- merge(wpdf,ps, by.x="PS",by.y="Code", all.x=TRUE)
}

#  alldata[alldata$Country.Region %in% Europe, varname] <-"Europe"
#  alldata[alldata$Country.Region %in% EU, varname] <-"EU"
#  alldata[alldata$Country.Region %in% EFTA, varname] <-"EFTA"
#  alldata[alldata$Country.Region %in% Africa, varname] <-"Africa"
#  alldata[alldata$Country.Region %in% MENA, varname] <-MENA[1]
#  alldata[alldata$Country.Region %in% SAmerica, varname] <-SAmerica[1]
#  alldata[alldata$Country.Region %in% CAsia, varname] <-CAsia[1]
#  alldata[alldata$Country.Region %in% SouthEastAsia, varname] <- SouthEastAsia[1]
#  alldata[alldata$Country.Region %in% CIS, varname] <-CIS[1]
#  alldata[alldata$Country.Region %in% c("US","Canada","Mexico"), varname] <-"North America"
#  alldata[alldata$Country.Region %in% SAsiaIO, varname] <-"South Asia & Indian Ocean"




##make sure we have the right column names. This version not used. 
multigrep<- function( smalllist,biglist,ignorecase=FALSE){
  unlist(llply(smalllist,function(a) grep(a,biglist, value=TRUE,ignore.case=ignorecase)))
}
multigrep(testcountries,unique(alldata$Country.Region),ignorecase=TRUE)
#this one finds exact names when needed
#
addrownrs<-function(lpdf,counter="day", sortby="") {
  if (sortby!="") lpdf[order(lpdf[,sortby]),]
  lpdf[,counter]<-as.numeric(row.names(lpdf))
  return(lpdf)
}# two problems: if rownames change to chr, errors occur. if they get out of order because of a sort, the graphs will be a mess. 
addcounter<-function(lpdf=alldata,id="CRPS",counter="day"){
  lpdf[,counter] <- 0 #just to add a column first
  lpdf<- ddply(lpdf,id, function(lpdf){lpdf[,counter]<- lpdf[,counter]<-as.numeric(row.names(lpdf));lpdf} )
  
  return(lpdf)
}


#*Now plot* note graphit is just here for legacy reasons. it is deprecated. graphit2 will become graphit soon. 
#
#```{r}
graphit <- function(countries=unique(alldata$CRPS), minval=1, ID="CRPS", varname="confirmed",
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


#multi lines, without melt, works but has wrong legends, and colors per id only. ! 
graphit3 <- function(countries=NULL, minval=1, id="Country.Region", xvar="day", 
                     yvars=c("confirmed", "recovered"), 
                     lpdf=covid19, fuzzy=TRUE,logx=FALSE, logy=TRUE, savename="", 
                     putlegend=TRUE, size=3,returnid="CRPS"){
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
