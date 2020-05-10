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
#lpdf<- ecdcdata;gridsize=5*6
makeDynRegions <- function(lpdf=JHH,gridsize=7*6) {
  a<- lpdf%>% ungroup %>% 
    filter(!(CRPS %in% c("USA","US","Australia","China","Canada","South America","Asia","Africa","World","Europe")),
           Date==max(Date))%>% 
    select( CRPS, confirmed)%>% arrange(desc(confirmed)) #done when extravars were created, and at creation of JHH already. 
  gridsize = ceiling(sqrt(length(unique(lpdf$CRPS))/8+.25)-.5) 
  gridsize= gridsize*(gridsize-1)
  #this way 8 grids of gridsize*(gridsize+1) result. 
  World1<-c("World 1",as.character(a[1:gridsize,]$CRPS ))
  World2<-c("World 2",as.character(a[(gridsize+1 ) : (2*gridsize),]$CRPS))
  World3<-c("World 3",as.character(a[(2*gridsize+1): (3*gridsize),]$CRPS))
  World4<-c("World 4",as.character(a[(3*gridsize+1): (4*gridsize),]$CRPS))
  World5<-c("World 5",as.character(a[(5*gridsize+1): (6*gridsize),]$CRPS))
  World6<-c("World 6",as.character(a[(6*gridsize+1): (7*gridsize),]$CRPS))
  World7<-c("World 7",as.character(a[(7*gridsize+1): min(nrow(a),8*gridsize),]$CRPS))
  World8<-c("World 8",as.character(a[(min(nrow(a),8*gridsize)+1):nrow(a),]$CRPS))
  a<- lpdf%>% ungroup %>% 
    filter((CRPS %in% setdiff(Europe, "Europe")),
           Date==max(Date))%>% 
    select(CRPS, confirmed)%>% arrange(desc(confirmed))
  gridsize<- 4*4
  Europe1<- c("Europe 1",as.character(a[1:gridsize,]$CRPS ))
  Europe2<-  c("Europe 2",as.character(a[(gridsize+1):(2*gridsize),]$CRPS))
  Europe3<-  c("Europe 3",as.character(a[(2*gridsize+1):(3*gridsize),]$CRPS))
  Europe4<-  c("Europe 4",as.character(a[(3*gridsize+1):min(nrow(a),4*gridsize),]$CRPS))
  
  as.list(paste("World",c("1","2","3","4","5","6","7"),sep=""))
  list( World1=World1, World2=World2, World3=World3, World4=World4, World5=World5, 
        World6=World6, World7=World7, World8=World8, Europe1=Europe1, 
        Europe2=Europe2, Europe3=Europe3)
}


imputeRecovered<- function(lpdf=JHH,lagrc=22,lagrd=15,dothese=FALSE,redo=FALSE){
#lpdf<- pdata.frame(lpdf,index=c("CRPS", "Date"),stringsAsFactors=FALSE)
if(!('recovered' %in% names(lpdf))) lpdf$recovered<- as.numeric(NA)
if(any(dothese)) lpdf$recovered_old<- lpdf$recovered
if (!"imputed"%in% names(lpdf))lpdf$imputed<-FALSE
rowstodo<- drop(is.na(lpdf$recovered)|dothese|(redo&lpdf$imputed))
if (verbose>=2) print("Imputing recovered for:"%+%
                        paste(unique(lpdf[rowstodo,][["Country.Region"]]),collapse=" / "))
if (sum(rowstodo)==0)return(lpdf)
#lpdf[rowstodo,"imputed"]<- TRUE
attr(lpdf,"imputed")<- "lagrc=22, lagdc=15"
lpdf<- lpdf%>% group_by(CRPS)%>% 
  mutate_cond( rowstodo,imputed=TRUE )%>%      
  mutate_cond( rowstodo,recovered = #max(recovered,
                 dplyr::lag(confirmed,lagrc)-  dplyr::lag(deaths,lagrd)) ##{{}} ipv !! ? 
#,na.rm=TRUE)
#lpdf[rowstodo,"recovered"]<-  dplyr::lag(lpdf[rowstodo,"confirmed"],lagrc)- 
#                           dplyr::lag(lpdf[rowstodo,"deaths"],lagrd)
#lpdf
}

ma.diff.lpdf<- function(lpdf,id="CRPS", varnames=c("confirmed","active","recovered", "deaths"),prefix="new_",n=3){
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

extravars<- function(lpdf,lagrc=0,lagdc=0){ 
  lpdf$active <- lpdf$confirmed - lpdf$deaths - lpdf$recovered
  lpdf<-lpdf[with(lpdf, order(CRPS, Date)), ]
  prefix = "new_"
  varnames=c("confirmed","active","recovered", "deaths")
  lpdf[,paste(prefix,varnames,sep="")]<- NULL 
  lpdf<- cbind(lpdf,ma.diff.lpdf(lpdf,prefix = prefix))
  lpdf$confirmed_pM <- 1000000*lpdf$confirmed/lpdf$population
  lpdf$active_pM    <- 1000000* lpdf$active  /lpdf$population
  lpdf$recovered_pM <- 1000000*lpdf$recovered/lpdf$population
  lpdf$deaths_pM    <- 1000000*lpdf$deaths   /lpdf$population
  
  lpdf$new_confirmed_pM <- 1000000*lpdf$new_confirmed/lpdf$population
  lpdf$new_active_pM    <- 1000000* lpdf$new_active  /lpdf$population
  lpdf$new_recovered_pM <- 1000000*lpdf$new_recovered/lpdf$population
  lpdf$new_deaths_pM    <- 1000000*lpdf$new_deaths  /lpdf$population
  
  lpdf$recovered_per_confirmed<- ifelse(plm::lag(lpdf$confirmed,lagrc)>0, 
                                        lpdf$recovered/plm::lag(lpdf$confirmed,lagrc), NA)
  lpdf$deaths_per_confirmed<- ifelse(plm::lag(lpdf$confirmed,lagdc)>0, 
                                     lpdf$deaths/plm::lag(lpdf$confirmed,lagdc), NA)
  lpdf$recovered_per_deaths<- ifelse(plm::lag(lpdf$deaths,lagrc-lagdc)>0,
                                     lpdf$recovered/plm::lag(lpdf$deaths,lagrc-lagdc), NA)
  
  lpdf$Date<- as.Date(lpdf$Date,format="%Y-%m-%d") #otherwise we cannot calculate the first date that confirmed is over minval. 
  lpdf$CRPS<- sortCRPS(lpdf) #the CRPS is now a factor! sorted on confirmed and CRPS! 
  lpdf
}


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

datasel.old<- function(countries=MSM, minval= 0, lpdf=JHH,  #deprecated, unused
                       varname="confirmed", id="CRPS"){
  return( lpdf[ (lpdf[,varname]>=minval)&(lpdf[,id] %in% countries) , ]) 
}
datasel<- function( lpdf=JHH, countries=MSM, minval= 0 ){ #unused
  return( filter(lpdf,(confirmed>=minval)&(CRPS %in% countries) ) )  
}


dataprep2<- function(lpdf,id, xvar, yvars, #not used, integrated into graphit again. 
                     variable.name="varname", value.name="count"){
  lpdf<- melt(lpdf ,id=c(id,xvar),measure.vars=yvars,
              variable.name=variable.name, value.name=value.name)
  lpdf[,variable.name]<- factor(lpdf[,variable.name], levels = yvars) #better for graphing and legends?
  lpdf$mygroup<- paste(lpdf[,id],lpdf[,variable.name],sep=", ")
  lpdf
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

#graphit(5) extract: 
  #svg(filename=paste("plots/",mytitle, ifelse(logy,", log scale",""),
  #                  ".svg",sep=""), width=16,height=9) #in inches?
  #print(myplot);dev.off()

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
writeRegiographs<- function(Regionlist,minval=100,nrs=1:4, id="CRPS",lpdf=covid19, until=Sys.Date()){
  for (i in 1:(length(Regionlist))){
    if (verbose>=2 ) print(paste(Sys.time(),"Region",i,Regionlist[[i]][1]))
    ids<-(findidnames((Regionlist[[i]]),searchid=id,lpdf=lpdf, fuzzy=FALSE,returnid="CRPS"))
    graphs(ids,Regionlist[[i]][1],minval,nrs, id,lpdf,until=until)
  }
}