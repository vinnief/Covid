source("requirements.R")




#### fast ccf
if(!require(collapse)){installed.packages("collapse");require(collapse)}

findMaxpsccf<- function(var1,var2, lpdf=covid19){
  d <- psccf(lpdf,t=Date,cols=c(var1,var2),by =CRPS,lag.max=30, plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(res$cor),]
  return(res_max$lag)
} 
findMaxpsccf("new_recovered","new_confirmed", covid19)


##############
#break labels in graph across lines to facet better: 
#for readable labels on facet titles, split them into lines with
#reformat <- function(x,splitt=", ",lab="\n"){ sapply(x, function(c){ paste(unlist(strsplit(as.character(c) , split=splitt)),collapse=lab) }) }
#dataset$variable <- factor(dataset$variable, labels=reformat(dataset$variable, lab='\n')
#And upon facetting, all labels will be very readable:
# ggplot(data=dataset, aes(x,y)) + geom_point() + facet_grid(. ~ variable)
#that is for factor labels. we need it for variable lables. 
