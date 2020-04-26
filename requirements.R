if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(directlabels)){
  install.packages("directlabels")
  require(directlabels)
}
#getpackage<- function(pname){
#  if(!require(eval(name))){
#    install.packages(pname)
##    require(eval(pname))
#  }}

if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(ggthemes)) {  installed.packages("ggthemes"); require(ggthemes)}
if (!require(scales)) {  installed.packages("scales"); require(scales)}
#if(!require(ggrepel)){
#  install.packages("ggrepel")
#  require(ggrepel)
#}# note ggrepel also does labels next to lines. 
#if (!require(devEMF)){
#  install.packages('devEMF') # just once
#  require(devEMF)}

if(!require(plm)){   install.packages("plm");  require(plm)}
 
if(!require(plyr)){   installed.packages("plyr"); require(plyr)}

if(! require(reshape2)){   install.packages("reshape2");require(reshape2)}

