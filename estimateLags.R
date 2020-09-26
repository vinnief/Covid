#source("requirements.R")
source("definitions.R")
# # Maximum cross correlation frequency per country. 
# News evidence suggests this should be around 21 or at least 15. It was much less on the data of Jan- April 2020
#
findMaxCCF(myPSCR = "Italy")
findMaxCCF(myPSCR = "Hubei,China")
findMaxCCFs(myPSCR = "Taiwan*")
# Italy 28, Hubei 19,  and Taiwan 26 days, in the data until 2020-09-22. 
#
# ## Recovered lagging Confirmed by...
rclags <- findMaxCCFs("new_recovered","new_confirmed")
rclags <- rclags[!is.nan(rclags$cor), ]
hist(rclags$lag, plot = TRUE, breaks = 20)
if (every( rclags[rclags$lag >= 0,"lag"], is.numeric)) 
   rclags[rclags$lag >= 0,][["lag"]] %>% median
# It seems that as of 2020-09-22, the median is now 14 days, with the bulk of the lags are between 10 to 20 days, with secondary peaks at zero, and at 26-27 days. Some 8 countries still exhibit negative lags. 
#
# In April 2020, it seemed 21 days was the mode time to recover since being confirmed infected for Taiwan*. By end of September, it became 26 days for Taiwan. 
#
# Singapore tested the first 100 patients, and all recover by the 31st day. Some the first day! 
# https://towardsdatascience.com/visual-notes-from-singapores-first-100-fully-recovered-covid-19-patients-aad7f2e1d0a0
# majority of patients among the first 100 fully recovered cases were confirmed within 14 days of the reported onset of symptom
#
# Sweden claims patients all recover by the 6th week, 42 days. Ref(?) 
#
rclags[order(rclags$lag,decreasing = TRUE),][1:20,]

rclags[order(rclags$lag,decreasing = FALSE),][1:15,]

# ## Recovered lagging Deaths by ...
verbose = 1
rdlags <- findMaxCCFs("new_recovered","new_deaths")
rdlags <- rdlags[!is.nan(rdlags$cor),]
hist(rdlags$lag, plot = TRUE,breaks = 20)
# It seems recovery lags deaths , but a lot of regions have lag around zero still. 

#rdlags[rdlags$lag<=5&rdlags$lag>=0,"PSCR"]
rdlags[rdlags$lag >= 0,][["lag"]] %>% median
rdlags[rdlags$lag > 5,][["PSCR"]]
rdlags[rdlags$lag > 10,][["PSCR"]]
rdlags[order(rdlags$lag,decreasing = TRUE),][1:20,]
# ## Deaths lagging Confirmed by ...
dclags <- findMaxCCFs("new_deaths","new_confirmed")
dclags <- dclags[!is.nan(dclags$cor),]
hist(dclags$lag, plot = TRUE,breaks = 20)
dclags[dclags$lag >= 0,][["lag"]] %>% median
dclags[order(dclags$lag,decreasing = TRUE),][1:20,]
# ## Discussion: 
# bug: instead of cor=NA, and lag=NA, we obtain cor =NAN, lag =-28 when var1 is missing. 
#
# So I suggest taking lags 42 and 36 (i.e. assume people die within 6 days if they die, which (in April) was the median of positive rd lags. 
# By September, the median DC lag equals 6. 
#
# Note we do not take the median positive RC lag, which equals 14 (JHH data until 2020-05-18), 
# if we take a smaller lag between r and d, then d might start rising faster than c, which leads to r decreasing over time and that is not possible.the New York,US data we have imputed suffers from this idiosyncracy.  )

LAGDC; LAGRD;LAGRC
