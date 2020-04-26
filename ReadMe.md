This is the analysis part of the timeline and area graphs. in R and Julia. The Julia part doesn't work yet, problems with reinstalling 1.3 over 1.1. will come I hope. 

Approach: 
the confirmed panel data is in csv format, row=country, column= date,
and we melt it into a long version, with on column per count vatiable, and Data being a row index. 
A CRPS key is constructed out of pasting Province.State and Country.Region together. 