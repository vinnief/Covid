This is the analysis part of the linegraphs. in R and Julia
the Julia part didnt work yet, problems with reinstalling. will come

approach: 
the confirmed panel data is in csv format, row=country, column= date,
and i thought it needed transposing, because i want to get rid of the country's data values below a minimum. If we continue doing that, 4 first columns need to be converted into a possible column label. I do that by deleting the latitude and longitude, 
and by concatenating province and country levels into one. 
the premanipulations (concatenation, deletion and transposition) are done in Excel for the moment, after which i place it in this directory with the date in the name. 
