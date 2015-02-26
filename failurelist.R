## Code to convert exported FM file to find failure reasons.
# Needs to grab just failures
# sort via reason
# calculate percent of each reason
library(dplyr)
library(xlsx)
filename <- readline("What is the file called? Don't forget the extension... ")

outputname <- readline("What is the output file called?...  ")
outputname <- paste(outputname, ".xlsx", sep="")

rawdata <- read.table(filename, 
                      sep="\t", 
                      stringsAsFactors=FALSE, 
                      na.strings = "")

colnames(rawdata) <- c("Failure_Reason", 
                       "Five_Prime_mod", 
                       "Three_Prime_mod", 
                       "sequence")

raw_tbl_df <- tbl_df(rawdata)

failures <- filter(rawdata, !is.na(Failure_Reason))

sorted <- arrange(failures, Failure_Reason, Five_Prime_mod, Three_Prime_mod)

by_Reason <- group_by(sorted, Failure_Reason)
failure_counts <- summarize(by_Reason, number_of_failures = n())
class(failure_counts) <- "data.frame"
date <- Sys.Date()


countedname <- paste("Counted", "for", date, sep=" ")
rawname <- paste("Raw", "for", date, sep=" ")

write.xlsx(failure_counts,
           file=outputname, 
           sheetName=countedname,
           row.names=FALSE,
           append=TRUE)

write.xlsx(sorted, 
           outputname,
           sheetName=rawname,
           row.names=FALSE,
           append=TRUE)




