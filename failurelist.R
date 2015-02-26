## Code to convert exported FM file to find failure reasons.
# Needs to grab just failures
# sort via reason
# calculate percent of each reason
library(dplyr)
library(xlsx)
filename <- readline("What is the file called? 
                     Don't forget the extension... ")

outputname <- readline("What do you want the output to be called?... ")
outputname <- paste(outputname, ".xlsx", sep="")

raw_or_count <- readline("Do you want a counted list or a raw list? 
                         1 for counted, 2 for raw... ")

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

if (raw_or_count == 1) {
        by_Reason <- group_by(sorted, Failure_Reason)
        failure_counts <- summarize(by_Reason, number_of_failures = n())
        class(failure_counts) <- "data.frame"
        write.xlsx(failure_counts, 
                   file=outputname, 
                   sheetName="Counted",
                   row.names=FALSE)
} else {
        write.xlsx(sorted, outputname,
                   sheetName="Raw",
                   row.names=FALSE)
}




