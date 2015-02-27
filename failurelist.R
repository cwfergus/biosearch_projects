## Code to convert exported FM file to find failure reasons.
# Needs to grab just failures
# sort via reason
# calculate percent of each reason
library(dplyr)
library(xlsx)
library(stringr)
filename <- readline("What is the file called? Don't forget the extension... ")

outputname <- readline("What is the output file called?...  ")
outputname <- paste(outputname, ".xlsx", sep="")

rawdata <- read.table(filename, 
                      sep="\t", 
                      stringsAsFactors=FALSE, 
                      na.strings = "",
                      quote="")

colnames(rawdata) <- c("Failure_Reason", 
                       "Five_Prime_mod", 
                       "Three_Prime_mod", 
                       "sequence")
#converts to tbl_df to enable advanced data manipulation
raw_tbl_df <- tbl_df(rawdata)

#converts all characters to lower case to enable easy manipulation
raw_tbl_df$Failure_Reason <- tolower(raw_tbl_df$Failure_Reason)
#Removes double spaces to enable easy manipulation
raw_tbl_df$Failure_Reason <- gsub("  ", " ", raw_tbl_df$Failure_Reason)
#turns all see SS number for syn info notes into NA to enable removal
raw_tbl_df$Failure_Reason <- gsub("see", NA, raw_tbl_df$Failure_Reason)
#turns all good wobbles into NA to enable removal

###raw_tbl_df$Failure_Reason <- gsub("wobble okay", NA, raw_tbl_df$Failure_Reason)

#turns all reassign notes to NA's to enable removal
raw_tbl_df$Failure_Reason <- gsub("reassign", NA, raw_tbl_df$Failure_Reason)

#turns all material notes to NA's to enable removal
raw_tbl_df$Failure_Reason <- gsub("material", NA, raw_tbl_df$Failure_Reason)

#turns all collection plates notes to NA's to enable removal
raw_tbl_df$Failure_Reason <- gsub("collection", NA, raw_tbl_df$Failure_Reason)

#removes Na values: passed, wobble, reassign, material, etc
failures <- filter(raw_tbl_df, !is.na(Failure_Reason))

#removes blank space at beginning or end of failure reason.
failures$Failure_Reason <- str_trim(failures$Failure_Reason)

#rename ms not okay notes to be the same
failures$Failure_Reason <- gsub("ms n", NA, failures$Failure_Reason)
failures$Failure_Reason[is.na(failures[,1])] <- "ms NOT okay"

#rename ms okay notes to be the same
failures$Failure_Reason <- gsub("ms o", NA, failures$Failure_Reason)
failures$Failure_Reason[is.na(failures[,1])] <- "ms okay"

#rename un checked Low yield notes to be all the same
failures$Failure_Reason <- gsub("low y", NA, failures$Failure_Reason)
failures$Failure_Reason[is.na(failures[,1])] <- "low yield"

#rename all wrong mass to be the same

sorted <- arrange(failures, Failure_Reason, Three_Prime_mod, Five_Prime_mod)

by_Reason <- group_by(sorted, Failure_Reason)
reason_counts <- summarize(by_Reason, number_of_failures = n())
class(reason_counts) <- "data.frame"
class(sorted) <- "data.frame"

by_reason_mods <- group_by(sorted, Failure_Reason, Five_Prime_mod, Three_Prime_mod)
reason_mod_counts <- summarize(by_reason_mods, failure_per_mod = n())

reason_count_arranged <- arrange(reason_counts, desc(number_of_failures))

date <- Sys.Date()


countedname <- paste("Counted", "for", date, sep=" ")
rawname <- paste("Raw", "for", date, sep=" ")

write.xlsx(reason_count_arranged,
           file=outputname, 
           sheetName=countedname,
           row.names=FALSE,
           append=FALSE)

write.xlsx(sorted, 
           outputname,
           sheetName=rawname,
           row.names=FALSE,
           append=TRUE)




