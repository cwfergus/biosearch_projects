library(dplyr)
library(xlsx)
library(stringr)
source('failurelistfunctions.R')

filename <- readline("What is the data file called? Don't forget the extension... ")

outputname <- readline("What is the output file called?...  ")
outputname <- paste(outputname, ".xlsx", sep="")

rawdata <- read.table(filename, 
                      sep="\t", 
                      stringsAsFactors=FALSE, 
                      na.strings = c("", " "),
                      quote="",
                      comment.char="")

colnames(rawdata) <- c("Failure_Reason", 
                       "Five_Prime_mod", 
                       "Three_Prime_mod", 
                       "sequence",
                       "sequence_ID")
raw_tbl_df <- tbl_df(rawdata)

seq_amount <- nrow(raw_tbl_df)

not_passed <- filter(raw_tbl_df, !is.na(Failure_Reason))

not_passed_amount <- nrow(not_passed)

#rm(rawdata)

not_passed <- clean_up()


only_failed <- not_failed()

#rm(raw_tbl_df)

clean_failure_reassigns <- failure_aggregation()

failures_and_reassigns <- nrow(clean_failure_reassigns)

clean_failure_reassigns$Failure_Reason <- gsub("Reassigned", NA, clean_failure_reassigns$Failure_Reason)
clean_failure <- filter(clean_failure_reassigns, !is.na(Failure_Reason))

failed_count <- nrow(clean_failure)

reason_counts <-
        clean_failure %>%
        group_by(Failure_Reason) %>%
        summarize(number_of_failures = n()) %>%
        arrange(desc(number_of_failures), Failure_Reason)

seqID_counts <- 
        clean_failure %>%
        group_by(sequence_ID) %>%
        summarize(number_of_failures_by_ID = n()) %>%
        arrange(desc(number_of_failures_by_ID))

mod_reason_counts <- 
        clean_failure %>%
        group_by(Five_Prime_mod, Three_Prime_mod) %>%
        summarize(failure_per_mod = n()) %>%
        group_by() %>%
        arrange(desc(failure_per_mod))

#rm(clean_failure)

failure_list <- 
        only_failed %>%
        arrange(Failure_Reason)


date <- Sys.Date()
countedname <- paste("Reason Counts for", date, sep=" ")
seqname <- paste("Sequence ID counts for", date, sep=" ")
modname <- paste("Mod Counts for", date, sep=" ")
listname <- paste("Failure list for", date, sep=" ")

class(reason_counts) <- "data.frame"
class(mod_reason_counts) <- "data.frame"
class(seqID_counts) <- "data.frame"
class(failure_list) <- "data.frame"

summary <- data_summary()

write.xlsx(summary,
           file=outputname, 
           sheetName="summary",
           row.names=FALSE,
           append=FALSE)

write.xlsx(reason_counts,
           file=outputname, 
           sheetName=countedname,
           row.names=FALSE,
           append=TRUE)

write.xlsx(seqID_counts,
           file=outputname, 
           sheetName=seqname,
           row.names=FALSE,
           append=TRUE)

write.xlsx(mod_reason_counts,
           file=outputname,
           sheetName=modname,
           row.names=FALSE,
           append=TRUE)

#write.xlsx(failure_list, 
 #          outputname,
  #         sheetName=listname,
   #        row.names=FALSE,
    #       append=TRUE)


#rm(list=ls())



