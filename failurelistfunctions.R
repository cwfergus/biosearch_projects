#Character Clean up function
clean_up <- function(){
        raw_tbl_df$Failure_Reason <- tolower(raw_tbl_df$Failure_Reason)
        raw_tbl_df$Failure_Reason <- gsub("  ", " ", raw_tbl_df$Failure_Reason)
        raw_tbl_df$Failure_Reason <- str_trim(raw_tbl_dffailures$Failure_Reason)
        raw_tbl_df
}

#None Failure removal
not_failed <- function(){
        raw_tbl_df$Failure_Reason <- gsub("see", NA, raw_tbl_df$Failure_Reason)
        raw_tbl_df$Failure_Reason <- gsub("reassign", NA, raw_tbl_df$Failure_Reason)
        raw_tbl_df$Failure_Reason <- gsub("material", NA, raw_tbl_df$Failure_Reason)
        raw_tbl_df$Failure_Reason <- gsub("collection", NA, raw_tbl_df$Failure_Reason)
        raw_tbl_df$Failure_Reason <- gsub("stellaris o", NA, raw_tbl_df$Failure_Reason)
        filter(raw_tbl_df, !is.na(Failure_Reason))
}

#Failure aggregation
failure_aggregation <- function(){
        only_failed$Failure_Reason <- gsub("ms n", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "ms NOT okay"
        
}