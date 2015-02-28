#Character Clean up function
clean_up <- function(){
        not_passed$Failure_Reason <- tolower(not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("  ", " ", not_passed$Failure_Reason)
        not_passed$Failure_Reason <- str_trim(not_passed$Failure_Reason)
        not_passed
}

#None Failure removal
not_failed <- function(){
        not_passed$Failure_Reason <- gsub("see", NA, not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("reass", NA, not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("material", NA, not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("collection", NA, not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("stellaris o", NA, not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("pass", NA, not_passed$Failure_Reason)
        not_passed$Failure_Reason <- gsub("wobble o", NA, not_passed$Failure_Reason)
        filter(not_passed, !is.na(Failure_Reason))
}

#Failure aggregation
failure_aggregation <- function(){
        only_failed$Failure_Reason <- gsub("ms n", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Ms NOT Okay"
        
        only_failed$Failure_Reason <- gsub("ms o", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Ms Okay"
        
        only_failed$Failure_Reason <- gsub("yield", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("yeild", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Low Yield"
        
        only_failed$Failure_Reason <- gsub("base", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Base-swap"
        
        only_failed$Failure_Reason <- gsub("wrong m", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Wrong Mass"
        
        only_failed$Failure_Reason <- gsub("flush", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Flushed"
        
        only_failed$Failure_Reason <- gsub("3 stel", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "More than 3 stellaris failures"
        
        only_failed$Failure_Reason <- gsub("extra tet", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Extra TET coupled"
        
        only_failed$Failure_Reason <- gsub("tet", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Poor TET"
        
        only_failed$Failure_Reason <- gsub("extra fam", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Extra FAM coupled"
        
        only_failed$Failure_Reason <- gsub("fam", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Poor FAM"
        
        only_failed$Failure_Reason <- gsub("biotin", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Poor Biotin Coupling"
        
        only_failed$Failure_Reason <- gsub("fluor", "Poor Fluorescense", only_failed$Failure_Reason)
        
        only_failed$Failure_Reason <- gsub("impurity", "ImPurity", only_failed$Failure_Reason)
        
        only_failed$Failure_Reason <- gsub("purity", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Poor Purity"
        
        only_failed$Failure_Reason <- gsub("n-", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "N- failure"
        
        only_failed$Failure_Reason <- gsub("syn", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Synthesis Failure"
        
        only_failed$Failure_Reason <- gsub("colum", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Column Issue"
        
        only_failed$Failure_Reason <- gsub("flp", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "No FLP"
        
        only_failed$Failure_Reason <- gsub("dmt", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "DMT left on"
        
        only_failed$Failure_Reason <- gsub("-1", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-2", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-3", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-4", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-5", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-6", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-7", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-8", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("-9", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+1", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+2", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+3", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+4", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+5", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+6", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+7", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+8", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason <- gsub("+9", NA, only_failed$Failure_Reason)
        only_failed$Failure_Reason[is.na(only_failed[,1])] <- "Impurity Present"
        
        
        only_failed
        
}

