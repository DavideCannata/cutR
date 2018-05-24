#' Reshape multidynamic query
#'
#'transforms any result of a multidynamic query into a tidy dataset.
#' @param dataset: is the datset we want to trasform
#' @param instrument: the default, NULL, uses all the instruments in the file.
#' Otherwise is possible to add one instrument ID or an array of instrument IDs
#' (e.g. to have shapes management and views 'instrument = c ( 102 , 201)' )
#'@param extratime: if TRUE removes extratime candidates; default is FALSE
#'@param partner/project: The default, null, returns all partner or project accounts, but is possible to use this for filtering
#'@param bio: the default (TRUE), adds biodata. Set it as FALSE if yu don't want biodata
#'@return Returns a tidy daframe with one person per row
#'@examples
#' reshapemd (export_from_germany,instrument = c(102,301), extratime = TRUE, project = 80254, bio = FALSE)
#' @export
#' @import reshape
#' @import dplyr
#' @import tidyr

reshapemd <- function(dataset, instrument = NULL, extratime = FALSE, partner = NULL, project = NULL,
    bio = TRUE) {

    if (extratime == TRUE) {
        filter(!is.null(set_extratime))  # Removes any candidate with extended time
    }
    if (!is.null(instrument)) {
        dataset <- subset(dataset, Inst_ID == instrument)  # Removes all data not related to the selected instrument
    }

    if (!is.null(partner)) {
        filter(Partner_ID == partner)
    }

    if (!is.null(project)) {
        filter(Project_ID == project)
    }

    ## Create age value

    if (bio == TRUE) {
        dataset$testyear <- as.integer(substr(dataset$Task_End, 1, 4))
        dataset$age <- (dataset$testyear - dataset$Part_BirthYear)
        dataset$age <- replace(dataset$age, dataset$age > 100, NA)
    }

    ## Create test time
    dataset$testtime <- dataset$Task_End - dataset$Task_Start

    # Remove unnecessary columns
    dataset$Partner_ID <- NULL
    dataset$Partner_Name <- NULL
    dataset$client_id <- NULL
    dataset$Project_ID <- NULL
    dataset$Project_Name <- NULL
    dataset$Project_Function <- NULL
    dataset$Project_TargetGroup <- NULL
    dataset$Project_WorkExp <- NULL
    dataset$Part_ExternalID <- NULL
    dataset$Task_ID <- NULL
    dataset$Task_Start <- NULL
    dataset$Inst_ID <- NULL
    dataset$score_string <- NULL
    dataset$set_extratime <- NULL
    dataset$testyear <- NULL
    dataset$Inst_ShortName <- NULL
    dataset$Task_End <- NULL
    dataset$Part_BirthYear <- NULL



    if (bio == TRUE) {
        scores <- cast(dataset, Part_ID ~ score_name, value = "score_decimal", fun.aggregate = max)
        bio <- dataset[!duplicated(dataset$Part_ID), ]  # Reshapes biodata
        bio$score_name <- NULL  # Removes score name from biodata dataset
        bio$score_decimal <- NULL  # Removes score decimal from biodata dataset
        dataset <- merge(bio, scores)  # Combines scores and biodata
    } else {
        dataset <- cast(dataset, Part_ID ~ score_name, value = "score_decimal", fun.aggregate = max)
    }


    # Reshape dataset to show one row per participant
    # clean "-inf"

    return(dataset)  # Reshapes scores
}
