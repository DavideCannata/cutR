# rename shapes

#'rename shapes dimensions
#'
#'Rename the column of shapes from data extracts addingright labels
#'@param dataset The dataset to rename
#'@param start_col The index  of the first column of the dataset that is a shapes dimension (e.g. directing for management)
#'@param version For now only available 'basic' and 'management'
#'@return A database, with the right column names for shapes
#'@export

rename_shapes <- function(dataset, start_col, version) {

    if (version == "basic") {

        colnames(dataset)[start_col:(start_col + 15)] <- c("Professional_challenge", "Autonomy",
            "Flexibility", "Recognition", "Self_efficacy", "Perseverance", "Keenness", "Identification",
            "Conscientiousness", "Creativity", "Circumspection", "Fun_at_work", "Harmony", "Sociable_skills",
            "Cooperation", "test_time")
        return(dataset)

    } else if (version == "management") {

        colnames(dataset)[start_col:(start_col + 18)] <- c("Directing", "Conscientious", "Analytical",
            "Conceptual", "Imaginative", "Open_to_change", "Autonomous", "Achieving", "Competitive",
            "Energetic", "Persuasive", "Socially_confident", "Sociable", "Agreeable", "Behavioural",
            "Prudent", "Focused_on_results", "Systematic", "test_time")
        return(dataset)

    } else {
        print("Error: please insert a valid name for the version")
    }
}
