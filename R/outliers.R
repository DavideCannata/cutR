## Function to generate plots for seeing outliers

#'Plots for outliers
#'
#'Creates a plot with all the observations of a variable in order of rank, to identify outliers
#'@param dataset The  dataframe (or tibble) in which is stored the variable
#'@param i The number or the name of the variable
#'@return a plot that is just perfecttp visualise outliers
#'@export

plot_rank <- function(dataset, i) {
    plot(dataset[[i]], rank(dataset[[i]]), xlab = ifelse(is.numeric(i), colnames(dataset)[i],
        i), ylab = "Rank")
}
