#' Filled Density Plot
#'
#' Create a simple and beautiful distribution plot
#' @param data the dataset in which is stored the variable to plot
#' @param x an integer, indicating the index of the column you want to visualise the distribution
#' @param col A string, indicating the color of the plot. Default is 'orange'.
#' @return A plot with a density function
#' @export


plot_density <- function(data, x, col = "orange") {
    d <- density(data[[x]], na.rm = TRUE)  ##calculate density
    plot(d, main = colnames(data)[x])  ##plot density
    polygon(d, col = col, border = "black")  ##describe shapes and colours
}
