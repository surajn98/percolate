library(ggplot2)
library(tidyr)

#' Plot board function
#'
#' @param x board 
#'
#' @return nothing, prints out plot of the board colored by value
#' @export
#'
#' @examples 
plot <- function(x){
  assert_that(is_valid(x))
  
  n <- attr(x, "n")
  
  board_data <- tidyr::gather(data.frame(row = 1:nrow(x), x), 
                              key = "column", value = "value", -row)
  board_data$column <- as.numeric(substr(board_data$column, 2, 
                                         nchar(board_data$column))) 
  
  colors <- c("black","white","lightblue3")
  
  ggplot(board_data, aes(x = column, y = max(row)-row, fill = factor(value))) +
    scale_fill_manual(values = colors) +
    geom_tile() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste0(n," x ",n," board: ", n*n, " total squares"))
}