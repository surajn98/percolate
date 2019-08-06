#' make_board function to create an object with class 'board'
#'
#' @param mat matrix either provided or set as NULL
#' @param n rows/columns either provided or set as 5
#' @param p probabilitiy of blocked squares in the matrix either provided or set as 0.25
#'
#' @return an object with class 'board'
#' @export
#'
#' @examples board(mat = generate_board_mat(4,0.4))
board <- function(mat = NULL, n = 5, p = 0.25){
  output <- vector(length = 3, mode = "list")
  
  if(is.null(mat)){
    mat <- generate_board_mat(n, p)
    output <- mat
  }
  else{
    output <- mat
  }
  
  attr(output, "n") <- dim(mat)[1]
  attr(output, "p") <- length(which(mat == 0))/n^2
  class(output) <- c("board", "matrix")
  output
}