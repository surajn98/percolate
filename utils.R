#' generate_board_mat to generate a board
#'
#' @param n number of rows/cols in the board, is a square matrix
#' @param p the percentage of squares in the board that are blocked
#'
#' @return the generate board
#' @export
#'
#' @examples print(generate_board_mat()), print(generate_board_mat(n = 8, p = 0.75))
generate_board_mat <- function(n = 5, p = 0.25){
  assert_that(length(n) == 1 && n > 0 && n %% 1 == 0)
  assert_that(0 <= p && p <= 1)
  
  board <- matrix(1, nrow = n, ncol = n)
  
  num_blocked <- floor(p*n^2)
  
  indices <- sample(1:n^2,num_blocked,replace = FALSE)
  
  for (i in 1:length(indices)){
    board[indices[i]] <- 0
  }
  
  return(board)
}


#' is_valid helper function to check validity of matrix
#'
#' @param mat a matrix
#'
#' @return either TRUE if mat is a valid matrix or an error
#' @export
#'
#' @examples print(is_valid(generate_board_mat())), print(is_valid(generate_board_mat(n = 1)))
is_valid <- function(mat){
  assert_that(nrow(mat) == ncol(mat) && (mat == 0 || mat == 1 || mat == 2))
  return(TRUE)
}


