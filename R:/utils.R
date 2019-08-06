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

#' Board Update Function
#'
#' @param mat matrix provided
#' @param row specific row we are updating
#' @param str_vec string vector we are analyzing
#'
#' @return a board of the now update matrix or NA if condition is satisfied
#' @export
#'
#' @examples
update_board <- function(mat, row, str_vec){
  if(str_vec != "." && str_vec != "*") return(NA)
  for (i in 1:length(str_vec)){
    if(str_vec[i] == "*") mat[row,i] = 0
    else if(str_vec[i] != ".") return(NA)
  }
  return(board(mat))
}

#' Board Reading Function 
#'
#' @param file text file provided with board
#'
#' @return board created with 0's and 1's
#' @export
#'
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")
read_boards <- function(file){
  text <- readLines(file)
  
  test_that("file starts and ends with dashes", {
    expect_true(text[1] == "----",
                text[length(text)] == "----")
  })
  
  result_list <- vector(length = length(which(text == "----")) - 1, 
                        mode = "list")
  
  i <- 1
  num_board <- 1
  while (num_board <= length(result_list)){
    i <- i + 1
    if(text[i-1] == "----"){
      n <- as.double(text[i])
      if (!is.na(n) && n <= 0) n <- NA
      i <- i + 1
    }
    
    if (is.na(n)) mat <- NA
    else{
      mat <- board(n = n, p = 0)
      elems <- strsplit(text[i], split = " ")[[1]]
      
      if(length(elems) != n) mat <- NA
      else if(i + n > length(text) ||
              text[i + n] != "----" ) mat <- NA
      else{
        start_mat_text <- i
        for (j in 1:n){
          mat <- update_board(mat, j, elems)
          if(length(elems) != n) mat <- NA
          if(is.na(mat)){
            i <- start_mat_text + n
            break
          }
          i <- i + 1
          elems <- strsplit(text[i], split = " ")[[1]]
        }
        
        
        if(i != length(text)){
          test_that("matrices are separated by dashes", {
            expect_true(text[i] == "----",
                        text[i + 1] != "----")
          })
        }
      }
    }
    
    result_list[[num_board]] <- mat
    num_board <- num_board + 1
  }
  
  return(result_list)
}
