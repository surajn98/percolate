mat_example_list <- list(matrix(c(1,1,1,1,0, 
                                  0,0,0,1,0, 
                                  1,1,1,1,0, 
                                  0,1,0,0,0, 
                                  0,1,1,1,1), 5, 5), 
                         matrix(c(1,1,1,1,0,
                                  0,0,0,1,0, 
                                  0,1,1,1,0, 
                                  0,1,0,0,0, 
                                  0,1,1,1,1), 5, 5),
                         matrix(c(1,1,1,1,0,
                                  0,0,0,1,0, 
                                  0,1,1,0,0, 
                                  0,1,0,0,0, 
                                  0,1,1,1,1), 5, 5))

#' Function to percolate board
#'
#' @param x board
#'
#' @return list, first element is percolated board, second element is boolean identifying if board percolated fully or not
#' @export
#'
#' @examples lapply(mat_example_list,FUN = percolate)
percolate <- function(x){
  start_board <- x
  
  assert_that(is_valid(start_board))
  assert_that(start_board == 0 || start_board == 1)
  
  for(i in 1:ncol(start_board)){
    if(start_board[1,i] == 1) start_board[1,i] = 2
  }
  
  result_board <- start_board
  changed <- TRUE
  while(changed != FALSE){
    for (i in 2:nrow(result_board)){
      for (j in 1:ncol(result_board)){
        if(result_board[i,j] == 1){
          
          #check above
          if(result_board[i-1,j] == 2) result_board[i,j] = 2
          
          #check below
          if(i != nrow(result_board)){
            if(result_board[i+1,j] == 2) result_board[i,j] = 2
          }
          
          #check right
          if(j != ncol(result_board)){
            if(result_board[i,j+1] == 2) result_board[i,j] = 2
          }
          
          #check left
          if(j != 1){
            if(result_board[i,j-1] == 2) result_board[i,j] = 2
          }
        }
      }
    }
    
    if(all(result_board == start_board)) changed <- FALSE
    else start_board <- result_board
  }
  
  
  result <- FALSE
  for (i in 1:ncol(result_board)){
    if(result_board[nrow(result_board),i] == 2) result <- TRUE
  }
  
  return(list(result_board = result_board,
              result = result))
}

# finding elapsed time
p <- 0.4
n <- seq(10, 100, by = 10)
avgs <- vector(length = 10)

for (i in 1:10){
  sum_time <- 0
  for (j in 1:length(n)){
    board_obj <- board(generate_board_mat(n[j],p))
    elapsed <- system.time(percolate(board_obj))[3]
    sum_time <- sum_time + elapsed
  }
  avgs[i] <- sum_time/10
}