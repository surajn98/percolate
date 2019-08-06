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

elapsed_time <- data.frame(n_squared = n^2, avg_time = avgs)

ggplot(data = elapsed_time, aes(x = avg_time, y = n_squared)) +
  geom_line() +
  labs(x = "Average Elapsed Time",
       y = "n^2",
       title = "Average Elapsed Time vs n^2")

# draw regression
time_lm <- lm(avg_time ~ n_squared, data = elapsed_time)

ggplot(data = elapsed_time, aes(x = avg_time, y = n_squared)) +
  geom_line() +
  geom_abline(slope = coef(time_lm)[2],
              intercept = coef(time_lm)[1], color = "red") +
  labs(x = "Average Elapsed Time",
       y = "n^2",
       title = "Average Elapsed Time vs n^2")

# see how fraction of p affects percolation
p <- seq(0, 1, by = 0.05)
percolate_percent <- vector(length = 3)

n <- 5
count <- 0
for (i in 1:length(p)){
  b <- board(generate_board_mat(n,p[i]))
  if(percolate(b)[[2]] == TRUE) count <- count+1
}
percolate_percent[1] <- count/length(p)

n <- 10
count <- 0
for (i in 1:length(p)){
  b <- board(generate_board_mat(n,p[i]))
  if(percolate(b)[[2]] == TRUE) count <- count+1
}
percolate_percent[2] <- count/length(p)

n <- 25
count <- 0
for (i in 1:length(p)){
  b <- board(generate_board_mat(n,p[i]))
  if(percolate(b)[[2]] == TRUE) count <- count+1
}
percolate_percent[3] <- count/length(p)

percolate_percent

