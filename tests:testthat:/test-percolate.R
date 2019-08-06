# test 1
my_board <- board(matrix(1, nrow = 10, ncol = 10))
test_that("percolate works on a board with all open sites.", {
  expect_true(class(percolate(my_board)) == "list",
              all(percolate(my_board)[[1]] == 2),
              percolate(my_board)[[2]] == TRUE)
})

# test 2
my_board <- board(matrix(0, nrow = 10, ncol = 10))
test_that("percolate works on a board with all blocked sites.", {
  expect_true(class(percolate(my_board)) == "list",
              all(percolate(my_board)[[1]] == 0),
              percolate(my_board)[[2]] == FALSE)
})

# test 3
my_board <- board(generate_board_mat(n = 10, p = 0.25))
my_board[1,] = 0
test_that("percolate works on a board where the top row being blocked.", {
  expect_true(class(percolate(my_board)) == "list",
              percolate(my_board)[[2]] == FALSE)
})

# test 4
my_board <- board(generate_board_mat(n = 10, p = 0.25))
my_board[nrow(my_board),] = 0
test_that("percolate works on a board where the bottom row is blocked.", {
  expect_true(class(percolate(my_board)) == "list",
              percolate(my_board)[[2]] == FALSE)
})

## testing 50 boards
test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))
  
  your_result_list <- lapply(board_list, percolate)
  
  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board
    
    identical(your_board, result_board) * 
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })
  
  expect_true(all(bool_vec))
})