# generate_board_mat test 1
test_that("generate_board_mat creates a 5 by 5 matrix with only 0's and 1's.", {
  expect_true(nrow(generate_board_mat()) == 5,
              ncol(generate_board_mat()) == 5,
              (generate_board_mat() == 0 || generate_board_mat() == 1))
})

# generate_board_mat test 2
test_that("generate_board_mat creates a 6 by 6 matrix with only 0's and 1's.", {
  expect_true(nrow(generate_board_mat(n = 6)) == 6,
              ncol(generate_board_mat(n = 6)) == 6,
              (generate_board_mat() == 0 || generate_board_mat() == 1))
})

# generate_board_mat test 3
test_that("generate_board_mat with p = 0 gives us a board with all 1's", {
  expect_true(all(generate_board_mat(p = 0) == 1))
})

# generate_board_mat test 4
test_that("generate_board_mat with p = 1 gives us a board with all 0's", {
  expect_true(all(generate_board_mat(p = 1) == 0))
})

# generate_board_mat test 5
test_that("n can only be a positive integer", {
  expect_error(generate_board_mat(n = c(1,2)))
  expect_error(generate_board_mat(n = "asf"))
  expect_error(generate_board_mat(n = 5.4))
  expect_error(generate_board_mat(n = -5))
})


# is_valid tests
test_mat <- matrix(1, 4, 3)
test_that("is_valid fails on a board that is not square.", {
  expect_error(is_valid(test_mat))
})

test_mat_2 <- matrix(4, 4, 4)
test_that("is_valid fails on a board that contains values that are not 0, 1, or 2.", {
  expect_error(is_valid(test_mat_2))
})

test_mat_3 <- 2
test_that("is_valid fails on an input that is not a matrix.", {
  expect_error(is_valid(test_mat_3))
})


# read_boards tests
output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")

file <- url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata")
load(file = file)

for (i in 1:length(output)){
  test_that("output is the same as board_list", {
    expect_true(identical(attr(output[[i]],"class"),
                          attr(board_list[[i]],"class")),
                identical(attr(output[[i]],"n"),
                          attr(board_list[[i]],"n")),
                identical(attr(output[[i]],"p"),
                          attr(board_list[[i]],"p")))
  })
}


