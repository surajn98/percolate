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

# more read_boards tests
expect_result_list <- vector(length = 1, mode = "list")
expect_result_list[[1]] <- NA

# test 1
test1_output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test1.txt")

test_that("read_boards works on incorrect number of rows", {
  expect_true(identical(test1_output,expect_result_list))
})

# test 2
test2_output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test2.txt")

test_that("read_boards works on incorrect character in rows", {
  expect_true(identical(test2_output,expect_result_list))
})

# test 3
test3_output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test3.txt")

test_that("read_boards works on no n specified", {
  expect_true(identical(test3_output,expect_result_list))
})

# test 4
test4_output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test4.txt")

test_that("read_boards works on incorrect characters in rows", {
  expect_true(identical(test4_output,expect_result_list))
})

# test 5
test5_output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test5.txt")

test_that("read_boards works on incorrect number of columns", {
  expect_true(identical(test5_output,expect_result_list))
})

# test 6
test6_output <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test6.txt")

test_that("read_boards works on negative n provided", {
  expect_true(identical(test6_output,expect_result_list))
})
