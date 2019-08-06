# test 1
test_that("generate_board_mat creates a 5 by 5 matrix with only 0's and 1's.", {
  expect_true(nrow(generate_board_mat()) == 5,
              ncol(generate_board_mat()) == 5,
              (generate_board_mat() == 0 || generate_board_mat() == 1))
})

# test 2
test_that("generate_board_mat creates a 6 by 6 matrix with only 0's and 1's.", {
  expect_true(nrow(generate_board_mat(n = 6)) == 6,
              ncol(generate_board_mat(n = 6)) == 6,
              (generate_board_mat() == 0 || generate_board_mat() == 1))
})

# test 3
test_that("generate_board_mat with p = 0 gives us a board with all 1's", {
  expect_true(all(generate_board_mat(p = 0) == 1))
})

# test 4
test_that("generate_board_mat with p = 1 gives us a board with all 0's", {
  expect_true(all(generate_board_mat(p = 1) == 0))
})

# test 5
test_that("n can only be a positive integer", {
  expect_error(generate_board_mat(n = c(1,2)))
  expect_error(generate_board_mat(n = "asf"))
  expect_error(generate_board_mat(n = 5.4))
  expect_error(generate_board_mat(n = -5))
})