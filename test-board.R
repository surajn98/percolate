# board tests
n = 4; p = 0.25
t_mat <- generate_board_mat(4,0.25)
test_board <- board(t_mat)
test_that("make_board provides a proper `board` structure.", {
  expect_equivalent(unclass(t_mat),unclass(test_board))
  expect_true(n == attr(test_board,"n"),
              p == attr(test_board,"p"))
})