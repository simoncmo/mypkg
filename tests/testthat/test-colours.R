test_that("n is at least 1", {
  expect_error(make_shades("goldenrod", -1),
               "n must be at least 1")
  expect_error(make_shades("goldenrod", 0),
               "n must be at least 1")
})

