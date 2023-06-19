test_that("datatable", {
  expect_gt(nrow(get_all_stocks()), 10)
})
