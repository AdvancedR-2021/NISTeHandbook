test_that("PPCC returns the right values", {
  expect_snapshot_output(PPCC(RANDWEIB.DAT$Y, 'gamma'))
  expect_snapshot_output(PPCC(RANDWEIB.DAT$Y, 'weibull'))
})
