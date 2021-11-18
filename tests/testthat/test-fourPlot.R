test_that("fourPlot outputs the right plot", {
  expect_snapshot_output(fourPlot(LEW.DAT$Deflection))
  expect_snapshot_output(fourPlot(LEW.DAT$Deflection))
})
