test_that("tukeyPPCC returns the right values", {
  expect_snapshot_output(tukeyPPCC(RANDWEIB.DAT$Y))
  expect_snapshot_output(tukeyPPCC(read.table("https://www.itl.nist.gov/div898/handbook/datasets/NORMAL2.DAT", skip=25)$V1))
  expect_snapshot_output(tukeyPPCC(read.table("https://www.itl.nist.gov/div898/handbook/datasets/RANDWALK.DAT", skip=25)$V1))
})
