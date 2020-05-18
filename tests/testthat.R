library(testthat)
library(week4)

#test_check("week4")

test_that("File name",{
  filename <- make_filename(2013)
  expect_that(filename,equals("accident_2013.csv.bz2"))
})

#expect_equal(make_filename("2013", "accident_2013.csv.bz2"))
#expect_equal(make_filename("2014", "accident_2014.csv.bz2"))
#expect_equal(make_filename("2015", "accident_2015.csv.bz2"))
