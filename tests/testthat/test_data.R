context("PackageTests")

# test_that("Internal Raw Data Accessible", {
#   expect_match(devtools::package_file("inst","extdata","accident_2013.csv.bz2"),
#                make_filename(2013))
#   expect_match(devtools::package_file("inst","extdata","accident_2014.csv.bz2"),
#                make_filename(2014))
#   expect_match(devtools::package_file("inst","extdata","accident_2015.csv.bz2"),
#                make_filename(2015))
# })

test_that("fars_read is working", {
  expect_is(fars_read(make_filename(2013)), "data.frame")
  expect_is(fars_read(make_filename(2014)), "data.frame")
  expect_is(fars_read(make_filename(2015)), "data.frame")
})

test_that("fars_read_years is a data frame", {
  expect_is(fars_read_years(c(2013,2014,2015)), "list")
})

test_that("fars_summarize_years is a data frame", {
  expect_is(fars_summarize_years(c(2013,2014,2015)), "data.frame")
})
