context("Upload data")

test_that("uploading 1 file from scopus", {
  mydata = uploaddata("../../savedrecs_2.csv", NULL)
  #expect_equal(mydata.cout, 10)
  #expect_match(string, "Testing")
  #expect_that(2 + 2 == 4, is_true())
  #expect_that(2 == 1, is_false())
})

test_that("uploading 1 file from web science", {
  #mydata = uploaddata("../../webscience.txt", NULL)
})

test_that("uploading 1 file from scopus and 1 file from web science", {
  #mydata = uploaddata("../../savedrecs_2.csv", NULL)
  #mydata = uploaddata("../../webscience.txt", mydata)
})
