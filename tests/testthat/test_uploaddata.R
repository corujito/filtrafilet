context("Upload data")

test_that("uploading 1 file from scopus", {
  mydata = uploaddata("../../scopus_2.csv", NULL)
  expect_equal(nrow(mydata), 315)
  #expect_equal(mydata$Journal.Impact.Factor[which(mydata$Source.Title =="EUROPEAN JOURNAL OF OPERATIONAL RESEARCH")], 2,358)
})

test_that("uploading 1 file from web science", {
  mydata = uploaddata("../../webscience.txt", NULL)
  expect_equal(nrow(mydata), 10)
  expect_equal(mydata$Journal.Impact.Factor[which(mydata$Source.Title == "COMPUTERS & OPERATIONS RESEARCH")], 1,637)
  expect_equal(mydata$Journal.Impact.Factor[which(mydata$Source.Title == "STRUCTURAL OPTIMIZATION")], -1)
  #expect_match(string, "Testing")
})

test_that("uploading 1 file from scopus and 1 file from web science", {
  mydata = uploaddata("../../scopus.csv", NULL)
  mydata = uploaddata("../../webscience.txt", mydata)
  expect_equal(nrow(mydata), 20)
  expect_equal(mydata$Journal.Impact.Factor[which(mydata$Source.Title == "COMPUTERS & OPERATIONS RESEARCH")], 1,637)
  expect_equal(mydata$Journal.Impact.Factor[which(mydata$Source.Title == "STRUCTURAL OPTIMIZATION")], -1)
})
