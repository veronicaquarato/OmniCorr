test_that("CheckSampleOrder correctly aligns sample order", {
  
  df1 <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(c("S1", "S2", "S3"), c("A", "B"))
  )
  
  df2 <- matrix(
    7:12,
    nrow = 3,
    dimnames = list(c("S3", "S1", "S2"), c("C", "D"))
  )
  
  result <- CheckSampleOrder(df1, df2)
  
  expect_identical(
    rownames(result$df2),
    rownames(result$df1)
  )
  
  expect_equal(
    result$df2["S1", ],
    df2["S1", ]
  )
})


test_that("CheckSampleOrder stops when sample sets do not match", {
  
  df1 <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(c("S1", "S2"), c("A", "B"))
  )
  
  df2 <- matrix(
    5:8,
    nrow = 2,
    dimnames = list(c("S1", "S3"), c("C", "D"))
  )
  
  expect_error(
    CheckSampleOrder(df1, df2),
    "Sample sets do not match"
  )
})


test_that("CheckSampleOrder stops when sample names are duplicated", {
  
  df1 <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(c("S1", "S1"), c("A", "B"))
  )
  
  df2 <- matrix(
    5:8,
    nrow = 2,
    dimnames = list(c("S1", "S2"), c("C", "D"))
  )
  
  expect_error(
    CheckSampleOrder(df1, df2),
    "Duplicate sample names detected"
  )
})