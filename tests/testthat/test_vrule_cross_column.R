# test_vrule_cross_column.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for vrule_cross_column.R
#=======================
require(vrule, quietly = TRUE)
require(testthat)

context("vrule_cross_column")

test_that("vrule_cross_column - numerics",{
  
  df = data.frame(colA = c(1,2), colB = c(2,3), colC = c(3,6))
  rule = vrule_cross_column$new(operator = ">", expr = "row$colA + row$colB")
  
  df_row1 = df[1,]
  rep1 = rule$validate(value = df_row1$colC, row = df_row1)
  expect_is(rep1, "vrule_report")
  expect_false(rep1$valid)
  expect_is(rep1$report, "data.frame")
  
  df_row2 = df[2,]
  rep2 = rule$validate(value = df_row2$colC, row = df_row2)
  expect_is(rep2, "vrule_report")
  expect_true(rep2$valid)
  expect_is(rep2$report, "data.frame")
})

test_that("vrule_cross_column - dates",{
  
  df = data.frame(
    time_start = as.Date(c("2010-01-01","2010-01-01", "2010-01-01", NA, NA)), 
    time_end = as.Date(c("2009-01-01", "2010-01-31", NA, "2010-01-01", NA))
  )
  rule = vrule_cross_column$new(operator = "<", expr = "row$time_end")
  
  df_row1 = df[1,]
  rep1 = rule$validate(value = df_row1$time_start, row = df_row1)
  expect_is(rep1, "vrule_report")
  expect_false(rep1$valid)
  expect_is(rep1$report, "data.frame")
  
  df_row2 = df[2,]
  rep2 = rule$validate(value = df_row2$time_start, row = df_row2)
  expect_is(rep2, "vrule_report")
  expect_true(rep2$valid)
  expect_is(rep2$report, "data.frame")
  
  df_row3 = df[3,]
  rep3 = rule$validate(value = df_row3$time_start, row = df_row3)
  expect_is(rep3, "vrule_report")
  expect_false(rep3$valid)
  expect_is(rep3$report, "data.frame")
  
  df_row4 = df[4,]
  rep4 = rule$validate(value = df_row4$time_start, row = df_row4)
  expect_is(rep4, "vrule_report")
  expect_false(rep4$valid)
  expect_is(rep4$report, "data.frame")
  
  df_row5 = df[5,]
  rep5 = rule$validate(value = df_row5$time_start, row = df_row5)
  expect_is(rep5, "vrule_report")
  expect_false(rep5$valid)
  expect_is(rep5$report, "data.frame")
  
})