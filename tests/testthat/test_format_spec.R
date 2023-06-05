# test_format_spec.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for format_spec.R
#=======================
require(vrule, quietly = TRUE)
require(testthat)
require(jsonlite)

context("format_spec")

test_that("decode format spec",{
  
  format = vrule::format_spec$new(
    json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/fdi-formats/main/cwp_rh_generic_gta.json")
  )
  expect_is(format, "format_spec")
})