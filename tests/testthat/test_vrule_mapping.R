# test_vrule_mapping.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for vrule_cross_column.R
#=======================
require(vrule, quietly = TRUE)
require(testthat)

context("vrule_mapping")

test_that("vrule_mapping",{
  map = vrule_mapping$new(
    ref_data_url = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/cross-term/codelist_mapping_source_authority_species.csv",
    ref_source_term = "species",
    ref_target_term = "source_authority",
    data_target_term = "source_authority"
  )
  expect_false(map$validate("SBF", data.frame(source_authority = "IOTC"))$valid)
  expect_true(map$validate("ALB", data.frame(source_authority = "ICCAT"))$valid)
})