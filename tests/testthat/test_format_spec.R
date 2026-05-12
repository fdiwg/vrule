# test_format_spec.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for format_spec.R
#=======================
require(vrule, quietly = TRUE)
require(testthat)
require(jsonlite)

context("format_spec")

test_that("decode format spec 1",{
  format = vrule::format_spec$new(
    json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/vrule/main/inst/extdata/tests/specs/format_spec1.json")
  )
  expect_is(format, "format_spec")
})

test_that("validate 1 row with format spec1",{
  format = vrule::format_spec$new(
    json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/vrule/main/inst/extdata/tests/specs/format_spec1.json")
  )
  dat = data.frame(
    source_authority = "IOTC",
    fishing_fleet = "COM",
    time_start = "2026-01-01",
    time_end = "2026-01-31",
    geographic_identifier = "IOTC_WEST",
    gear_type = "09.1",
    species = "ALB",
    fishing_mode = "FS",
    measurement = "catch",
    measurement_type = "NC",
    measurement_value = 1,
    measurement_unit = "t",
    measurement_processing_level = "original_eesample"
  )
  format$validate(dat)
})

test_that("decode format spec - benchmarking",{
  # format = vrule::format_spec$new(
  #   json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/fdi-formats/main/cwp_rh_generic_gta_taskII.json")
  # )
  # data = readr::read_csv("D:/Downloads/gta/IOTC/iotc_catch_all_1m_firms_level0_2025-04-22_FIXED_processing_level.csv", guess_max = 0)
  # #using 'pair' mode
  # system.time(test1 <- format$validate(data = data, mode = "pair", parallel = T, mc.cores = 12))
  # # user     system   ellapsed 
  # # 132.33   123.77   1305.78
  # 
  # #using 'column' mode
  # system.time(test2 <- format$validate(data = data, mode = "column", parallel = T, mc.cores = 12))
  # # user     system   ellapsed 
  # # 142.89   148.83   637.47
  # 
  # waldo::compare(test1, test2)
})
