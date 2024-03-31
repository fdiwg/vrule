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
    json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/fdi-formats/main/cwp_rh_generic_gta_taskI.json")
  )
  expect_is(format, "format_spec")
})

test_that("decode format spec - benchmarking",{
  #simplified
  data <- readr::read_csv("D:/Documents/CLIENTS/FAO/Projets/WECAFC-FIRMS/formated/WECAFC_task_II_1_DMA_2013_2020_simplified_format,.csv", guess_max = 0)
  format = vrule::format_spec$new(
    json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/fdi-formats/main/cwp_rh_simplified_wecafc_taskII.1.json")
  )
  expect_is(format, "format_spec")
  
  #generic
  data <- readr::read_csv("D:/Downloads/nominal_catch_iotc_level0.csv", guess_max = 0) %>% as.data.frame()
  data$measurement = "catch"
  data$measurement_obs = ""
  data[1,]$fishing_fleet = "United Arab Emirates"
  
  format = vrule::format_spec$new(
    json = jsonlite::read_json("https://raw.githubusercontent.com/fdiwg/fdi-formats/main/cwp_rh_generic_gta_taskI.json")
  )
  format$column_specs[[2]]$rules[[1]]$allow_labels = T
  
  data_sample = data[1:20,]
  report = format$validate(data[1:20,])
  report = report[report$category != "Data structure",]
  hst = format$display_as_handsontable(data_sample, report, read_only = F)
  
  
  #benchmarking
  library(rbenchmark)
  
  rbenchmark::benchmark(
    format$validate(data[1:10000,], method = "rowcol"),
    format$validate(data[1:10000,], method = "matrix"),
    format$validate(data[1:10000,], method = "grid"),
    format$validate(data[1:10000,], method = "grid_mapply"),
    replications = 10
  )
  
})