#' vrule_raw_codelist
#' @name vrule_raw_codelist
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_raw_codelist <- R6Class("vrule_raw_codelist",
  inherit = vrule_abstract_simple,
  private = list(
    category = "Controlled terms",
    name = "Raw Codelist"
  ),
  public = list(
    ref_values = NULL,
    initialize = function(ref_values = NULL, ...){
      super$initialize(...)
      #case of raw values
      self$ref_values = ref_values
    },
    
    validate = function(value, ...){
      rep <- super$validate(value, ...)
      if(length(which(value == self$ref_values))==0){
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = self$getType(),
          message = sprintf("Source value %s is not among allowed values [%s]", 
                            value, paste0(self$ref_values, collapse=","))
        )
      }
      return(rep)
    }
  )
)


#' vrule_codelist_R
#' @name vrule_codelist_R
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_codelist_R <- R6::R6Class(
  "vrule_Robject",
  inherit = vrule_raw_codelist,
  public = list(
    ref_package = NULL,
    ref_data = NULL,
    ref_data_column = NULL,

    initialize = function(ref_package, ref_data, ref_data_column, ...) {
      codelist <- getExportedValue(ref_package, ref_data)

      if (!ref_data_column %in% names(codelist)) {
        stop(
          "Column ", ref_data_column, " not found in ",
          ref_package, "::", ref_data, ".",
          call. = FALSE
        )
      }

      self$ref_package <- ref_package
      self$ref_data <- ref_data
      self$ref_data_column <- ref_data_column

      super$initialize(
        ref_values = unique(codelist[[ref_data_column]]),
        ...
      )
    }
  )
)