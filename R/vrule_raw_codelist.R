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
    initialize = function(ref_values = NULL){
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
          type = "ERROR",
          message = sprintf("Source value %s is not among allowed values [%s]", 
                            value, paste0(self$ref_values, collapse=","))
        )
      }
      return(rep)
    }
  )
)