#' vrule_year
#' @name vrule_year
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_year <- R6Class("vrule_year",
  inherit = vrule_integer,
  private = list(
    category = "Time",
    name = "Year"
  ),
  public = list(
    initialize = function(na_allowed = FALSE, ...){
      super$initialize(na_allowed = na_allowed)
    },
    validate = function(value){
      rep = super$validate(value)
      if(nrow(rep$report)==0){
        if(!is.na(value)) if(regexpr("^\\d{4}$", value) < 0){
          rep <- vrule_report$new(
            valid = FALSE,
            report = data.frame(
              category = self$getCategory(),
              rule = self$getName(),
              type = "ERROR",
              message = sprintf("Source value %s is not valid year", value)
            )
          )
        }
      }
      return(rep)
    }
  )
)