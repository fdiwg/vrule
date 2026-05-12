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
    
    #'@description Initializes a year validation rule
    #'@param na_allowed \code{TRUE} if NA values are allowed, \code{FALSE otherwise}
    #'@param ... any other arg
    initialize = function(na_allowed = FALSE, ...){
      super$initialize(na_allowed = na_allowed, ...)
    },
    
    #'@description Validates a year
    #'@param value value
    #'@param ... any other args
    #'@return a validation report, object of class \link{vrule_report}
    validate = function(value, ...){
      rep = super$validate(value, ...)
      if(nrow(rep$report)==0){
        if(!is.na(value)) if(regexpr("^\\d{4}$", value) < 0){
          rep <- create_vrule_report(
            valid = FALSE,
            category = self$getCategory(),
            rule = self$getName(),
            type = self$getType(),
            message = sprintf("Source value %s is not valid year", value)
          )
        }
      }
      return(rep)
    }
  )
)

#' vrule_date
#' @name vrule_date
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date <- R6Class("vrule_date",
  inherit = vrule_character,
  private = list(
    category = "Time",
    name = "Date"
  ),
  public = list(
    
    #'@description Initializes a date validation rule
    #'@param na_allowed \code{TRUE} if NA values are allowed, \code{FALSE otherwise}
    #'@param ... any other arg
    initialize = function(na_allowed = FALSE, ...){
      super$initialize(na_allowed = na_allowed, ...)
    },
    
    #'@description Validates a date
    #'@param value value
    #'@param ... any other args
    #'@return a validation report, object of class \link{vrule_report}
    validate = function(value, ...){
      rep = super$validate(value, ...)
      if(nrow(rep$report)==0){
        if(!is.na(value)) if(regexpr("^[1-9]\\d{3}-((0[13578]|1[02])-(0[1-9]|[12][0-9]|3[01])|(0[469]|11)-(0[1-9]|[12][0-9]|30)|(02)-(0[1-9]|[12][0-9]))", value) < 0){
          rep <- create_vrule_report(
            valid = FALSE,
            category = self$getCategory(),
            rule = self$getName(),
            type = self$getType(),
            message = sprintf("Source value %s is not valid date", value)
          )
        }
      }
      return(rep)
    }
  )
)

#' vrule_datetime
#' @name vrule_datetime
#' @docType class
#' @importFrom R6 R6Class


#' vrule_timestamp
#' @name vrule_timestamp
#' @docType class
#' @importFrom R6 R6Class