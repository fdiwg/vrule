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
    initialize = function(na_allowed = FALSE, ...){
      super$initialize(na_allowed = na_allowed)
    },
    validate = function(value){
      rep = super$validate(value)
      if(nrow(rep$report)==0){
        if(!is.na(value)) if(regexpr("^[1-9]\\d{3}-((0[13578]|1[02])-(0[1-9]|[12][0-9]|3[01])|(0[469]|11)-(0[1-9]|[12][0-9]|30)|(02)-(0[1-9]|[12][0-9]))", value) < 0){
          rep <- vrule_report$new(
            valid = FALSE,
            report = data.frame(
              category = self$getCategory(),
              rule = self$getName(),
              type = "ERROR",
              message = sprintf("Source value %s is not valid date", value)
            )
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