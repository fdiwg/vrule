#' vrule_operator_relational
#' @title vrule_operator_relational
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_operator_relational <- R6Class("vrule_operator_relational",
  inherit = vrule_operator_binary,
  private = list(
    operators = c(">",">=","<","<="),
    category = "Relational operators",
    name = "Relational operator"
  ),
  public = list(
    
    #'@description Initializes a operator relational validation rule
    #'@param operator operator
    #'@param expr expr
    #'@param ... any other arg
    initialize = function(operator, expr, ...){
      if(!operator %in% private$operators){
        stop(sprintf("Invalid relational operator '%s'. Possible values are among values [%s]",
                     operator, paste0(private$operators, collapse=",")))
      }
      super$initialize(operator = operator, expr = expr, ...)
    },
    
    #'@description Abstract method to validate data
    #'@param value value
    #'@param ... any other args
    #'@return a validation report, object of class \link{vrule_report}
    validate = function(value, ...){
      nvr = vrule_numeric$new(type = self$getType())
      rep = nvr$validate(value)
      if(nrow(rep$report)==0){
        rep = super$validate(value)
      }
      return(rep)
    }
  )
)

#' vrule_threshold
#' @title vrule_threshold
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_threshold <- R6Class("vrule_threshold",
 inherit = vrule_operator_relational,
 public = list(
   
   #'@description Initializes a threshold validation rule
   #'@param operator operator
   #'@param threshold threshold
   #'@param ... any other arg
   initialize = function(operator, threshold, ...){
     super$initialize(operator = operator, expr = threshold, ...)
   },
   
   #'@description  Validates data based on a threshold
   #'@param value value
   #'@param ... any other args
   #'@return a validation report, object of class \link{vrule_report}
   validate = function(value, ...){
     super$validate(value, ...)
   }
 )
)

#' vrule_date_threshold
#' @title vrule_date_threshold
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_threshold <- R6Class("vrule_date_threshold",
   inherit = vrule_operator_relational,
   public = list(
     
     #'@description Initializes a date threshold validation rule
     #'@param operator operator
     #'@param threshold threshold
     #'@param ... any other arg
     initialize = function(operator, threshold, ...){
       super$initialize(operator = operator, expr = as.Date(threshold), ...)
     },
     
     #'@description  Validates data based on a date threshold
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       super$validate(as.Date(value), ...)
     }
   )
)

#' vrule_min
#' @title vrule_min
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_min <- R6Class("vrule_min",
   inherit = vrule_operator_relational,
   public = list(
     
     #'@description Initializes a min threshold validation rule
     #'@param minValue min value
     #'@param ... any other arg
     initialize = function(minValue, ...){
       super$initialize(operator = ">=", expr = minValue, ...)
     },
     
     #'@description  Validates data based on a min threshold
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       super$validate(value, ...)
     }
   )
)

#' vrule_date_min
#' @title vrule_date_min
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_min <- R6Class("vrule_date_min",
   inherit = vrule_operator_relational,
   public = list(
     
     #'@description Initializes a date min validation rule
     #'@param minValue min value
     #'@param ... any other arg
     initialize = function(minValue, ...){
       super$initialize(operator = ">=", expr = as.Date(minValue), ...)
     },
     
     #'@description  Validates data based on a min date threshold
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       super$validate(as.Date(value), ...)
     }
   )
)

#' vrule_max
#' @title vrule_max
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_max <- R6Class("vrule_max",
   inherit = vrule_operator_relational,
   public = list(
     
     #'@description Initializes a max threshold validation rule
     #'@param maxValue max value
     #'@param ... any other arg
     initialize = function(maxValue, ...){
       super$initialize(operator = "<=", expr = maxValue, ...)
     },
     
     #'@description Validates data based on a max threshold
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       super$validate(value, ...)
     }
   )
)

#' vrule_date_max
#' @title vrule_date_max
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_max <- R6Class("vrule_date_max",
   inherit = vrule_operator_relational,
   public = list(
     
     #'@description Initializes a date max validation rule
     #'@param maxValue max value
     #'@param ... any other arg
     initialize = function(maxValue, ...){
       super$initialize(operator = "<=", expr = as.Date(maxValue), ...)
     },
     
     #'@description  Validates data based on a max date threshold
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       super$validate(as.Date(value), ...)
     }
   )
)

#' vrule_range
#' @title vrule_range
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_range <- R6Class("vrule_range",
   inherit = vrule_abstract_simple,
   public = list(
     
     #'@field minValue min value
     minValue = NA,
     #'@field maxValue max value
     maxValue = NA,
     
     #'@description Initializes a range validation rule
     #'@param minValue min value
     #'@param maxValue max value
     #'@param ... any other arg
     initialize = function(minValue, maxValue, ...){
       super$initialize(...)
       self$minValue = minValue
       self$maxValue = maxValue
     },
     
     #'@description  Validates data based on a range
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       range_rule = vrule_operator_and$new(
         vrule_min$new(minValue = self$minValue, type = self$getType()),
         vrule_max$new(maxValue = self$maxValue, type = self$getType()),
         type = self$getType()
       )
       report = range_rule$validate(value, ...)
       return(report)
     }
   )
)



#' vrule_date_range
#' @title vrule_date_range
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_range <- R6Class("vrule_date_range",
   inherit = vrule_abstract_simple,
   public = list(
     
     #'@field minValue min value
     minValue = NA,
     #'@field maxValue max value
     maxValue = NA,
     
     #'@description Initializes a range validation rule
     #'@param minValue min value
     #'@param maxValue max value
     #'@param ... any other arg
     initialize = function(minValue, maxValue, ...){
        super$initialize(...)
        self$minValue = as.Date(minValue)
        self$maxValue = as.Date(maxValue)
     },
     
     #'@description  Validates data based on a date range
     #'@param value value
     #'@param ... any other args
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(value, ...){
       range_rule = vrule_operator_and$new(
         vrule_min$new(minValue = self$minValue, type = self$getType()),
         vrule_max$new(maxValue = self$maxValue, type = self$getType()),
         type = self$getType()
       )
       report = range_rule$validate(as.Date(value), ...)
       return(report)
     }
   )
)
