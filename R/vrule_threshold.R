#' vrule_operator_relational
#' @name vrule_operator_relational
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
    initialize = function(operator, expr, ...){
      if(!operator %in% private$operators){
        stop(sprintf("Invalid relational operator '%s'. Possible values are among values [%s]",
                     operator, paste0(private$operators, collapse=",")))
      }
      super$initialize(operator = operator, expr = expr)
    },
    
    validate = function(value, ...){
      nvr = vrule_numeric$new()
      rep = nvr$validate(value)
      if(nrow(rep$report)==0){
        rep = super$validate(value)
      }
      return(rep)
    }
  )
)

#' vrule_threshold
#' @name vrule_threshold
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_threshold <- R6Class("vrule_threshold",
 inherit = vrule_operator_relational,
 public = list(
   initialize = function(operator, threshold){
     super$initialize(operator = operator, expr = threshold)
   },
   
   validate = function(value, ...){
     super$validate(value, ...)
   }
 )
)

#' vrule_date_threshold
#' @name vrule_date_threshold
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_threshold <- R6Class("vrule_date_threshold",
                           inherit = vrule_operator_relational,
                           public = list(
                             initialize = function(operator, threshold){
                               super$initialize(operator = operator, expr = as.Date(threshold))
                             },
                             
                             validate = function(value, ...){
                               super$validate(as.Date(value), ...)
                             }
                           )
)

#' vrule_min
#' @name vrule_min
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_min <- R6Class("vrule_min",
   inherit = vrule_operator_relational,
   public = list(
     initialize = function(minValue){
       super$initialize(operator = ">=", expr = minValue)
     },
     
     validate = function(value, ...){
       super$validate(value, ...)
     }
   )
)

#' vrule_date_min
#' @name vrule_date_min
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_min <- R6Class("vrule_date_min",
                     inherit = vrule_operator_relational,
                     public = list(
                       initialize = function(minValue){
                         super$initialize(operator = ">=", expr = as.Date(minValue))
                       },
                       
                       validate = function(value, ...){
                         super$validate(as.Date(value), ...)
                       }
                     )
)

#' vrule_max
#' @name vrule_max
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_max <- R6Class("vrule_max",
   inherit = vrule_operator_relational,
   public = list(
     initialize = function(maxValue){
       super$initialize(operator = "<=", expr = maxValue)
     },
     
     validate = function(value, ...){
       super$validate(value, ...)
     }
   )
)

#' vrule_date_max
#' @name vrule_date_max
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_max <- R6Class("vrule_date_max",
                     inherit = vrule_operator_relational,
                     public = list(
                       initialize = function(maxValue){
                         super$initialize(operator = "<=", expr = as.Date(maxValue))
                       },
                       
                       validate = function(value, ...){
                         super$validate(as.Date(value), ...)
                       }
                     )
)

#' vrule_range
#' @name vrule_range
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_range <- R6Class("vrule_range",
                       inherit = vrule_abstract_simple,
                       public = list(
                         minValue = NA,
                         maxValue = NA,
                         initialize = function(minValue, maxValue){
                           self$minValue = minValue
                           self$maxValue = maxValue
                         },
                         
                         validate = function(value, ...){
                           range_rule = vrule_operator_and$new(
                             vrule_min$new(minValue = self$minValue),
                             vrule_max$new(maxValue = self$maxValue)
                           )
                           report = range_rule$validate(value, ...)
                           return(report)
                         }
                       )
)



#' vrule_date_range
#' @name vrule_date_range
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_date_range <- R6Class("vrule_date_range",
   inherit = vrule_abstract_simple,
   public = list(
     minValue = NA,
     maxValue = NA,
     initialize = function(minValue, maxValue){
        self$minValue = as.Date(minValue)
        self$maxValue = as.Date(maxValue)
     },
     
     validate = function(value, ...){
       range_rule = vrule_operator_and$new(
         vrule_min$new(minValue = self$minValue),
         vrule_max$new(maxValue = self$maxValue)
       )
       report = range_rule$validate(as.Date(value), ...)
       return(report)
     }
   )
)
