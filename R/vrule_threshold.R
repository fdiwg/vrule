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
    
    validate = function(value){
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
   
   validate = function(value){
     super$validate(value)
   }
 )
)