#' vrule_op
#' @name vrule_op
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_op <- R6Class("vrule_op",
  inherit = vrule_abstract,
  private = list(
    category = "Operators",
    name = NA
  ),
  public = list(
    operator = NA,
    rules = list(),
    initialize = function(operator, ...){
      self$operator = operator
      rules = list(...)
      if(!all(sapply(rules, is, "vrule_abstract"))){
        stop("At least one rule defined is not an object extending 'vrule_abstract'")
      }
      self$rules = rules
    },
    
    validate = function(value){
      report <- do.call("rbind", lapply(self$rules, function(rule){
        rule$validate(value)
      }))
      return(report)
    }
  )
)

#' vrule_and
#' @name vrule_and
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_and <- R6Class("vrule_and",
   inherit = vrule_op,
   public = list(
     initialize = function(...){
       super$initialize(operator = "&", ...)
     },
     
     validate = function(value){
       report <- super$validate(value)
       #if(nrow(report)>0) if(any(report$type == "ERROR")){
       #  report <- rbind(report, data.frame(
       #    category = self$getCategory(),
       #    rule = "And operator",
       #    type = "ERROR",
       #    message = sprintf("Value %s is not valid because at least one validation rule is not fulfilled", value)
       #  ))
       #}
       return(report)
     }
   )
)