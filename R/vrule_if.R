#' vrule_if
#' @name vrule_if
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_if <- R6Class("vrule_if",
  inherit = vrule_abstract_complex,
  private = list(
    category = NA,
    name = NA
  ),
  public = list(
    
    #'@field if_condition if expression
    if_condition = NA,
    #'@field then_apply list of expressions to apply if condition is fulfilled
    then_apply = list(),
    #'@field else_apply list of expressions to apply if condition is not fulfilled
    else_apply = list(),
    
    #'@description Initializes a conditionnal validation rule
    #'@param if_condition if condition
    #'@param then_apply list of expressions to apply if condition is fulfilled
    #'@param else_apply list of expressions to apply if condition is not fulfilled
    #'@param ... any other arg
    initialize = function(if_condition, then_apply = list(), else_apply = list(), ...){
      super$initialize(...)
      self$if_condition = if_condition
      self$then_apply = then_apply
      self$else_apply = else_apply
    },
    
    #'@description Abstract method to validate data
    #'@param value value
    #'@param row row
    #'@return a validation report, object of class \link{vrule_report}
    validate = function(value, row){
      rep = super$validate()
      expr = eval(parse(text = self$if_condition))
      if(is.logical(expr)){
        if(isTRUE(expr)){
          logical_rule = if(length(self$then_apply)>1){
            do.call(vrule_operator_and$new, c(self$then_apply, type = self$getType()))
          }else{
            self$then_apply[[1]]
          }
          rep = logical_rule$validate(value, row)
        }else{
          logical_rule = if(length(self$else_apply)>1){
            do.call(vrule_operator_and$new, c(self$else_apply, type = self$getType()))
          }else if(length(self$else_apply)==1){
            self$else_apply[[1]]
          }else{
            NULL
          }
          if(!is.null(logical_rule)) rep = logical_rule$validate(value, row)
        }
      }
      return(rep)
    }
  )
)