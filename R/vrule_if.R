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
    if_condition = NA,
    then_apply = list(),
    else_apply = list(),
    initialize = function(if_condition, then_apply = list(), else_apply = list()){
      self$if_condition = if_condition
      self$then_apply = then_apply
      self$else_apply = else_apply
    },
    
    #validate
    validate = function(value, row){
      rep = super$validate()
      expr = eval(parse(text = self$if_condition))
      if(is.logical(expr)){
        if(isTRUE(expr)){
          logical_rule = if(length(self$then_apply)>1){
            do.call(vrule_operator_and$new, self$then_apply)
          }else{
            self$then_apply[[1]]
          }
          rep = logical_rule$validate(value, row)
        }else{
          logical_rule = if(length(self$else_apply)>1){
            do.call(vrule_operator_and$new, self$else_apply)
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