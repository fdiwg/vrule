#' vrule_operator_binary
#' @name vrule_operator_binary
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_operator_binary <- R6Class("vrule_operator_binary",
  inherit = vrule_abstract,
  private = list(
    category = "Binary operators",
    name = "Binary operator"
  ),
  public = list(
    operator = NA,
    expr = NA,
    initialize = function(operator, expr, ...){
      super$initialize(...)
      self$operator = operator
      self$expr = expr
    },
    
    validate = function(value, ...){
      rep = super$validate(value, ...)
      expr_value = self$expr
      op_expr = sprintf("value %s expr_value", self$operator)
      cond = eval(parse(text = op_expr))
      if(is.na(cond)){
        rep_type = self$getType()
        rep_msg = ""
        if(is.na(value) & is.na(expr_value)){
          rep_msg = sprintf("Source and target values are NA. Cannot compare each other with operator %s", self$operator) 
        }else{
          if(is.na(value)){
            rep_msg = sprintf("Source value is NA. Cannot compare to target value %s with operator %s", self$expr, self$operator)
          }
          if(is.na(expr_value)){
            rep_type = "WARNING"
            rep_msg = sprintf("Target value is NA. Cannot compare source value %s to it with operator %s", value, self$operator)
          }
        }
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = rep_type,
          message = rep_msg
        )
      }else if(!cond){
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = self$getType(),
          message = sprintf("Source value %s is not %s %s", value, self$operator, self$expr)
        )
      }
      return(rep)
    }
    
  )
)