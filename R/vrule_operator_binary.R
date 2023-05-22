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
      self$operator = operator
      self$expr = expr
    },
    
    validate = function(value){
      rep = super$validate()
      expr_value = self$expr
      op_expr = sprintf("value %s expr_value", self$operator)
      cond = eval(parse(text = op_expr))
      if(!cond){
        rep <- vrule_report$new(
          valid = FALSE,
          report = data.frame(
            category = self$getCategory(),
            rule = self$getName(),
            type = "ERROR",
            message = sprintf("Source value %s is not %s %s", value, self$operator, self$expr)
          )
        )
      }
      return(rep)
    }
    
  )
)