#' vrule_cross_column
#' @name vrule_cross_column
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_cross_column <- R6Class("vrule_cross_column",
  inherit = vrule_abstract_complex,
  private = list(
    category = NA,
    name = NA,
    logical_rule = NA,
    eval_crossfield_expr = function(row,expr){
      pattern<-unlist(strsplit(expr,"[+-]|[\\*]|[/]|\\(|\\)"))
      pattern<-pattern[pattern!=""]
      replacement<-sapply(pattern,function(x)eval(parse(text=x)))
      recode_expr<-stringi::stri_replace_all_regex(expr,
                                                   pattern=pattern,
                                                   replacement=replacement,
                                                   vectorize=FALSE)
      expr<-gsub("NA",0,recode_expr)
      recode_expr = eval(parse(text=recode_expr))
      return(recode_expr)
    }
  ),
  public = list(
    operator = NA,
    expr = NA,
    initialize = function(operator, expr){
      self$operator = operator
      self$expr = expr
    },
    
    validate = function(value, row){
      expr = private$eval_crossfield_expr(row = row, expr = self$expr)
      logical_rule = vrule_operator_binary$new(operator = self$operator, expr = expr)
      logical_rule$validate(value = value) 
    }
  )
)
  