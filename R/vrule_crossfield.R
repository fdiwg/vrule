#' vrule_crossfield_validator
#' @name vrule_crossfield_validator
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_crossfield_validator <- R6Class("vrule_crossfield_validator",
  inherit = vrule_abstract_complex,
  private = list(
    category = NA,
    name = NA,
    logical_rule = NA,
    eval_crossfield_expr = function(row,expr, key="column:"){
      pattern<-unlist(strsplit(expr,"[+-]|[\\*]|[/]|\\(|\\)"))
      pattern<-pattern[pattern!=""]
      pattern_recode<-gsub(key,"row$",pattern)
      replacement<-sapply(pattern_recode,function(x)eval(parse(text=x)))
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
      logical_rule = vrule_operator_logical$new(operator = self$operator, expr = expr)
      logical_rule$validate(value = value) 
    }
  )
)
  