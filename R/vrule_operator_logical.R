#' vrule_operator_logical
#' @name vrule_operator_logical
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_operator_logical <- R6Class("vrule_operator_logical",
  inherit = vrule_operator_binary,
  private = list(
    operators = c("&", "|"),
    category = "Logical operators",
    name = "Logical operator"
  ),
  public = list(
    operator_fun = NA,
    rules = list(),
    initialize = function(operator, ...){
      super$initialize(operator, expr = NULL)
      if(!operator %in% private$operators){
        stop(sprintf("Invalid logical operator '%s'. Value should be among values [%s]",
                     operator, paste0(private$operators, collapse=",")))
      }
      self$operator_fun = switch(operator, "&" = "all", "|" = "any")
      rules = list(...)
      if(!all(sapply(rules, is, "vrule_abstract"))){
        stop("At least one rule defined is not an object extending 'vrule_abstract'")
      }
      self$rules = rules
    },
    
    validate = function(value, row){
      reports = lapply(self$rules, function(rule){
        rule$validate(value, row)
      })
      rep = vrule_report$new(
        valid = do.call(self$operator_fun, lapply(reports, function(report){report$valid})),
        report = do.call("rbind", lapply(reports, function(report){report$report}))
      )
      return(rep)
    }
  )
)

#' vrule_operator_and
#' @name vrule_operator_and
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_operator_and <- R6Class("vrule_operator_and",
  inherit = vrule_operator_logical,
  private = list(
    name = "Logical AND operator"
  ),
  public = list(
    initialize = function(...){
      super$initialize(operator = "&", ...)
    },
    
    validate = function(value, row){
      super$validate(value, row)
    }
  )
)

#' vrule_operator_or
#' @name vrule_operator_or
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_operator_or <- R6Class("vrule_operator_or",
  inherit = vrule_operator_logical,
  private = list(
    name = "Logical OR operator"
  ),
  public = list(
    initialize = function(...){
      super$initialize(operator = "|", ...)
    },
    
    validate = function(value, row){
      super$validate(value, row)
    }
  )
)