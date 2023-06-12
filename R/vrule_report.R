#' vrule_report
#' @name vrule_report
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_report <- R6Class("vrule_report",
  public = list(
    valid = TRUE,
    report = structure(
      list(
        category = character(0), 
        rule = character(0), 
        type = character(0), 
        message = character(0), 
        class = "data.frame", 
        row.names = character(0)
      )
    ),
    initialize = function(valid = NULL, report = NULL){
      if(!is.null(valid)) self$valid = valid
      if(!is.null(report)) self$report = report
    }
  )
)

#'create_vrule_report
#'@name create_vrule_report
#'@export
create_vrule_report = function(
    valid, category, rule, type, message
){
  vrule_report$new(
    valid = valid,
    report = structure(
      list(
        category = category, 
        rule = rule, 
        type = type, 
        message = message, 
        class = "data.frame", 
        row.names =c(NA,-1)
      )
    )
  )
}