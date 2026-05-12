#' vrule_report
#' @name vrule_report
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_report <- R6Class("vrule_report",
  public = list(
    #'@field valid valid
    valid = TRUE,
    
    #'@field report report
    report = data.table::data.table(
      category = character(0), 
      rule = character(0), 
      type = character(0), 
      message = character(0)
    ),
    
    #'@description Initializes a validation report
    #'@param valid \code{TRUE} or \code{FALSE}
    #'@param report report
    initialize = function(valid = NULL, report = NULL){
      if(!is.null(valid)) self$valid = valid
      if(!is.null(report)) self$report = report
    }
  )
)

#'create_vrule_report
#'@name create_vrule_report
#'@param valid valid \code{TRUE} or \code{FALSE}
#'@param category category
#'@param rule rule
#'@param type type
#'@param message message
#'@export
create_vrule_report = function(
    valid, category, rule, type, message
){
  vrule_report$new(
    valid = valid,
    report = data.table::data.table(
      category = category, 
      rule = rule, 
      type = type, 
      message = message
    )
  )
}