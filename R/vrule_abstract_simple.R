#'@name vrule_abstract_simple
#'@title vrule_abstract_simple
#'@docType class
#'@importFrom R6 R6Class
#'@export
vrule_abstract_simple <- R6Class("vrule_abstract_simple",
  inherit = vrule_abstract,
  private = list(
    category = NA,
    name = NA
  ),
  public = list(
    
    #'@description Initializes an abstract simple validation rule
    #'@param ... args
    initialize = function(...){
      super$initialize(...)
    },
    
    #'@description Abstract method to validate data
    #'@param value value
    #'@param ... any other args
    #'@return a validation report, object of class \link{vrule_report}
    validate = function(value, ...){
      super$validate(value, ...)
    }
  )                                  
)