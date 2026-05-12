#'@name vrule_abstract_complex
#'@title vrule_abstract_complex
#'@docType class
#'@importFrom R6 R6Class
#'@export
vrule_abstract_complex <- R6Class("vrule_abstract_complex",
  inherit = vrule_abstract,
  private = list(
   category = NA,
   name = NA
  ),
  public = list(
   #'@description Initializes an abstract complex validation rule
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