#'@name vrule_abstract_complex
#'@export
vrule_abstract_complex <- R6Class("vrule_abstract_complex",
  inherit = vrule_abstract,
  private = list(
   category = NA,
   name = NA
  ),
  public = list(
   initialize = function(...){
     super$initialize(...)
   },
   validate = function(value, ...){
     super$validate(value, ...)
   }
   
  )                                  
)