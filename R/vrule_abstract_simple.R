#'@name vrule_abstract_simple
#'@export
vrule_abstract_simple <- R6Class("vrule_abstract_simple",
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