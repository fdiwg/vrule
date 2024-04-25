#' vrule_datatype
#' @name vrule_datatype
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_datatype <- R6Class("vrule_datatype",
  inherit = vrule_abstract_simple,
  private = list(
    category = "Data types",
    name = "Data type"
  ),
  public = list(
    type = NA,
    na_allowed = FALSE,
    initialize = function(type, na_allowed = FALSE, ...){
      self$type = type
      self$na_allowed = na_allowed
    },
    
    validate = function(value, ...){
      rep <- super$validate(value, ...)
      if(self$na_allowed & is.na(value)) return(rep)
      
      val = suppressWarnings(as(value, self$type))
      if(is.na(val)){
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = "ERROR",
          message = sprintf("Value %s is not %s", value, self$type)
        )
      }else{
        if(!is.na(val)) if(self$type != "logical") {
          sci_regexpr <- "^(-?(\\d+\\.)?\\d+)[eE]([+-]?)(\\d+)$"
          checked = if(grepl(sci_regexpr, value)) FALSE else (value != val)
          if(checked){
            rep <- create_vrule_report(
              valid = FALSE,
              category = self$getCategory(),
              rule = self$getName(),
              type = "ERROR",
              message = sprintf("Source value %s is not equal to value (%s) after coercing to type '%s'", value, val, self$type)
            )
          }
        }
      }
      
      return(rep)
    }
  )                                  
)

#' vrule_numeric
#' @name vrule_numeric
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_numeric <- R6Class("vrule_numeric",
   inherit = vrule_datatype,
   private = list(
     name = "Numeric data type"
   ),
   public = list(
     initialize = function(na_allowed = FALSE, ...){
       super$initialize(type = "numeric", na_allowed = na_allowed)
     }
   )
)


#' vrule_integer
#' @name vrule_integer
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_integer <- R6Class("vrule_integer",
   inherit = vrule_datatype,
   private = list(
     name = "Integer data type"
   ),
   public = list(
     initialize = function(na_allowed = FALSE, ...){
       super$initialize(type = "integer", na_allowed = na_allowed)
     },
     
     validate = function(value, ...){
       rep = super$validate(value, ...)
       return(rep)
     }
   )
)

#' vrule_double
#' @name vrule_double
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_double <- R6Class("vrule_double",
  inherit = vrule_datatype,
  private = list(
    name = "Double data type"
  ),
  public = list(
    initialize = function(na_allowed = FALSE, ...){
      super$initialize(type = "double", na_allowed = na_allowed)
    },
    
    validate = function(value, ...){
      rep = super$validate(value, ...)
      return(rep)
    }
  )
)

#' vrule_logical
#' @name vrule_logical
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_logical <- R6Class("vrule_logical",
   inherit = vrule_datatype,
   private = list(
     name = "Logical data type"
   ),
   public = list(
     initialize = function(na_allowed = FALSE, ...){
       super$initialize(type = "logical", na_allowed = na_allowed)
     },
     
     validate = function(value, ...){
       rep = super$validate(value, ...)
       return(rep)
     }
   )
)

#' vrule_character
#' @name vrule_character
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_character <- R6Class("vrule_character",
                         inherit = vrule_datatype,
                         private = list(
                           name = "Character data type"
                         ),
                         public = list(
                           initialize = function(na_allowed = FALSE, ...){
                             super$initialize(type = "character", na_allowed = na_allowed)
                           }
                         )
)