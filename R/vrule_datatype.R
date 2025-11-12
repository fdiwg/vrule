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
    datatype = NA,
    na_allowed = FALSE,
    initialize = function(datatype, na_allowed = FALSE, ...){
      super$initialize(...)
      self$datatype = datatype
      self$na_allowed = na_allowed
    },
    
    validate = function(value, ...){
      rep <- super$validate(value, ...)
      if(self$na_allowed & is.na(value)) return(rep)
      
      val = suppressWarnings(as(value, self$datatype))
      if(is.na(val)){
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = self$getType(),
          message = sprintf("Value %s is not %s", value, self$datatype)
        )
      }else{
        if(!is.na(val)) if(self$datatype != "logical") {
          sci_regexpr <- "^(-?(\\d+\\.)?\\d+)[eE]([+-]?)(\\d+)$"
          checked = if(grepl(sci_regexpr, value)) FALSE else (value != val)
          if(checked){
            rep <- create_vrule_report(
              valid = FALSE,
              category = self$getCategory(),
              rule = self$getName(),
              type = self$getType(),
              message = sprintf("Source value %s is not equal to value (%s) after coercing to data type '%s'", value, val, self$datatype)
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
       super$initialize(datatype = "numeric", na_allowed = na_allowed, ...)
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
       super$initialize(datatype = "integer", na_allowed = na_allowed, ...)
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
      super$initialize(datatype = "double", na_allowed = na_allowed, ...)
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
       super$initialize(datatype = "logical", na_allowed = na_allowed, ...)
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
                             super$initialize(datatype = "character", na_allowed = na_allowed, ...)
                           }
                         )
)