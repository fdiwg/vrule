#' vrule_threshold
#' @name vrule_threshold
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_threshold <- R6Class("vrule_threshold",
 inherit = vrule_datatype,
 private = list(
   category = "Thresholds",
   name = "Threshold"
 ),
 public = list(
   operator = NULL,
   threshold = NULL,
   initialize = function(operator, threshold){
     self$operator = operator
     self$threshold = threshold
   },
   
   validate = function(value){
     nvr = vrule_numeric$new()
     rep = nvr$validate(value)
     if(nrow(rep)==0){
       threshold_expr = sprintf("value %s %s", self$operator, self$threshold)
       cond = eval(parse(text = threshold_expr))
       if(!cond){
         rep <- data.frame(
           category = self$getCategory(),
           rule = self$getName(),
           type = "ERROR",
           message = sprintf("Source value %s is not %s %s", value, self$operator, self$threshold)
         )  
       }
     }
     return(rep)
   }
 )
)