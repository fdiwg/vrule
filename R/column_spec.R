#' column_spec
#' @name column_spec
#' @docType class
#' @importFrom R6 R6Class
#' @export
column_spec <- R6Class("column_spec",
   public = list(
     name = NA,
     urn = NA,
     aliases = list(),
     required = TRUE,
     rules = list(),
     initialize = function(json){
       self$name = json$name
       self$urn = json$urn
       if(!is.null(json$aliases)) self$aliases = json$aliases
       if(!is.null(json$required)) self$required = json$required
       #rules
       if(!is.null(json$rules)){
         self$rules = lapply(json$rules, function(json_rule){
           clazz = vrule_abstract$getClassByName(json_rule$name)
           rule = do.call(clazz$new, json_rule$args)
           return(rule)
         })
       }
     },
     
     #validate
     validate = function(value){
       rep = data.frame(
         category = character(0),
         rule = character(0),
         type = character(0),
         message = character(0)
       )
       if(length(self$rules)>0){
         the_rule = do.call(vrule_and$new, self$rules)
         rep = the_rule$validate(value)
       }
       return(rep)
     },
     
     #hasCodelist
     hasCodelist = function(){
       any(sapply(self$rules, is, "vrule_codelist"))
     }
   )                       
)