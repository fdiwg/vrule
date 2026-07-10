#' column_spec
#' @name column_spec
#' @docType class
#' @importFrom R6 R6Class
#' @export
column_spec <- R6Class("column_spec",
   public = list(
     
     #'@field name name
     name = NA,
     #'@field urn urn
     urn = NA,
     #'@field dimension dimension
     dimension = FALSE,
     #'@field aliases aliases
     aliases = list(),
     #'@field required required
     required = TRUE,
     #'@field rules rules
     rules = list(),
     
     #'@description Initializes a column specification
     #'@param obj object of class \link{list}
     initialize = function(obj = NULL){
       if(!is.null(obj)){
         self$name = obj$name
         self$urn = obj$urn
         self$dimension = if(!is.null(obj$dimension)) obj$dimension else FALSE
         if(!is.null(obj$aliases)) self$aliases = obj$aliases
         if(!is.null(obj$required)) self$required = obj$required
         #rules
         if(!is.null(obj$rules)){
           self$rules = decodeVrules(obj$rules)
         }
       }
     },
     
     #'@description set name
     #'@param name name
     setName = function(name){
       self$name = name
     },
     
     #'@description set URN
     #'@param urn urn
     setURN = function(urn){
       self$urn = urn
     },
     
     #'@description Set if the column is a dimension
     #'@param isDimension isDimension
     isDimension = function(isDimension){
       self$dimension = isDimension
     },
     
     #'@description Set aliases
     #'@param aliases aliases
     setAliases = function(aliases){
       self$aliases = aliases
     },
     
     #'@description Set if the column is required
     #'@param required required
     setRequired = function(required){
       self$required = required
     },
     
     #'@description Adds a validation rule
     #'@param rule rule
     addRule = function(rule){
       if(!inherits(rule, "vrule_abstract")){
         stop("The rule should be an vrule object")
       }
       self$rules[[length(self$rules)+1]] <- rule
     },
     
     #'@description Method to validate column data
     #'@param values values
     #'@param rows rows
     #'@return a validation report, object of class \link{vrule_report}
     validate = function(values, rows){
       if(length(self$rules)>0){
         the_rule = if(length(self$rules)>1){
           do.call(vrule_operator_and$new, self$rules)
         }else{
           self$rules[[1]]
         }
         report = NULL
         j = which(names(rows) == self$name)
         if(length(values)==1){
           rep = the_rule$validate(values, rows)
           if(nrow(rep$report)==0){
             report = NULL
           }else{
             ii = as.integer(row.names(rows))
             report = cbind(
               i = ii, j = j,
               row = paste("Row",ii),
               col = self$name, col_alias = NA,
               rep$report
             )
           }
         }else{
           #vectorized form
           report =  do.call("rbind", lapply(1:length(values), function(i){
             rep = the_rule$validate(value = values[[i]], row = rows[i,])
             if(nrow(rep$report)==0) return(NULL)
             ii = as.integer(row.names(rows[i,]))
             cbind(
               i = ii, j = j,
               row = paste("Row",ii),
               col = self$name, col_alias = NA,
               rep$report
             )
           }))
         }
         out = vrule_report$new(
           valid = !any(report$type == "ERROR"),
           report = report
         )
         return(out)
       }else{
         return(vrule_report$new())
       }
     },
     
     #'@description Indicates if column specification includes a codelist validation rule
     #'@return \code{TRUE} if it includes codelist validation, \code{FALSE} otherwise
     hasCodelist = function(){
       any(sapply(self$rules, is, "vrule_codelist")) |
       any(sapply(self$rules, is, "vrule_raw_codelist"))
     }
   )                       
)