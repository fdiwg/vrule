#' column_spec
#' @name column_spec
#' @docType class
#' @importFrom R6 R6Class
#' @export
column_spec <- R6Class("column_spec",
   public = list(
     name = NA,
     urn = NA,
     dimension = FALSE,
     aliases = list(),
     required = TRUE,
     rules = list(),
     initialize = function(json = NULL){
       if(!is.null(json)){
         self$name = json$name
         self$urn = json$urn
         self$dimension = if(!is.null(json$dimension)) json$dimension else FALSE
         if(!is.null(json$aliases)) self$aliases = json$aliases
         if(!is.null(json$required)) self$required = json$required
         #rules
         if(!is.null(json$rules)){
           self$rules = decodeVrules(json$rules)
         }
       }
     },
     
     #setName
     setName = function(name){
       self$name = name
     },
     
     #setURN
     setURN = function(urn){
       self$urn = urn
     },
     
     #isDimension
     isDimension = function(isDimension){
       self$dimension = isDimension
     },
     
     #setAliases
     setAliases = function(aliases){
       self$aliases = aliases
     },
     
     #setRequired
     setRequired = function(required){
       self$required = required
     },
     
     #addRule
     addRule = function(rule){
       if(!inherits(rule, "vrule_abstract")){
         stop("The rule should be an vrule object")
       }
       self$rules[[length(self$rules)+1]] <- rule
     },
     
     #validate
     validate = function(values, rows){
       if(length(self$rules)>0){
         the_rule = if(length(self$rules)>1){
           do.call(vrule_operator_and$new, self$rules)
         }else{
           self$rules[[1]]
         }
         out = NULL
         if(length(values)==1){
           out = the_rule$validate(values, rows) 
         }else{
           j = which(names(rows) == self$name)
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
           out = vrule_report$new(
             valid = !any(report$type == "ERROR"),
             report = report
           )
         }
         return(out)
       }else{
         return(vrule_report$new())
       }
     },
     
     #hasCodelist
     hasCodelist = function(){
       any(sapply(self$rules, is, "vrule_codelist")) |
       any(sapply(self$rules, is, "vrule_raw_codelist"))
     }
   )                       
)