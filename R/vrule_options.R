#'@name setVruleOptions
#'@title setVruleOptions
#'@param ... options
#'@export
setVruleOptions = function(...){
 args = list(...)
 opts = names(args)
 if(any(!opts %in% names(.vrule.options))){
   stop(sprintf("Options [%s] are not valid options in vrule", 
                paste0(opts[!opts %in% names(.vrule.options)],collapse=",")))
 }
 for(opt in opts){
   .vrule.options[[opt]] = args[[opt]]
 }
}

#'@name getVruleOptions
#'@title getVruleOptions
#'@export
getVruleOptions = function(){
  return(as.list(.vrule.options))
}

#'@name getVruleOption
#'@title getVruleOption
#'@param opt an option name
#'@return the value for that option
#'@export
getVruleOption = function(opt){
  return(.vrule.options[[opt]])
}