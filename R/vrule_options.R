#'@name setVruleOptions
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
#'@export
getVruleOptions = function(){
  return(as.list(.vrule.options))
}

#'@name getVruleOption
#'@export
getVruleOption = function(opt){
  return(.vrule.options[[opt]])
}