#' decodeVrules
#' @name decodeVrules
#' @export
decodeVrules = function(json){
  out = json
  if(is.list(json)){
    if(is.null(names(json))){
      out = lapply(json, function(json_part){
        out_part = json_part
        if(!is.null(names(json_part))) if("vrule" %in% names(json_part)) {
          clazz = vrule_abstract$getClassByName(paste0("vrule_", json_part$vrule))
          args = lapply(json_part$args, decodeVrules)
          names(args) = names(json_part$args)
          out_part = do.call(clazz$new, args)
        }
        return(out_part)
      })
    }else{
      if("vrule" %in% names(json)) {
        clazz = vrule_abstract$getClassByName(paste0("vrule_", json$vrule))
        args = lapply(json$args, decodeVrules)
        names(args) = names(json$args)
        out = do.call(clazz$new, args)
      }
    }
  }
  return(out)
}

