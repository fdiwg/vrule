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
          json_part_args = json_part$args
          if(!is.null(json_part$type)) json_part_args = c(json_part_args, type = toupper(json_part$type))
          args = lapply(json_part_args, decodeVrules)
          names(args) = names(json_part_args)
          out_part = do.call(clazz$new, args)
        }
        return(out_part)
      })
    }else{
      if("vrule" %in% names(json)) {
        clazz = vrule_abstract$getClassByName(paste0("vrule_", json$vrule))
        json_args = json$args
        if(!is.null(json$type)) json_args = c(json_args, type = toupper(json$type))
        args = lapply(json_args, decodeVrules)
        names(args) = names(json_args)
        out = do.call(clazz$new, args)
      }
    }
  }
  return(out)
}

