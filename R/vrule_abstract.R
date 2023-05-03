#'@name vrule_abstract
#'@export
vrule_abstract <- R6Class("vrule_abstract",
  private = list(
    category = NA,
    name = NA
  ),
  public = list(
    initialize = function(...){
    },
    
    validate = function(value, ...){
      empty_rep = vrule_report$new()
      return(empty_rep)
    },
    
    getCategory = function(){
      return(private$category)
    },
    
    getName = function(){
      return(private$name)
    }
  )
)

vrule_abstract$getClasses = function(extended = FALSE, pretty = FALSE){
  getClassesInheriting(classname = "vrule_abstract", extended = extended, pretty = pretty)
}

vrule_abstract$getClassByName = function(name){
  clazz <- NULL
  list_of_classes <- getVruleClasses()
  classname <- list_of_classes[name == list_of_classes]
  if(length(classname)>0){
    clazz <- try(eval(parse(text=classname[1])))
  }
  return(clazz)
}

#' @name getClassesInheriting
#' @aliases getClassesInheriting
#' @title getClassesInheriting
#'
#' @param classname the name of the superclass for which inheriting sub-classes have to be listed
#' @param extended whether we want to look at user namespace for third-party sub-classes
#' @param pretty prettify the output as \code{data.frame}

#' @export
#' @description get the list of classes inheriting a given super class provided by its name
#'
#' @usage getClassesInheriting(classname, extended, pretty)
#'
#' @examples
#'   getClassesInheriting("vrule_integer")
getClassesInheriting <- function(classname, extended = FALSE, pretty = FALSE){
  list_of_classes <- ls(getNamespaceInfo("vrule", "exports"))
  if(extended) {
    search_envs <- search()
    search_envs <- search_envs[search_envs!="package:vrule"]
    list_of_other_classes <- unlist(sapply(search_envs, ls))
    list_of_classes <- c(list_of_classes, list_of_other_classes)
  }
  
  list_of_classes <- list_of_classes[sapply(list_of_classes, function(x){
    clazz <- try(eval(parse(text=x)),silent=TRUE)
    if(is(clazz, "try-error")) clazz <- try(eval(parse(text=paste0("vrule::",x))),silent=TRUE)
    r6Predicate <- class(clazz)[1]=="R6ClassGenerator"
    if(!r6Predicate) return(FALSE)
    
    vruleObjPredicate <- FALSE
    superclazz <- clazz
    while(!vruleObjPredicate && !is.null(superclazz)){
      clazz_fields <- names(superclazz)
      if(!is.null(clazz_fields)) if(length(clazz_fields)>0){
        if("get_inherit" %in% clazz_fields){
          superclazz <- superclazz$get_inherit()
          vrulePredicate <- FALSE
          if("parent_env" %in% clazz_fields) vrulePredicate <- environmentName(superclazz$parent_env)=="vrule"
          vruleObjPredicate <- superclazz$classname == classname && vrulePredicate
        }else{
          break
        }
      }
    }
    return(vruleObjPredicate)
  })]
  
  list_of_classes <- as.vector(list_of_classes)
  if(pretty){
    std_info <- do.call("rbind",lapply(list_of_classes, function(x){
      clazz <- try(eval(parse(text=x)),silent=TRUE)
      if(is(clazz,"try-error")) clazz <- try(eval(parse(text=paste0("vrule::",x))),silent=TRUE)
      std_info <- data.frame(
        environment = environmentName(clazz$parent_env),
        name = clazz$public_fields$name,
        stringsAsFactors = FALSE
      )
      return(std_info)
    }))
    
    list_of_classes <- cbind(
      class = list_of_classes,
      std_info,
      stringsAsFactors = FALSE
    )
  }
  return(list_of_classes)
}

#' @name getVruleClasses
#' @aliases getVruleClasses
#' @title getVruleClasses
#' @export
#' @description get the list of validation classes, ie classes extending \link{vrule_abstract} super class,
#' including classes eventually defined outside \pkg{vrule}. In case the latter is on the search path,
#' the list of validation classes will be cached for optimized used by \pkg{vrule} encoder/decoder.
#'
#' @usage getVruleClasses()
#'
#' @examples
#'   getVruleClasses()
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getVruleClasses <- function(){
  if("package:vrule" %in% search()){
    if(is.null(.vrule$classes)){
      .vrule$classes <- getClassesInheriting(classname = "vrule_abstract", extended = TRUE, pretty = FALSE)
    }
    return(.vrule$classes)
  }else{
    getClassesInheriting(classname = "vrule_abstract", extended = TRUE, pretty = FALSE)
  }
}