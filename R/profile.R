.onLoad <- function (libname, pkgname) { # nocov start
  
  #deactivate scientific notation
  options(scipen=999)
  
  #hidden objects
  assign(".vrule", new.env(), envir= asNamespace(pkgname))
  assign(".vrule.options", new.env(), envir= asNamespace(pkgname))
  .vrule.options$parallel = FALSE
  .vrule.options$cores = parallel::detectCores()
  
} # nocov end
