.onLoad <- function (libname, pkgname) { # nocov start
  #hidden objects
  assign(".vrule", new.env(), envir= asNamespace(pkgname))
  
} # nocov end
