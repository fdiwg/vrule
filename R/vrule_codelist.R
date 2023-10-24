#' vrule_codelist
#' @name vrule_codelist
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_codelist <- R6Class("vrule_codelist",
  inherit = vrule_abstract_simple,
  private = list(
    category = "Controlled terms",
    name = "Codelist"
  ),
  public = list(
    ref_data_url = NULL,
    ref_data = NULL,
    ref_data_column = "code",
    ref_meta_url = NULL,
    ref_meta = NULL,
    ref_values = NULL,
    initialize = function(ref_values = NULL,
                          ref_data_url = NULL, ref_data_column = "code",
                          ref_meta_url = NULL){
      #case of raw values
      self$ref_values = ref_values
      #case of codelist url
      if(!is.null(ref_data_url)){
        self$ref_data_url = ref_data_url
        self$ref_data_column = ref_data_column
        ref_data  = try(readr::read_csv(self$ref_data_url, guess_max = 0, show_col_types = FALSE), silent = FALSE)
        if(!is(ref_data, "try-error")){
          self$ref_data = ref_data
          if(!ref_data_column %in% colnames(self$ref_data)){
            stop(sprintf("Ref codelist '%s' doesn't have a column '%s'", 
                         self$ref_data_url, self$ref_data_column))
          }
        }else{
          stop(sprintf("Ref codelist '%s' does not exist", ref_data_url))
        }
      }
      
      if(!is.null(ref_meta_url)){
        self$ref_meta_url = ref_meta_url
        ref_meta = try(atom4R::readDCEntry(ref_meta_url), silent = FALSE)
        if(!is(ref_meta, "try-error")){
          self$ref_meta = ref_meta
        }else{
          stop(sprintf("Ref codelist metadata '%s' doesn't seem to be a Dublin Core XML file", ref_meta_url))
        }
      }
    },
    
    validate = function(value, ...){
      rep <- super$validate(value, ...)
      #ref_values
      if(!is.null(self$ref_values)){
        if(!value %in% self$ref_values){
          rep <- create_vrule_report(
            valid = FALSE,
            category = self$getCategory(),
            rule = self$getName(),
            type = "ERROR",
            message = sprintf("Source value %s is not among allowed values [%s]", 
                                value, paste0(self$ref_values, collapse=","))
          )
        }
      }
      #ref codelist
      if(!is.null(self$ref_data)){
        if(!value %in% self$ref_data[[self$ref_data_column]]){
          rep <- create_vrule_report(
            valid = FALSE,
            category = self$getCategory(),
            rule = self$getName(),
            type = "ERROR",
            message = sprintf("Source value '%s' not allowed in codelist '%s'", 
                                value, self$ref_data_url)
          )
        }
      }
      
      return(rep)
    }
  )
)