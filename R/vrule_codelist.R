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
    ref_data_column_alt = "label",
    ref_meta_url = NULL,
    ref_meta = NULL,
    allow_labels = TRUE,
    initialize = function(ref_data_url = NULL, 
                          ref_data_column = "code", ref_data_column_alt = "label",
                          allow_labels = FALSE,
                          ref_meta_url = NULL){
      #case of codelist url
      if(!is.null(ref_data_url)){
        self$ref_data_url = ref_data_url
        self$ref_data_column = ref_data_column
        self$ref_data_column_alt = ref_data_column_alt
        self$allow_labels = allow_labels
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
      #ref codelist
      if(length(which(value == self$ref_data[[self$ref_data_column]]))==0){
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = "ERROR",
          message = sprintf("Source value '%s' does not match any code in codelist '%s'", 
                              value, self$ref_data_url)
        )
        if(self$allow_labels) if(length(which(value == self$ref_data[[self$ref_data_column_alt]]))>0){
          code = self$ref_data[self$ref_data[[self$ref_data_column_alt]]==value,][[self$ref_data_column]]
          rep <- create_vrule_report(
            valid = TRUE,
            category = self$getCategory(),
            rule = self$getName(),
            type = "WARNING",
            message = sprintf("Source value '%s' does match label for code '%s' in codelist '%s'",
                              value, code, self$ref_data_url)
          )
        }
      }
      
      return(rep)
    }
  )
)