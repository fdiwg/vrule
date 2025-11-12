#' vrule_mapping
#' @name vrule_mapping
#' @docType class
#' @importFrom R6 R6Class
#' @export
vrule_mapping <- R6Class("vrule_mapping",
  inherit = vrule_abstract_complex,
  private = list(
    category = "Controlled terms",
    name = "Mapping"
  ),
  public = list(
    ref_data_url = NULL,
    ref_data = NULL,
    ref_source_term = NULL,
    ref_target_term = NULL,
    data_target_term = NULL,
    ref_meta_url = NULL,
    ref_meta = NULL,
    initialize = function(ref_data_url = NULL, 
                          ref_source_term = NULL,
                          ref_target_term = NULL,
                          data_target_term = NULL,
                          ref_meta_url = NULL, ...){
      super$initialize(...)
      #case of mapping url
      if(!is.null(ref_data_url)){
        self$ref_data_url = ref_data_url
        self$ref_source_term = ref_source_term
        self$ref_target_term = ref_target_term
        self$data_target_term = data_target_term
        ref_data  = try(readr::read_csv(self$ref_data_url, guess_max = 0, show_col_types = FALSE), silent = FALSE)
        if(!is(ref_data, "try-error")){
          ref_data = as.data.frame(ref_data)
          self$ref_data = ref_data
          if(!ref_source_term %in% colnames(self$ref_data)){
            stop(sprintf("Ref mapping '%s' doesn't have a column '%s'", 
                         self$ref_data_url, self$ref_source_term))
          }
          if(!ref_target_term %in% colnames(self$ref_data)){
            stop(sprintf("Ref mapping '%s' doesn't have a column '%s'", 
                         self$ref_data_url, self$ref_target_term))
          }
        }else{
          stop(sprintf("Ref mapping '%s' does not exist", ref_data_url))
        }
      }
      
      if(!is.null(ref_meta_url)){
        self$ref_meta_url = ref_meta_url
        ref_meta = try(atom4R::readDCEntry(ref_meta_url), silent = FALSE)
        if(!is(ref_meta, "try-error")){
          self$ref_meta = ref_meta
        }else{
          stop(sprintf("Ref mapping metadata '%s' doesn't seem to be a Dublin Core XML file", ref_meta_url))
        }
      }
    },
    
    validate = function(value, row){
      rep <- super$validate(value, row)
      #ref mapping
      target = row[,self$data_target_term]
      map = self$ref_data[
        self$ref_data[,self$ref_source_term] == value &
        self$ref_data[,self$ref_target_term] == target,]
      if(nrow(map)==0){
        rep <- create_vrule_report(
          valid = FALSE,
          category = self$getCategory(),
          rule = self$getName(),
          type = self$getType(),
          message = sprintf("Source %s '%s' and target %s '%s' are not mapped in '%s'", 
                            self$ref_source_term, value, 
                            self$ref_target_term, target,
                            self$ref_data_url)
        )
      }
      
      return(rep)
    }
  )
)