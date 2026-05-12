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
    
    #'@field ref_data_url ref data url
    ref_data_url = NULL,
    #'@field ref_data ref data
    ref_data = NULL,
    #'@field ref_source_term ref source term
    ref_source_term = NULL,
    #'@field ref_target_term ref target term
    ref_target_term = NULL,
    #'@field data_target_term data target term
    data_target_term = NULL,
    #'@field ref_meta_url ref meta url
    ref_meta_url = NULL,
    #'@field ref_meta ref meta
    ref_meta = NULL,
    
    #'@description Initializes a mapping-based validation rule
    #'@param ref_data_url ref data url
    #'@param ref_source_term ref source term
    #'@param ref_target_term ref target term
    #'@param data_target_term data target term
    #'@param ref_meta_url ref metadata url
    #'@param ... any other arg
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
    
    #'@description Abstract method to validate data
    #'@param value value
    #'@param row row
    #'@return a validation report, object of class \link{vrule_report}
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