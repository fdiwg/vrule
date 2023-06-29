#' format_spec
#' @name format_spec
#' @docType class
#' @importFrom R6 R6Class
#' @export
format_spec = R6Class("format_spec",
  
  public = list(
    name = NA,
    urn = NA,
    title = NA,
    column_specs = list(),
    initialize = function(json){
      self$name = json$name
      self$urn = json$urn
      self$title = json$title
      #column_specs
      self$column_specs = lapply(json$column_specs, function(json_column_spec){
        column_spec = column_spec$new(json = json_column_spec)
        return(column_spec)
      })
    },
    
    #getColumnSpecByName
    getColumnSpecByName = function(name){
      cspec = NULL
      cspecs = self$column_specs[sapply(self$column_specs, function(spec){spec$name == name})]
      if(length(cspecs)>0) cspec = cspecs[[1]]
      return(cspec)
    },
    
    #getColumnSpecByURN
    getColumnSpecByURN = function(urn){
      cspec = NULL
      cspecs = self$column_specs[sapply(self$column_specs, function(spec){spec$urn == urn})]
      if(length(cspecs)>0) cspec = cspecs[[1]]
      return(cspec)
    },
    
    #getColumnSpecByAlias
    getColumnSpecByAlias = function(alias){
      cspec = NULL
      cspecs = self$column_specs[sapply(self$column_specs, function(spec){alias %in% spec$aliases})]
      if(length(cspecs)>0) cspec = cspecs[[1]]
      return(cspec)
    },
    
    #getColumnSpec
    getColumnSpec = function(column){
      cspec = self$getColumnSpecByName(name = column)
      if(is.null(cspec)) cspec = self$getCOlumnSpecByAlias(alias = column)
      return(cspec)
    },
    
    #validateStructure
    validateStructure = function(data){
      if(tibble::is_tibble(data)){
        data = as.data.frame(data)
      }
      
      structure_report = do.call("rbind", lapply(self$column_specs, function(column_spec){
        rep <-data.frame(
          i = integer(0),
          j = integer(0),
          row = character(0),
          col = character(0),
          col_alias = character(0),
          category = character(0),
          rule = character(0),
          type = character(0),
          message = character(0)
        )
        columns = colnames(data)[sapply(colnames(data), function(x){
          x %in% c(column_spec$name, column_spec$aliases)
        })]
        if(column_spec$required & length(columns)==0){
          rep = data.frame(
            i = NA, j = NA,
            row = NA, col = column_spec$name, col_alias = NA,
            category = "Data structure",
            rule = "Column check",
            type = "ERROR",
            message = sprintf("No column found for mandatory format column specification '%s'", column_spec$name)
          )
        }
        if(!column_spec$required & length(columns)==0){
          rep = data.frame(
            i = NA, j = NA,
            row = NA, col = column_spec$name, col_alias = NA,
            category = "Data structure",
            rule = "Column check",
            type = "WARNING",
            message = sprintf("No column found for optional format column specification '%s'", column_spec$name)
          )
        }
        return(rep)
      }))
      ignored_columns = setdiff(colnames(data), do.call("c", lapply(self$column_specs, function(x){
        c(x$name, x$aliases)
      })))
      if(length(ignored_columns)>0){
        structure_report = rbind(
          structure_report,
          do.call("rbind", lapply(ignored_columns, function(ignored_column){
            rep = data.frame(
              i = NA, j = NA,
              row = NA, col = ignored_column, col_alias = NA,
              category = "Data structure",
              rule = "Column check",
              type = "WARNING",
              message = sprintf("No format column specification found for column '%s'. Column will be ignored", ignored_column)
            )
            return(rep)
          }))
        )
      }
      return(structure_report)
      
    },
    
    #validateContent
    validateContent = function(data, method = "grid"){
      if(tibble::is_tibble(data)){
        data = as.data.frame(data)
      }
      
      column_specs <- lapply(colnames(data), self$getColumnSpec)
      
      content_report <- switch(method,
        "grid" = {
          pairs = expand.grid(j = 1:ncol(data), i = 1:nrow(data))
          do.call("rbind", lapply(1:nrow(pairs), function(p){
            pair = pairs[p,]
            i = pair$i
            j = pair$j
            column_name = colnames(data)[j]
            column_spec = column_specs[[j]]
            column_alias = NA
            if(column_name %in% column_spec$aliases){
              column_alias = column_name
            }
            rep = NULL
            if(!is.null(column_spec)){
              rep = column_spec$validate(value = data[i,j], row = data[i,])
              rep_ext = if(nrow(rep$report)>0){
                structure(
                  list(i = i, j = j, row = paste("Row",i), 
                       col = column_spec$name, col_alias = column_alias), 
                  class = "data.frame", row.names = c(NA,-1L))
              }else{
                structure(
                  list(i = integer(0), j = integer(0), row = character(0), 
                      col = character(0), col_alias = character(0)), 
                  class = "data.frame", row.names = integer(0))
              }
              rep = cbind(rep_ext, rep$report)
            }
            return(rep)
    
          }))
        },
        "matrix" = {
          i = 0
          do.call("rbind", apply(as.matrix(data), 1, function(row){
            i <<- i+1
            row_df = structure(as.list(row), class = "data.frame", row.names = c(NA,-1L))
            do.call("rbind", lapply(1:length(row), function(j){
              
              column_name = colnames(data)[j]
              column_spec = column_specs[[j]]
              column_alias = NA
              if(column_name %in% column_spec$aliases){
                column_alias = column_name
              }
              
              rep = column_specs[[j]]$validate(row[j], row_df)
              rep_ext = if(nrow(rep$report)>0){
                structure(
                  list(i = i, j = j, row = paste("Row",i), 
                       col = column_spec$name, col_alias = column_alias), 
                  class = "data.frame", row.names = c(NA,-1L))
              }else{
                structure(
                  list(i = integer(0), j = integer(0), row = character(0), 
                       col = character(0), col_alias = character(0)), 
                  class = "data.frame", row.names = integer(0))
              }
              rep = cbind(rep_ext, rep$report)
            }))
          }))
        }
      )

      return(content_report)
      
    },
    
    #validate
    validate = function(data, method = "matrix"){
      if(tibble::is_tibble(data)){
        data = as.data.frame(data)
      }
      
      #1. check structure
      structure_report = self$validateStructure(data)
      #if some error --> we stop here
      if(nrow(structure_report)>0) if(any(structure_report$type == "ERROR")){
        return(structure_report)
      }
      
      #2. check content
      content_report = self$validateContent(data = data, method = method)
      
      #3. check duplicates
      #TODO
      
      report = rbind(structure_report, content_report)
      return(report)
    }, 
    
    #handsontable
    validate_and_display_as_handsontable = function(data, method = "matrix", use_css_classes = FALSE){
      
      report = self$validate(data = data, method = method)
      report = report[report$category != "Data structure",]
      
      #check if any warning
      rows_with_warning <- c()
      cols_with_warning <- c()
      report_with_warning <- report[report$type == "WARNING",]
      if(nrow(report_with_warning)>0){
        report_with_warning <- unique(report_with_warning[,c("row", "col")])
        report_with_warning$row <- sapply(report_with_warning$row, function(x){as.integer(gsub("Row ", "", x))})
        report_with_warning$col <- sapply(report_with_warning$col, function(x){which(colnames(data)==x)})
        row.names(report_with_warning) <- NULL
        report_with_warning <- as.matrix(report_with_warning)
        rows_with_warning <- report_with_warning[,1]
        cols_with_warning <- report_with_warning[,2]
      }
      #check if any error
      rows_with_error <- c()
      cols_with_error <- c()
      report_with_error <- report[report$type == "ERROR",]
      if(nrow(report_with_error)>0){
        report_with_error <- unique(report_with_error[,c("row", "col")])
        report_with_error$row <- sapply(report_with_error$row, function(x){as.integer(gsub("Row ", "", x))})
        report_with_error$col <- sapply(report_with_error$col, function(x){which(colnames(data)==x)})
        row.names(report_with_error) <- NULL
        report_with_error <- as.matrix(report_with_error)
        rows_with_error <- report_with_error[,1]
        cols_with_error <- report_with_error[,2]
      }
      #create handsontable
      out_tbl <- rhandsontable::rhandsontable(
        data, 
        readOnly = TRUE,
        use_css_classes = use_css_classes,
        rows_with_warning = rows_with_warning-1,
        cols_with_warning = cols_with_warning-1,
        rows_with_error = rows_with_error-1,
        cols_with_error = cols_with_error-1
      ) %>%
        hot_cols(
          #fixedColumnsLeft = 1,
          colWidths = 150,
          manualColumnResize = TRUE,
          renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (instance.params) {
                
                //use css classes
                var use_css_classes = instance.params.use_css_classes;
                //manage cells that are valid
                var cell_valid = true;
                
                //manage cells with warnings
                console.log('Warnings');
                console.log('Warning rows');
                row_warning_to_highlight = instance.params.rows_with_warning
                row_warning_to_highlight = row_warning_to_highlight instanceof Array ? row_warning_to_highlight : [row_warning_to_highlight]
                console.log(row_warning_to_highlight);
                console.log('Warning cols');
                col_warning_to_highlight = instance.params.cols_with_warning
                col_warning_to_highlight = col_warning_to_highlight instanceof Array ? col_warning_to_highlight : [col_warning_to_highlight]
                console.log(col_warning_to_highlight);
                for (var i=0; i < row_warning_to_highlight.length; i++) {
                    for(var j=0; j < col_warning_to_highlight.length; j++){
                      var warning_row = row_warning_to_highlight.length == 1? row_warning_to_highlight[i].row : row_warning_to_highlight[i];
                      var warning_col = col_warning_to_highlight.length == 1? col_warning_to_highlight[i].col: col_warning_to_highlight[i];
                      if (warning_row == row && warning_col == col) {
                          if(use_css_classes){
                            $(td).addClass('cell_with_warning');
                          }else{
                            $(td).css('background','#fff3cd');
                          }
                          cell_valid = false;
                          break; break;
                      }
                    }
                }
                
                //manage cells with errors
                console.log('Errors');
                console.log('Error rows');
                row_error_to_highlight = instance.params.rows_with_error
                row_error_to_highlight = row_error_to_highlight instanceof Array ? row_error_to_highlight : [row_error_to_highlight]
                console.log(row_error_to_highlight);
                console.log('Error cols');
                col_error_to_highlight = instance.params.cols_with_error
                col_error_to_highlight = col_error_to_highlight instanceof Array ? col_error_to_highlight : [col_error_to_highlight]
                console.log(col_error_to_highlight);
                for (var i = 0; i < row_error_to_highlight.length; i++) {
                    for(var j = 0; j < col_error_to_highlight.length; j++){
                      var error_row = row_error_to_highlight.length == 1? row_error_to_highlight[i].row : row_error_to_highlight[i];
                      var error_col = col_error_to_highlight.length == 1? col_error_to_highlight[i].col: col_error_to_highlight[i];
                      if (error_row == row && error_col == col) {
                          if(use_css_classes){
                            $(td).addClass('cell_with_error');
                          }else{
                            $(td).css('background','#f8d7da');
                          }
                          cell_valid = false;
                          break; break;
                      }
                    }
                }
                
                if(cell_valid){
                  if(use_css_classes){
                    $(td).addClass('cell_valid');
                  }else{
                    $(td).css('background','#d4edda');
                  }
                }
            }
            }") 
      
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          cell_report = report[report$i == i & report$j == j,]
          if(nrow(cell_report)==0){
            cell_report <- NULL
          }else{
            cell_report <- paste0(sapply(1:nrow(cell_report), function(idx){
              paste0("- ", cell_report[idx, "type"], ": ", cell_report[idx, "message"])
            }), collapse="\n")
          }
          out_tbl <- out_tbl %>% 
            hot_cell(i, j, comment = cell_report)
        }
      }
      out_tbl
    }
  )
)
