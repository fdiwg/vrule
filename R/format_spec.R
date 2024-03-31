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
    initialize = function(json = NULL){
      if(!is.null(json)){
        self$name = json$name
        self$urn = json$urn
        self$title = json$title
        #column_specs
        self$column_specs = lapply(json$column_specs, function(json_column_spec){
          column_spec = column_spec$new(json = json_column_spec)
          return(column_spec)
        })
      }
    },
    
    #setName
    setName = function(name){
      self$name = name
    },
    
    #setURN
    setURN = function(urn){
      self$urn = urn
    },
    
    #setTitle
    setTitle = function(title){
      self$title = title
    },
    
    #addColumnSpec
    addColumnSpec = function(column_spec){
      if(!inherits(column_spec, "column_spec")){
        stop("The column specification should be an object of class 'column_spec'")
      }
      self$column_specs[[length(self$column_specs)+1]] <- column_spec
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
      if(is.null(cspec)) cspec = self$getColumnSpecByAlias(alias = column)
      return(cspec)
    },
    
    #validateStructure
    validateStructure = function(data){
      if(tibble::is_tibble(data)){
        data = as.data.frame(data)
      }
      
      empty_rep = data.frame(
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
      
      structure_report = do.call("rbind", lapply(self$column_specs, function(column_spec){
        rep <- empty_rep
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
    validateContent = function(data, parallel = FALSE, ...){
      if(tibble::is_tibble(data)){
        data = as.data.frame(data)
      }
      
      column_specs <- lapply(colnames(data), self$getColumnSpec)
      null_column_specs <- sapply(column_specs, is.null)
      
      
      if(length(null_column_specs)>0){
        #ensure we ignore columns not part of the part (no column_spec)
        data = data[which(!null_column_specs)]
        #ensure we retain only non null column specs
        column_specs = column_specs[!null_column_specs]
      }

      validatePair = function(i,j, data, column_specs){
        rep = column_specs[[j]]$validate(value = data[i,j], row = data[i,])
        if(nrow(rep$report)==0) return(data.table::data.table(
          i = integer(0), j = integer(0), row = character(0), 
          col = character(0), col_alias = character(0),
          category = character(0), rule = character(0),
          type = character(0), message = character(0)
        ))
        column_name = colnames(data)[j]
        return(data.table::data.table(
          i = i, j = j, row = paste("Row",i), 
          col = column_specs[[j]]$name, col_alias = if(column_name %in% column_specs[[j]]$aliases) column_name else NA,
          category = rep$report$category, rule = rep$report$rule,
          type = rep$report$type, message = rep$report$message
        ))

      }
      
      content_report <- if(parallel){
        parallel_handler = NULL
        if(Sys.info()[1] != "Windows"){
          data.table::rbindlist(parallel::mclapply(1:nrow(data), function(i, data, column_specs, validatePair){
            data.table::rbindlist(lapply(1:ncol(data), function(j){
              c(i, j, data, column_specs)
            }))
          }, data = data, 
          column_specs = column_specs, 
          validatePair = validatePair,
          mc.cores = getVruleOption("cores")))
        }else{
          cl <- parallel::makeCluster(min(nrow(data), getVruleOption("cores")))
          parallel::clusterEvalQ(cl, library("vrule"))
          parallel::clusterEvalQ(cl, library("data.table"))
          parallel::clusterExport(cl, varlist= "data")
          out = data.table::rbindlist(
            parallel::parLapply(
              cl = cl, 1:nrow(data), 
              fun = function(i, data, column_specs, validatePair){
                data.table::rbindlist(lapply(1:ncol(data), function(j){
                  validatePair(i, j, data, column_specs)
                }))
              },
              data = data,
              column_specs = column_specs,
              validatePair = validatePair
            )
          )
          parallel::stopCluster(cl)
          out
        }
      }else{
        data.table::rbindlist(lapply(1:nrow(data), function(i){
          data.table::rbindlist(lapply(1:ncol(data), function(j){
            validatePair(i, j, data, column_specs)
          }))
        }))
      }

      return(content_report)
      
    },
    
    #validate
    validate = function(data, parallel = FALSE, ...){
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
      content_report = self$validateContent(data = data, parallel = parallel, ...)
      
      #3. check duplicates
      #TODO
      
      report = rbind(structure_report, content_report)
      return(report)
    }, 
    
    #display_as_handsontable
    display_as_handsontable = function(data, report, ...){
      
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
        rows_with_warning = rows_with_warning-1,
        cols_with_warning = cols_with_warning-1,
        rows_with_error = rows_with_error-1,
        cols_with_error = cols_with_error-1,
        ...
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
                row_warning_to_highlight = instance.params.rows_with_warning
                row_warning_to_highlight = row_warning_to_highlight instanceof Array ? row_warning_to_highlight : [row_warning_to_highlight]
                col_warning_to_highlight = instance.params.cols_with_warning
                col_warning_to_highlight = col_warning_to_highlight instanceof Array ? col_warning_to_highlight : [col_warning_to_highlight]
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
                row_error_to_highlight = instance.params.rows_with_error
                row_error_to_highlight = row_error_to_highlight instanceof Array ? row_error_to_highlight : [row_error_to_highlight]
                col_error_to_highlight = instance.params.cols_with_error
                col_error_to_highlight = col_error_to_highlight instanceof Array ? col_error_to_highlight : [col_error_to_highlight]
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
      
      report$pair = paste(report$i,report$j,sep="_")
      for(pair in unique(report$pair)){
        cell_rep = report[report$pair == pair,]
        cell_report <- paste0(sapply(1:nrow(cell_rep), function(idx){
          paste0("- ", cell_rep[idx, "type"], ": ", cell_rep[idx, "message"])
        }), collapse="\n")
        out_tbl <- out_tbl %>%
          hot_cell(cell_rep[1L,"i"], cell_rep[1L,"j"], comment = cell_report)
      }
      out_tbl
    },
    
    #validate_and_display_as_handsontable
    validate_and_display_as_handsontable = function(data, parallel = parallel,
                                                    read_only = TRUE, use_css_classes = FALSE, ...){
      
      report = self$validate(data = data, parallel = parallel, ...)
      report = report[report$category != "Data structure",]
      self$display_as_handsontable(data = data, report = report, read_only = read_only, use_css_classes = use_css_classes)
    },
    
    #standardizeStructure
    standardizeStructure = function(data, exclude_unused = TRUE){
      if(is.tibble(data)) data = as.data.frame(data)
      format_spec_cols = sapply(self$column_specs, function(x){x$name})
      data_names<-names(data)
      for (i in 1:length(self$column_specs)){
        target<-self$column_specs[[i]]
        std_name<-target$name
        alt_names<-unlist(target$aliases)
        if(std_name%in%data_names){}else{
          if(any(alt_names%in%data_names)){
            usedName<-alt_names[alt_names%in%data_names]
            names(data)[names(data) == usedName] <- std_name
          }
        }
      }
      if(exclude_unused){
        data<-data[intersect(format_spec_cols,names(data))]
      }
      return(data)
    },
    
    #standardizeContent
    standardizeContent = function(data){
      if(is.tibble(data)) data = as.data.frame(data)
      data = self$standardizeStructure(data, exclude_unused = T)
      cl_col_specs = self$column_specs[sapply(self$column_specs, function(x){x$hasCodelist()})]
      
      if(length(cl_col_specs)>0){
        cols<-sapply(cl_col_specs, function(x){x$name})
        correct_order<-names(data)
        data$row_order<-1:nrow(data)
        for(col_spec in cl_col_specs){
          col = col_spec$name
          if(col %in% correct_order){
            all_ref<-col_spec$rules[[1]]$ref_data
            all_ref<-as.data.frame(all_ref)
            ref<-unique(subset(all_ref,label%in%unique(data[,col]),select=c(code,label)))
            
            data_not_mappable = data[(!data[,col] %in% all_ref$code) & (!data[,col] %in% all_ref$label),]
            data_to_map = data[(!data[,col] %in% ref$code) & data[,col] %in% ref$label,]
            data_to_map<-merge(data_to_map,ref,by.x=col,by.y="label",all.x=T,sort=F)
            data_to_map[,col] <- data_to_map$code
            data_to_map$code <- NULL
            data_not_to_map = data[data[,col] %in% all_ref$code,]
            data<- rbind(data_not_mappable, data_to_map, data_not_to_map)
            data<- data[order(data$row_order),]
            data<-subset(data,select=c(correct_order,"row_order"))
          }
          
        }
        data <- data[order(data$row_order), ]
        data <-subset(data,select=-c(row_order))
        row.names(data)<-1:nrow(data)
      }
      return(data)
    }
  )
)
