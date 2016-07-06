#' Table
#' 
#' @export  
Table <- R6Class(
  "Table",
  private = list(
    columns = NULL,
    nRows = NULL,
    fromJson = function(json){
      private$fromColumns(json$nRows, lapply(json$columns, function(jsonColumn) Column$new(json=jsonColumn)))
    },
    fromDataFrame = function(df){
      private$fromColumns(nrow(df), lapply(colnames(df), function(cname) Column$new(name=cname, vector=unlist(df[[cname]]))))
    },
    fromColumns = function(nRows,columns) {
      if (is.null(nRows)) stop("Table : nRows is required.")
      if (is.null(columns)) stop("Table : columns is required.")
      private$columns = columns
      private$nRows = nRows
    }
  ),
  public = list(
    
    initialize = function(nRows=NULL,columns=NULL,df=NULL,json=NULL){
      if (!is.null(json)){
        private$fromJson(json)
      } else if (!is.null(df)){
        private$fromDataFrame(df)
      } else {
        private$fromColumns(nRows,columns)
      }
    },
    
    getNRows = function() private$nRows,
    getColumns = function() private$columns,
    
    getColumn = function(name){
      cc <- private$columns
      list <- cc[sapply(cc, function(column) column$getName() == name)]
      if (length(list) > 0) return (list[[1]])
      return (NULL)
    },
    
    getColumnNames = function(){
      return (lapply(private$columns, function(column) column$getName()))
    },
    
    as.data.frame = function(){
      d <- data.frame(lapply(private$columns, function(column){
        values = column$getValues()
        data = values$getData()
        if (values$getType() == "factor"){
          dictionary = values$getDictionary() 
          data = factor(data, labels=dictionary)
        }
        return (data) 
      }))
      colnames(d) <- make.names(lapply(private$columns, function(column) column$getName()))
      return (d)
    },
    toTson = function() list(nRows=tson.int(private$nRows), columns=lapply(private$columns, function(c) c$toTson()))
  ) 
)
 
#' ComputedTable
#' 
#' @export  
ComputedTable <- R6Class(
  'ComputedTable',
  inherit=Table,
  private = list(
    fromDataFrame = function(df){
      super$fromDataFrame(df)
      ids = self$getColumn(".ids")
      if (is.null(ids)) stop("A column named .ids is required.")
      values = ids$getValues()
      data = values$getData()
      if (!is.integer(data)) stop("A column ids must be integer.")
      if (values$getType() != "num") stop("A column ids must be integer.")
      values$setSubType("uint32")
    }
  )
)

MatrixTable <- R6Class(
  'MatrixTable',
  inherit=Table,
  private = list(
    idColumnName = NULL,
    nMatrixCols = NULL,
    nMatrixRows = NULL,
    fromJson = function(json){
      super$fromJson(json)
      private$idColumnName=json$idColumnName
      private$nMatrixCols=json$nMatrixCols
      private$nMatrixRows=json$nMatrixRows
    }
  ),
  public = list(
    initialize = function(nRows=NULL,nMatrixCols=NULL,nMatrixRows=NULL,idColumnName=".ids",columns=NULL,df=NULL,json=NULL){
      super$initialize(nRows=nRows,columns=columns,df=df,json=json)
      if (is.null(json)){
        if (is.null(nMatrixCols)) stop("Table : nMatrixCols is required.")
        if (is.null(nMatrixRows)) stop("Table : nMatrixRows is required.")
        if (is.null(idColumnName)) stop("Table : idColumnName is required.")
        private$nMatrixCols = nMatrixCols
        private$nRows = nRows
      }
    },
    getIdColumnName = function() private$idColumnName,
    getNMatrixCols = function() private$nMatrixCols,
    getNMatrixRows = function() private$nMatrixRows
  )
)