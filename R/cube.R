#' Cube
#' 
#' @export  
Cube <- R6Class(
  'Cube',
  public = list(
    columnsTable = NULL,
    rowsTable = NULL,
    sourceTable = NULL,
    initialize = function(columnsTable = NULL, rowsTable = NULL , sourceTable = NULL, json = NULL){
      self$columnsTable = columnsTable
      self$rowsTable = rowsTable
      self$sourceTable = sourceTable
      if (!is.null(json)){
        self$fromJson(json)
      } 
    },
    fromJson = function(json){
      self$columnsTable = Table$new(json= json$columnsTable)
      self$rowsTable = Table$new(json= json$rowsTable)
      self$sourceTable = MatrixTable$new(json= json$sourceTable)
    },
    toJson = function(){
      return (list(
        columnsTable = self$columnsTable$toJson(),
        rowsTable = self$rowsTable$toJson(),
        sourceTable = self$sourceTable$toJson()
      ))
    }
  )
)