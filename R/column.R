#' Column
#' 
#' @export 
Column <- R6Class(
  "Column",
  private = list(
    name = NULL,
    values = NULL,
    fromValues = function(name,values){
      if (is.null(name) || length(name) == 0) stop("Column : name is required")
      if (is.null(values)){
        stop("Column : values is required")
      } else {
        if (!inherits(values, "ColumnValues")) {
          stop("Column : 'values' is not a ColumnValues object.")
        }
      }
      private$name = name
      private$values = values
    },
    fromVector = function(name,vector){
      private$fromValues(name,ColumnValues$new(vector=vector))
    },
    fromJson = function(json){
      private$fromValues(json$name,ColumnValues$new(json=json$values))
    }
  ),
  public = list(
    initialize = function(name=NULL,values=NULL,vector=NULL,json=NULL) {
      if (!is.null(json)){
        private$fromJson(json)
      } else {
        if (is.null(name) || length(name) == 0) stop("Column : name is required")
        if (!is.null(values)){
          private$fromValues(name, values)
        } else if (!is.null(vector)){
          private$fromVector(name, vector)
        } else  {
          stop("Column : 'values, vector or json' is required.")
        } 
      }
    },
    getName = function() private$name,
    getValues = function() private$values,
    toTson = function() list(name=tson.character(private$name), values= private$values$toTson())
  )
)









