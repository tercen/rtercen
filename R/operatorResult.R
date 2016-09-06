#' Result
#' 
#' @export  
CubeOperatorTableResult <- R6Class(
  'CubeOperatorTableResult',
  public = list(
    tables = NULL,
    joinOperators = NULL,
    initialize = function(tables=list(),joinOperators=list()){
      self$tables = tables
      self$joinOperators = joinOperators
    },
    toTson = function(){
      return (list(tables=lapply(self$tables, function(table) table$toTson()),
                   joinOperators=lapply(self$joinOperators, function(jop) jop$toTson())))
    }
  )
)