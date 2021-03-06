SQLExpr <- R6Class(
  'SQLExpr',
  public = list(
    type = NULL,
    toJson = function(){
      return (list(type=unbox(self$type)))
    }
  )
)

SQLTopExpr <- R6Class(
  'SQLTopExpr',
  inherit = SQLExpr
)

#' SQLValue
#' 
#' @export  
SQLValue <- R6Class(
  'SQLValue',
  inherit = SQLExpr,
  public = list(
    type = "value",
    value = NULL,
    initialize = function(value=NULL,json=NULL){
      self$value =value
      if (!is.null(json)){
        self$value =json$value
      }
    },
    toJson = function(){
      json = super$toJson()
      json$value = unbox(self$value)
      return (json)
    }
  )
)

#' SQLColumn
#' 
#' @export  
SQLColumn <- R6Class(
  'SQLColumn',
  inherit = SQLExpr,
  public = list(
    name = NULL,
    type = "column",
    initialize = function(name=NULL,json=NULL){
      self$name =name
      if (!is.null(json)){
        self$name =json$name
      }
    },
    toJson = function(){
      json = super$toJson()
      json$name = unbox(self$name)
      return (json)
    }
  )
)

SQLOp <- R6Class(
  'SQLOp',
  inherit = SQLTopExpr,
  public = list(
    column = NULL,
    value = NULL,
    initialize = function(column=NULL, value=NULL,json=NULL){
      self$column =column
      self$value =value
      if (!is.null(json)){
        self$column =SQLColumn$new(json=json$column)
        self$value =SQLValue$new(json=json$value)
      }
    },
    toJson = function(){
      json = super$toJson()
      json$column = self$column$toJson()
      json$value = self$value$toJson()
      return (json)
    }
  )
)

#' SQLEquals
#' 
#' @export  
SQLEquals <- R6Class(
  'SQLEquals',
  inherit = SQLOp,
  public = list(
    type = "equals"
  )
)

#' SQLNotEquals
#' 
#' @export  
SQLNotEquals <- R6Class(
  'SQLNotEquals',
  inherit = SQLOp,
  public = list(
    type = "notequals"
  )
)

#' SQLGreater
#' 
#' @export  
SQLGreater <- R6Class(
  'SQLGreater',
  inherit = SQLOp,
  public = list(
    type = "greater"
  )
)

#' SQLLess
#' 
#' @export  
SQLLess <- R6Class(
  'SQLLess',
  inherit = SQLOp,
  public = list(
    type = "less"
  )
)

#' SQLNot
#' 
#' @export  
SQLNot <- R6Class(
  'SQLNot',
  inherit = SQLTopExpr,
  public = list(
    expr = NULL,
    type="not",
    initialize = function(expr=NULL, json=NULL){
      self$expr=expr
      if (!is.null(json)){
        self$expr = sqlExprFactory(json$expr)
      }
    },
    toJson = function(){
      json = super$toJson()
      json$expr = self$expr$toJson()
      return (json)
    }  
  )
)

SQLLogical <- R6Class(
  'SQLLogical',
  inherit = SQLTopExpr,
  public = list(
    exprs = NULL,
    initialize = function(exprs=NULL, json=NULL){
      if (!is.null(exprs)){
        self$exprs= lapply(exprs, function(each) sqlExprFactory(each))
      }
      
      if (!is.null(json)){
        self$exprs = lapply(json$expr, function(each) sqlExprFactory(each))  
      }
    },
    toJson = function(){
      json = super$toJson()
      json$exprs = lapply(self$exprs , function(each) each$toJson())
      return (json)
    }  
  )
)

#' SQLOr
#' 
#' @export  
SQLOr <- R6Class(
  'SQLOr',
  inherit = SQLLogical,
  public = list(
    type="or"
  )
)


#' SQLAnd
#' 
#' @export  
SQLAnd <- R6Class(
  'SQLAnd',
  inherit = SQLLogical,
  public = list(
    type="and"
  )
)

sqlExprFactory <- function(m){
  type = m$type
  if (is.null(type)) stop("sqlExprFactory : type is null")
  if (type == "value") {
    return (SQLValue$new(json=m))
  } else if (type == "column") {
    return (SQLColumn$new(json=m))
  } else if (type == "equals") {
    return (SQLEquals$new(json=m))
  } else if (type == "notequals") {
    return (SQLNotEquals$new(json=m))
  } else if (type == "greater") {
    return (SQLGreater$new(json=m))
  } else if (type == "less") {
    return (SQLLess$new(json=m))
  } else if (type == "and") {
    return (SQLAnd$new(json=m))
  } else if (type == "or") {
    return (SQLOr$new(json=m))
  } else if (type == "not") {
    return (SQLNot$new(json=m))
  } else {
    stop(paste0("SQLExpr : unknown type : " , type))
  }
}