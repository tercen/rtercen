#' CubeQuery
#' 
#' @export  
CubeQuery <- R6Class(
  'CubeQuery',
  private = list(
    tercenClient = NULL
  ),
  public = list(
    version=NULL,
    rowColumns=NULL,
    colColumns=NULL,
    qtColumns=NULL,
    xaxisColumn=NULL,
    colorColumns=NULL,
    labelColumns=NULL,
    sqlExpr=NULL,
    schemaIds=NULL,
    relation=NULL,
    operator=NULL,
    initialize = function(tercenClient, json=NULL){
      private$tercenClient = tercenClient
      if (!is.null(json)){
        self$version=as.character(json$version)
        self$rowColumns = lapply(json$rowColumns, function(each) CubeFactor$new(json=each))
        self$colColumns = lapply(json$colColumns, function(each) CubeFactor$new(json=each))
        self$qtColumns = lapply(json$qtColumns, function(each) CubeFactor$new(json=each))
        self$colorColumns = lapply(json$colorColumns, function(each) CubeFactor$new(json=each))
        self$labelColumns = lapply(json$labelColumns, function(each) CubeFactor$new(json=each))
        if (!is.null(json$xaxisColumn)){
          self$xaxisColumn = CubeFactor$new(json=json$xaxisColumn)
        }
        if (!is.null(json$sqlExpr)){
          self$sqlExpr = sqlExprFactory(json$sqlExpr)
        }
        self$schemaIds = as.character(json$schemaIds)
        self$relation = relationFromJson(json$relation)
        if (!is.null(json$operator)){
          self$operator = CubeQueryOperator$new(json=json$operator)
        }
        
      }
    },
    execute = function() private$tercenClient$executeCubeQuery(self),
    toJson = function(){
      json = list(
        version=unbox(self$version),
        rowColumns=lapply(self$rowColumns, function(each) each$toJson()),
        colColumns=lapply(self$colColumns, function(each) each$toJson()),
        qtColumns=lapply(self$qtColumns, function(each) each$toJson()),
        labelColumns=lapply(self$labelColumns, function(each) each$toJson()),
        colorColumns=lapply(self$colorColumns, function(each) each$toJson()),
        relation=self$relation$toJson()
      )
      if (!is.null(self$xaxisColumn)){
        json$xaxisColumn = self$xaxisColumn$toJson()
      }
      if (!is.null(self$sqlExpr)){
        json$sqlExpr = self$sqlExpr$toJson()
      }
      if (!is.null(self$schemaIds)){
        json$schemaIds = I(self$schemaIds)
      }
      if (!is.null(self$operator)){
        json$operator = self$operator$toJson()
      }
      
      return (json)
    }
  )
)

WorkflowCubeQuery <- R6Class(
  'WorkflowCubeQuery',
  inherit = CubeQuery,
  private = list(
    workflowId = NULL,
    stepId = NULL
  ),
  public = list(
    initialize = function(tercenClient, workflowId, stepId, json=NULL){
      super$initialize(tercenClient, json=json)
      
      private$workflowId = workflowId
      private$stepId = stepId
    },
    setResult = function(df=NULL, result=NULL){
      private$tercenClient$setResult(private$workflowId, private$stepId, df, result=result)
    }
  ) 
)

TaskCubeQuery <- R6Class(
  'TaskCubeQuery',
  inherit = CubeQuery,
  private = list(
    taskId = NULL
  ),
  public = list(
    initialize = function(tercenClient, taskId, json=NULL){
      super$initialize(tercenClient, json=json)
      private$taskId = taskId
    },
    setResult = function(df=NULL, result=NULL){
      private$tercenClient$setResultForTaskId(private$taskId, df, result=result)
    }
  ) 
)
 
#' CubeFactor
#' 
#' @export  
CubeFactor <- R6Class(
  'CubeFactor',
  public = list(
    name = NULL,
    type = NULL,
    initialize = function(type=NULL, name=NULL,json=NULL){
      self$type=type
      self$name =name
      if (!is.null(json)){
        self$type=as.character(json$type)
        self$name =as.character(json$name)
      }
    },
    toJson = function(){
      return (list(name=unbox(self$name),type=unbox(self$type)))
    }
  )
)
  
CubeQueryOperator = R6Class(
  'CubeQueryOperator',
  public = list(
    operatorId = NULL,
    parameters = NULL,
    namespace = NULL,
    initialize = function(operatorId=NULL, parameters=NULL,namespace=NULL,json=NULL){
      self$operatorId=operatorId
      self$parameters =parameters
      self$namespace =namespace
      if (!is.null(json)){
        self$operatorId=as.character(json$operatorId)
        self$parameters =as.list(json$parameters)
        self$namespace =as.character(json$namespace)
      }
    },
    toJson = function(){
      return (list(operatorId=unbox(self$operatorId),
                   namespace=unbox(self$namespace),
                   parameters=I(self$parameters)))
    }
  )
)
