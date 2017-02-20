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
    axisQueries=NULL,
    sqlExpr=NULL,
    relation=NULL,
    operator=NULL,
    initialize = function(tercenClient, json=NULL){
      private$tercenClient = tercenClient
      if (!is.null(json)){
        self$version=as.character(json$version)
        self$rowColumns = lapply(json$rowColumns, function(each) CubeFactor$new(json=each))
        self$colColumns = lapply(json$colColumns, function(each) CubeFactor$new(json=each))
        self$axisQueries = lapply(json$axisQueries, function(each) CubeAxisQuery$new(json=each))
        if (!is.null(json$sqlExpr)){
          self$sqlExpr = sqlExprFactory(json$sqlExpr)
        }
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
        axisQueries=lapply(self$axisQueries, function(each) each$toJson()),
        relation=self$relation$toJson()
      )
      if (!is.null(self$sqlExpr)){
        json$sqlExpr = self$sqlExpr$toJson()
      }
      if (!is.null(self$operator)){
        json$operator = self$operator$toJson()
      }
      return (json)
    }
  )
)

#' CubeAxisQuery
#' 
#' @export  
CubeAxisQuery <- R6Class(
  'CubeAxisQuery',
  public = list(
    pointSize=NULL,
    chartType=NULL,
    yAxisColumn=NULL,
    xAxisColumn=NULL,
    errorColumns=NULL,
    labelColumns=NULL,
    colorColumns=NULL,
    initialize = function(json=NULL){
      if (!is.null(json)){
        self$pointSize=as.integer(json$pointSize)
        self$chartType=as.character(json$chartType)
        if (!is.null(json$yAxisColumn)){
          self$yAxisColumn = CubeFactor$new(json=json$yAxisColumn)
        }
        if (!is.null(json$xAxisColumn)){
          self$xAxisColumn = CubeFactor$new(json=json$xAxisColumn)
        }
        self$errorColumns = lapply(json$errorColumns, function(each) CubeFactor$new(json=each))
        self$labelColumns = lapply(json$labelColumns, function(each) CubeFactor$new(json=each))
        self$colorColumns = lapply(json$colorColumns, function(each) CubeFactor$new(json=each))
        
      }
    },
    toJson = function(){
      json = list(
        pointSize=unbox(self$pointSize),
        chartType=unbox(self$chartType),
        errorColumns=lapply(self$errorColumns, function(each) each$toJson()),
        labelColumns=lapply(self$labelColumns, function(each) each$toJson()),
        colorColumns=lapply(self$colorColumns, function(each) each$toJson())
      )
      if (!is.null(self$yAxisColumn)){
        json$yAxisColumn = self$yAxisColumn$toJson()
      }
      if (!is.null(self$xAxisColumn)){
        json$xAxisColumn = self$xAxisColumn$toJson()
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
    setResult = function(df){
      private$tercenClient$setResult(private$workflowId, private$stepId, df)
    },
    sendResult = function(result){
      private$tercenClient$setResult(private$workflowId, private$stepId, NULL, result=result)
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
    setResult = function(df){
      private$tercenClient$setResultForTaskId(private$taskId, df)
    },
    sendResult = function(result){
      private$tercenClient$setResultForTaskId(private$taskId, NULL, result=result)
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
