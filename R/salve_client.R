#' SlaveClient
#' 
#' @export
SlaveClient <- R6Class(
  'SlaveClient',
  inherit=ClientImpl,
  private = list(  
    slaveUri = NULL,
    getSalveUri = function(uri,...){
      return (paste0(private$slaveUri,uri,...))
    } 
  ),
  public = list(
    initialize = function(username=NULL,password=NULL,authToken=NULL, serviceUri=NULL,slaveUri=NULL,aclContext=NULL){
      super$initialize(username=username,password=password,authToken=authToken, serviceUri=serviceUri)
      private$slaveUri = slaveUri
      if (is.null(private$slaveUri)) stop("slaveUri is required")
    },
    
    getCubeQuery = function(workflowId=NULL, stepId=NULL, taskId=NULL){
      tid = taskId
      if (is.null(tid)){
        tid = current.taskId()
      } 
      
      task = self$getTask(tid)
      
      query = list(type=unbox("cube_query_from_stepId"),
                   workflowId=unbox(task$runParam$workflowId),
                   stepId=unbox(task$runParam$stepId),
                   withOperator=TRUE)
      
      response <- POST(private$getUri("/workflow/query"),
                       add_headers(authorization = private$authToken),
                       body=query, 
                       encode = "json")
      
      if (status_code(response) != 200){
        private$faildResponse(response, "getCubeQuery")
      } 
      object = content(response)
      return (TaskCubeQuery$new(self , tid, json=object$cubeQuery))
    },
    
    executeCubeQuery = function(cubeQuery) {
      query = cubeQuery$toJson()
      response <- POST(private$getSalveUri("/query/cubeQuery"),
                       add_headers(authorization = private$authToken),
                       body=query,
                       encode = "json")
    
      if (status_code(response) != 200){
        private$faildResponse(response, "executeCubeQuery")
      }         
      return (Cube$new(json=fromTSON(content(response))))
    },
    
    setResultForTaskId = function(taskId, df){
      if (is.null(taskId)) stop("taskId is required")
      if (!is.character(taskId)) stop("taskId must be of type character") 
      if (is.null(df)) stop("df is required")
      if (!is.data.frame(df)) stop("df must be of type data.frame")
      
      table = ComputedTable$new(df=df)$toTson()
      binaryData = toTSON(list(taskId=tson.character(taskId) , result=table))
      
      response <- POST(private$getSalveUri("/query/cubeQueryResult"),
                       add_headers(authorization = private$authToken),
                       body=binaryData)
      
      if (status_code(response) != 200){
        private$faildResponse(response, "setResultForTaskId")
      }   
    }
  )
)