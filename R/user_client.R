#' UserClient
#' 
#' @export
UserClient <- R6Class(
  'UserClient',
  inherit=ClientImpl,
  private = list(
    waitTaskDone = function(id){
      query = list(type=unbox("task_wait_done"), id=id)
      response <- POST(private$getUri("/task/query"),
                       add_headers(authorization = private$authToken),
                       body=query,
                       encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "waitTaskDone")
      } 
      taskHolder = content(response)
      
      if (taskHolder$task$state == -1){
        stop(taskHolder$task$errors)
      }
      return (taskHolder)
    },
    createTaskResult = function(taskId, fileName){
      params = list(type=unbox("StoreDataStepTaskResultRun"),
                    fileName=unbox(fileName),
                    dataStepTaskId=unbox(taskId))
      task = list(name=unbox("SendRSessionResult"), runParam=params)
      query = list(type=unbox("task_create") , task=task)
      response <- POST(private$getUri("/task/query"),
                       add_headers(authorization = private$authToken),
                       body=query,
                       encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "createTaskResult")
      } 
      object = content(response)
      return (object$task)
    },
    setTempFile = function(object){
      response <- PUT(private$getUri("/tempFile"),
                      add_headers(authorization = private$authToken),
                      body=object)
      if (status_code(response) != 201){
        private$faildResponse(response, "setTempFile")
      } 
      object = content(response)
      return (object$name) 
    },
    sendTaskResult = function(taskId, filename){
      task = private$createTaskResult(taskId, filename)
      taskHolder = private$waitTaskDone(task[["_id"]])
      if (taskHolder$task$state == -1){
        stop(taskHolder$task$errors)
      }
    },
    createQueryTask = function(cubeQuery){
#       query = toJSON(cubeQuery$toJson())
#       response <- POST(private$getUri("/tableSchema/cubeQuery"), add_headers(authorization = private$authToken), body=query)
      query = cubeQuery$toJson()
      response <- POST(private$getUri("/tableSchema/cubeQuery"),
                       add_headers(authorization = private$authToken),
                       body=query,  encode = "json")
       
      if (status_code(response) != 200){
        private$faildResponse(response, "createQueryTask")
      } 
      object = content(response)
      return (object$taskId)
    },

    getCubeQueryFromWorkflow = function(workflowId, stepId){
      query = list(type=unbox("cube_query_from_stepId") ,
                   workflowId=unbox(workflowId),
                   stepId=unbox(stepId),
                   withOperator=TRUE)
      response <- POST(private$getUri("/workflow/query"),
                       add_headers(authorization = private$authToken),
                       body=query,
                       encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "getCubeQuery")
      } 
      object = content(response)
       
      return (WorkflowCubeQuery$new(self, workflowId, stepId, json=object$cubeQuery))
    }
  ),
  public = list(
    initialize = function(username=NULL,password=NULL,authToken=NULL, serviceUri="https://tercen.com/service"){
       super$initialize(username=username,password=password,authToken=authToken, serviceUri=serviceUri)
    },
    getCubeQuery = function(workflowId, stepId, taskId=NULL){
      if (is.null(taskId)){
        return (private$getCubeQueryFromWorkflow(workflowId, stepId))
      } else {
        stop("bad params")
      }
    },
    executeCubeQuery = function(cubeQuery) {
      taskId = private$createQueryTask(cubeQuery)
      taskHolder = private$waitTaskDone(taskId)
      filename = taskHolder$task$runParam$result$filename
      result = private$getTempFile(filename)
      json = fromTSON(result)
      cube = Cube$new(json=json)
      return (cube)
    },
    setResult = function(workflowId,stepId,df, result=NULL){
      if (is.null(workflowId)) stop("workflowId is required")
      if (!is.character(workflowId)) stop("workflowId must be of type character")
      if (is.null(stepId)) stop("stepId is required")
      if (!is.character(stepId)) stop("stepId must be of type character")
      
      if (is.null(df) && is.null(result) ) stop("df or result is required")
      
      if (!is.null(df)){
        if (!is.data.frame(df)) stop("df must be of type data.frame")
        table = ComputedTable$new(df=df)
        result = CubeOperatorTableResult$new(tables=list(table))$toTson()
      }  
      
      binaryData = toTSON(result$toTson())
      filename = private$setTempFile(binaryData)
      taskId = self$createComputationTask(workflowId,stepId)
      private$sendTaskResult(taskId, filename)
    }
  )
)