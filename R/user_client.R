  
UserClient <- R6Class(
  'UserClient',
  inherit=ClientImpl,
  private = list(
    createQueryTask = function(cubeQuery){
      query = toJSON(cubeQuery$toJson())
      response <- POST(private$getUri("/tableSchema/cubeQuery"), add_headers(authorization = private$authToken), body=query)
      if (status_code(response) != 200){
        private$faildResponse(response, "createQueryTask")
      } 
      object = content(response)
      return (object)
    },
    waitTaskDone = function(id){
      query = list(type=unbox("task_wait_done"), id=id)
      response <- POST(private$getUri("/task/query"), add_headers(authorization = private$authToken), body=query, encode = "json")
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
      params = list(type=unbox("StoreDataStepTaskResultRun"), fileName=unbox(fileName), dataStepTaskId=unbox(taskId))
      task = list(name=unbox("SendRSessionResult"), runParam=params)
      query = list(type=unbox("task_create") , task=task)
      response <- POST(private$getUri("/task/query"), add_headers(authorization = private$authToken), body=query , encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "sendTaskResult")
      } 
      object = content(response)
      return (object$task)
    },
    setTempFile = function(object){
      response <- PUT(private$getUri("/tempFile"), add_headers(authorization = private$authToken), body=object)
      if (status_code(response) != 201){
        private$faildResponse(response, "setTempFile")
      } 
      object = content(response)
      return (object$name) 
    },
    sendCommand = function(command){
      if (is.null(command)) stop("command is required")
      if (!inherits(command, "Command")) stop("command is not aCommand object")
      response = NULL
      if (inherits(command, "LocalRunStepCommand") || inherits(command, "ResetStepCommand")){
        url = NULL
        if (inherits(command, "LocalRunStepCommand")) {
          url = private$getUri("/workflow/run")
        } else {
          url = private$getUri("/workflow/reset")
        }
        query = command$toJson()
        
        response <- POST(url, add_headers(authorization = private$authToken), body=query , encode = "json")
        if (status_code(response) != 200){
          private$faildResponse(response, "sendCommand")
        } 
        object = content(response)
        
        return (CommandBuffer$new(json=object))
      } else {
        stop("unknwon command")
      }
    },
    sendTaskResult = function(taskId, filename){
      task = private$createTaskResult(taskId, filename)
      taskHolder = private$waitTaskDone(task[["_id"]])
      if (taskHolder$task$state == -1){
        stop(taskHolder$task$errors)
      }
    }
  ),
  public = list(
    initialize = function(username=NULL,password=NULL,authToken=NULL, serviceUri="https://tercen.com/service"){
       super$initialize(username=username,password=password,authToken=authToken, serviceUri=serviceUri)
    },
    executeCubeQuery = function(cubeQuery) {
      
      taskRef = private$createQueryTask(cubeQuery)
      
      taskHolder = private$waitTaskDone(taskRef$taskId)
      
      
      filename = taskHolder$task$runParam$result$filename
      result = private$getTempFile(filename)
      json = fromTSON(result)
      
      cube = Cube$new(json=json)
      return (cube)
    },
    setResult = function(workflowId,stepId,df){
      if (is.null(workflowId)) stop("workflowId is required")
      if (!is.character(workflowId)) stop("workflowId must be of type character")
      if (is.null(stepId)) stop("stepId is required")
      if (!is.character(stepId)) stop("stepId must be of type character")
      if (is.null(df)) stop("df is required")
      if (!is.data.frame(df)) stop("df must be of type data.frame")
      table = ComputedTable$new(df=df)$toTson()
      binaryData = toTSON(table)
      filename = private$setTempFile(binaryData)
      
      workflow = self$getWorkflow(workflowId)  
      command = LocalRunStepCommand$new(workflowId=workflow$id,
                                        workflowRev=workflow$rev,
                                        stepId=stepId)
      
      commandBuffer = private$sendCommand(command)
      
      #       setStepStateCommand = commandBuffer$commands[[1]]
      setStepTaskIdCommand = commandBuffer$commands[[2]]
      #       setRevWorkflowCommand = commandBuffer$commands[[3]]
      
      taskId = setStepTaskIdCommand$taskId
      
      private$sendTaskResult(taskId, filename)
    }
  )
)