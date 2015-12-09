library(R6)
library(httr)
library(jsonlite)
library(rtson) 

#' Tercen Client for R
#' 
#' Access Tercen at \url{http://tercen.com} 
#'  
#' @name rtercen-package
#' @aliases rtercen
#' @docType package
#' @import R6 httr rtson jsonlite
NULL
 
#' Tercen Client
#' 
#' @export
TercenClient <- R6Class(
  'TercenClient',
  private = list(
    clientImpl = NULL
  ),
  public = list(
    initialize = function(username=NULL,password=NULL,authToken=NULL, serviceUri="https://tercen.com/service"){
      argsMap = parseCommandArgs()
      if (!is.null(argsMap$slaveUri)){
        private$clientImpl = SlaveClient$new(username=argsMap$username,
                                             password=argsMap$password,
                                             authToken=argsMap$token,
                                             serviceUri=argsMap$serviceUri,
                                             slaveUri=argsMap$slaveUri)
      } else {
        private$clientImpl = UserClient$new(username=username,password=password,authToken=authToken, serviceUri=serviceUri)
      }
    },
    createComputationTask = function(workflowId,stepId){
      return (private$clientImpl$createComputationTask(workflowId, stepId))
    },
    getCubeQuery = function(workflowId, stepId , taskId=NULL){
      return (private$clientImpl$getCubeQuery(workflowId, stepId, taskId=taskId))
    },
    executeCubeQuery = function(cubeQuery) {
      return (private$clientImpl$executeCubeQuery(cubeQuery))
    }
  )
  
)

ClientImpl <- R6Class(
  'ClientImpl',
  private = list(
    serviceUri = NULL,
    username = NULL,
    password = NULL,
    authorization = NULL,
    authToken = NULL,
    authenticate = function() {
      if (!is.null(private$username)){
        if (is.null(private$password)) stop("password is not defined")
        query = list(usernameOrEmail=unbox(private$username), password=unbox(private$password))
        response <- POST(private$getUri("/user/createSession"), body=query , encode = "json")
        if (status_code(response) != 200) stop("Authentication failed")
        session = content(response)
        private$authToken = session$tocken$tocken
      }
    },
    getUri = function(uri,...){
      return (paste0(private$serviceUri,uri,...))
    },
    faildResponse = function(response, msg){
      stop(paste0("Failed : ", msg , " : status=", status_code(response), " body=" , content(response)))
    },
    getTempFile = function(filename) {       
      if (is.null(filename)) stop("filename cannot be null")
      fname = curl::curl_escape(filename)
      url = private$getUri("/tempCSVFile/", fname)      
      response <- GET(url, add_headers(authorization = private$authToken))
      if (status_code(response) != 200){
        private$faildResponse(response, "getTempFile")
      } 
      object = content(response)      
      return (object) 
    },
    getCubeQueryFromTaskId = function(taskId){
      task = self$getTask(taskId)
      query = list(type=unbox("cube_query_from_stepId") , workflowId=unbox(task$runParam$workflowId), stepId=unbox(task$runParam$stepId))
      response <- POST(private$getUri("/workflow/query"), add_headers(authorization = private$authToken), body=query, encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "getCubeQuery")
      } 
      object = content(response)
      
      return (TaskCubeQuery$new(taskId, json=object$cubeQuery))
    },
    getCubeQueryFromWorkflow = function(workflowId, stepId){
      query = list(type=unbox("cube_query_from_stepId") , workflowId=unbox(workflowId), stepId=unbox(stepId))
      response <- POST(private$getUri("/workflow/query"), add_headers(authorization = private$authToken), body=query, encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "getCubeQuery")
      } 
      object = content(response)
       
      return (WorkflowCubeQuery$new(workflowId, stepId, json=object$cubeQuery))
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
    }
  ),
  public = list(
    initialize = function(username=NULL,password=NULL,authToken=NULL, serviceUri="https://tercen.com/service"){
      private$serviceUri = serviceUri
      private$username = username
      private$password = password
      private$authToken = authToken
      if (is.null(private$serviceUri)) stop("serviceUri is required")
      if (is.null(private$authToken) && (is.null(private$username) || is.null(private$password) ) ){
        stop("username and password or authToken are required")
      }
      private$authenticate()
    },
    createComputationTask = function(workflowId,stepId){
      workflow = self$getWorkflow(workflowId)  
      command = LocalRunStepCommand$new(workflowId=workflow$id,
                                        workflowRev=workflow$rev,
                                        stepId=stepId)
      commandBuffer = private$sendCommand(command)
      #       setStepStateCommand = commandBuffer$commands[[1]]
      setStepTaskIdCommand = commandBuffer$commands[[2]]
      #       setRevWorkflowCommand = commandBuffer$commands[[3]]
      taskId = setStepTaskIdCommand$taskId
      return(taskId)
    },
    getCubeQuery = function(workflowId, stepId, taskId=NULL){
       
    },
    getTask = function(id){
      query = list(type=unbox("task_get_by_id"), id=unbox(id))
      response <- POST(private$getUri("/task/query"), add_headers(authorization = private$authToken), body=query , encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "getTask")
      } 
      object = content(response)
      return (object$task)
    }, 
    getWorkflow = function(id){
      response <- GET(private$getUri("/workflow/workflow/", id), add_headers(authorization = private$authToken))
      if (status_code(response) != 200){
        private$faildResponse(response, "getWorkflow")
      } 
      object = content(response)
      return (Workflow$new(object))
    },
    
    geSchema = function(id){
      response <- GET(private$getUri("/tableSchema/", id), add_headers(authorization = private$authToken))
      if (status_code(response) != 200){
        private$faildResponse(response, "geSchema")
      } 
      object = content(response)
      return (object)
    },
    
    executeCubeQuery = function(cubeQuery) {
      
    }
    
  )
)