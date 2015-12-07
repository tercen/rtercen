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
      private$clientImpl = UserClient$new(username=username,password=password,authToken=authToken, serviceUri=serviceUri)
    },
    getCubeQuery = function(workflowId, stepId){
      return (private$clientImpl$getCubeQuery(workflowId, stepId))
    },
    executeCubeQuery = function(cubeQuery) {
      return (private$clientImpl$executeCubeQuery(cubeQuery))
    },
    setResult = function(workflowId,stepId,df){
      return (private$clientImpl$setResult(workflowId,stepId,df))
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
      if (is.null(private$authToken)){
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
    getCubeQuery = function(workflowId, stepId){
      query = list(type=unbox("cube_query_from_stepId") , workflowId=unbox(workflowId), stepId=unbox(stepId))
      response <- POST(private$getUri("/workflow/query"), add_headers(authorization = private$authToken), body=query, encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "getCubeQuery")
      } 
      object = content(response)
      
      return (CubeQuery$new(json=object$cubeQuery))
    },
    getTask = function(id){
      query = list(type=unbox("task_get_by_id"), id=unbox(id))
      response <- POST(private$getUri("/task/query"), add_headers(authorization = private$authToken), body=query , encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "getTask")
      } 
      object = content(response)
      return (object)
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
      
    },
    setResult = function(workflowId,stepId,df){
      
    }
  )
)