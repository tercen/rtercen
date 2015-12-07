 
SlaveClient <- R6Class(
  'SlaveClient',
  inherit=ClientImpl,
  private = list(  
    slaveUri = NULL,
    aclContext = NULL,
    getSalveUri = function(uri,...){
      return (paste0(private$slaveUri,uri,...))
    }
  ),
  public = list(
    initialize = function(username=NULL,password=NULL,authToken=NULL, serviceUri=NULL,slaveUri=NULL,aclContext=NULL){
      super$initialize(username=username,password=password,authToken=authToken, serviceUri=serviceUri)
      private$aclContext = aclContext
      if (is.null(private$aclContext)) stop("aclContext is required")
      private$slaveUri = slaveUri
      if (is.null(private$slaveUri)) stop("slaveUri is required")
    },
    executeCubeQuery = function(cubeQuery) {
      query = cubeQuery$toJson()
      response <- POST(private$getSalveUri("/query/cubeQuery"), add_headers(authorization = private$authToken, SCI_ACL_CONTEXT=private$aclContext), body=query , encode = "json")
      if (status_code(response) != 200){
        private$faildResponse(response, "executeCubeQuery")
      }         
      return (Cube$new(json=fromTSON(content(response))))
    },
    setResult = function(workflowId,stepId,df){
      if (is.null(workflowId)) stop("workflowId is required")
      if (!is.character(workflowId)) stop("workflowId must be of type character")
      if (is.null(stepId)) stop("stepId is required")
      if (!is.character(stepId)) stop("stepId must be of type character")
      if (is.null(df)) stop("df is required")
      if (!is.data.frame(df)) stop("df must be of type data.frame")
      table = ComputedTable$new(df=df)$toTson()
      binaryData = toTSON(list(workflowId=tson.character(workflowId), stepId=tson.character(stepId), result=table))
      response <- POST(private$getSalveUri("/query/cubeQueryResult"), add_headers(authorization = private$authToken, SCI_ACL_CONTEXT=private$aclContext), body=binaryData)
      if (status_code(response) != 200){
        private$faildResponse(response, "setResult")
      }   
    }
  )
)