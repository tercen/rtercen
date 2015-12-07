CommandBuffer  <- R6Class(
  'CommandBuffer',
  public = list(
    workflowId = NULL,
    workflowRev = NULL,
    commands = NULL,
    initialize = function(workflowId=NULL,workflowRev=NULL,commands=NULL,json=NULL){
      self$workflowId = workflowId
      self$workflowRev = workflowRev
      self$commands = commands
      if (!is.null(json)){
        self$workflowId = json$workflowId
        self$workflowRev = json$workflowRev
        self$commands = lapply(json$commands, commandFactory )
      }
    },
    toJson = function() list(
      workflowId=unbox(self$workflowId),
      workflowRev=unbox(self$workflowRev),
      commands= lapply(self$commands , function(each) each$toJson())
    )
  )
)


commandFactory <- function(json){
  name = json$name
  command = NULL
  if (name == "SetStepTaskIdCommand"){
    command = SetStepTaskIdCommand$new(json=json)
  } else if (name == "SetStepStateCommand"){
    command = SetStepStateCommand$new(json=json)
  } else if (name == "LocalRunStepCommand"){
    command = LocalRunStepCommand$new(json=json)
  } else if (name == "ResetStepCommand"){
    command = ResetStepCommand$new(json=json)
  } else if (name == "SetRevWorkflowCommand"){
    command = SetRevWorkflowCommand$new(json=json)
  } else {
    stop(paste0("unknwon command name : ", name))
  }
  return (command)
}

Command <- R6Class(
  'Command',
  public = list(
    name=NULL,
    data=NULL,
    initialize = function(json=json){
      if (!is.null(json)){
        if (is.null(json$data)) stop("wrong command data")
        self$data=json$data
      } else {
        data = list()
      }
    },
    toJson = function() list(name=unbox(self$name), data=self$data)
  )
)

WorkflowCommand <- R6Class(
  'WorkflowCommand',
  inherit=Command,
  public = list(
    initialize = function(json=NULL, workflowId=NULL){
      super$initialize(json)
      if (!is.null(workflowId)){
        self$workflowId = workflowId
      }
    }
  ),
  active = list(
    workflowId = function(workflowId) {
      if (missing(workflowId)) return(self$data$workflowId)
      else self$data$workflowId = unbox(workflowId)
    }
  )
)

UpdateWorkflowCommand <- R6Class(
  'UpdateWorkflowCommand',
  inherit=WorkflowCommand
)

SetRevWorkflowCommand <- R6Class(
  'SetRevWorkflowCommand',
  inherit=UpdateWorkflowCommand,
  active = list(
    rev = function(rev) {
      if (missing(rev)) return(self$data$rev)
      else self$data$rev = unbox(rev)
    }
  )
)

StepCommand <- R6Class(
  'StepCommand',
  inherit=UpdateWorkflowCommand,
  active = list(
    stepId = function(stepId) {
      if (missing(stepId)) return(self$data$stepId)
      else self$data$stepId = unbox(stepId)
    }
  )
)

SetStepStateCommand <- R6Class(
  'SetStepStateCommand',
  inherit=StepCommand,
  active = list(
    state = function(state) {
      if (missing(state)) return(self$data$state)
      else self$data$state = unbox(state)
    }
  )
)

SetStepTaskIdCommand <- R6Class(
  'SetStepTaskIdCommand',
  inherit=StepCommand,
  active = list(
    taskId = function(taskId) {
      if (missing(taskId)) return(self$data$taskId)
      else self$data$taskId = unbox(taskId)
    }
  )
)

ComputationCommand <- R6Class(
  'ComputationCommand',
  inherit=WorkflowCommand,
  public = list(
    initialize = function(json=NULL, workflowId=NULL,workflowRev=NULL,stepId=NULL,username=NULL){
      super$initialize(json=json, workflowId=workflowId)
      if (!is.null(workflowRev)) self$workflowRev = workflowRev
      if (!is.null(stepId)) self$stepId = stepId
      if (!is.null(username)) self$username = username
    }
  ),
  active = list(
    workflowRev = function(workflowRev) {
      if (missing(workflowRev)) return(self$data$workflowRev)
      else self$data$workflowRev = unbox(workflowRev)
    },
    stepId = function(stepId) {
      if (missing(stepId)) return(self$data$stepId)
      else self$data$stepId = unbox(stepId)
    },
    username = function(username) {
      if (missing(username)) return(self$data$username)
      else self$data$username = unbox(username)
    }
  )
)

LocalRunStepCommand <- R6Class(
  'LocalRunStepCommand',
  inherit=ComputationCommand,
  public = list(
    name='LocalRunStepCommand'
  )
)

ResetStepCommand <- R6Class(
  'ResetStepCommand',
  inherit=ComputationCommand,
  public = list(
    name='ResetStepCommand'
  )
)