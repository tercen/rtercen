 
Workflow <- R6Class(
  'Workflow',
  public = list(
    id = NULL,
    rev = NULL,
    json = NULL,
    initialize = function(json=json){
      if (!is.null(json)){
        self$fromJson(json)
      }
    },
    fromJson = function(json){
      self$id=json[["_id"]]
      self$rev=json[["_rev"]]
    },
    toJson = function() self$json
  )
)