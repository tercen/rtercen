
# strJson = '{"type":"union","id":"main","relations":[{"type":"relation","id":"relation1"},{"type":"relation","id":"relation2"}]}'
# u2 = UnionRelation$new(json=jsonlite::fromJSON(strJson, simplifyVector = FALSE))

# strJson = '{"type":"composite","id":"fc80a6f0-2520-11e6-a90d-c538b190e4a2","mainRelation":{"type":"union","id":"main","relations":[{"type":"relation","id":"mainTable1"},{"type":"relation","id":"mainTable2"}]},"joinOperators":[{"type":"join","rightRelation":{"type":"relation","id":"5"},"leftPair":{"lColumns":["main.sex","main.country"],"rColumns":["5._sex","5._country"]}}]}'
# relationFromJson(jsonlite::fromJSON(strJson, simplifyVector = FALSE))

#' relationFromJson
#' 
#' @export 
relationFromJson = function(json){
  type = json$type
  if (type == 'relation'){
    return (SimpleRelation$new(json=json))
  } else if (type == 'union'){
    return (UnionRelation$new(json=json))
  } else if (type == 'composite'){
    return (CompositeRelation$new(json=json))
  } else if (type == 'prefix'){
    return (PrefixRelation$new(json=json))
  } else if (type == 'pairwise'){
    return (PairWiseRelation$new(json=json))
  } else {
    stop(paste0('relationFromJson : unknown type ', type))
  }
}

#' joinOperatorFromJson
#' 
#' @export 
joinOperatorFromJson = function(json){
  type = json$type
  if (type == 'join'){
    return (JoinOperator1$new(json=json))
  } else if (type == 'melt'){
    return (JoinMeltOperator$new(json=json))
  } else {
    stop(paste0('joinOperatorFromJson : unknown type ', type))
  }
}

#' SimpleRelation
#' 
#' @export  
SimpleRelation <- R6Class(
  'SimpleRelation',
  public = list(
    id = NULL,
    initialize = function(id=NULL,json=NULL){
      self$id =id
      
      if (!is.null(json)){      
        self$id =as.character(json$id)
      }
    },
    toJson = function(){
      return (list(type=unbox('relation'),
                   id=unbox(self$id)))
    },
    toTson = function(){
      return (list(type=tson.character('relation'),
                   id=tson.character(self$id)))
    }
  )
)

#' PrefixRelation
#' 
#' @export  
PrefixRelation <- R6Class(
  'PrefixRelation',
  public = list(
    id = NULL,
    relation = NULL,
    prefix = NULL,
    initialize = function(id=NULL, relation=NULL, prefix=NULL,json=NULL){
      self$id =id
      self$relation =relation
      self$prefix =prefix
      
      if (!is.null(json)){      
        self$id =as.character(json$id)
        self$relation =relationFromJson(json$relation)
        self$prefix =as.character(json$prefix)
      }
      
      if (is.null(self$relation)) stop('relation is null')
      if (is.null(self$prefix)) stop('prefix is null')
      
    },
    toJson = function(){
      return (list(type=unbox('prefix'),
                   id=unbox(self$id),
                   relation=self$relation$toJson(),
                   prefix=unbox(self$prefix)))
    },
    toTson = function(){
      return (list(type=tson.character('prefix'),
                   id=tson.character(self$id),
                   relation=self$relation$toTson(),
                   prefix=tson.character(self$prefix)))
    }
  )
)

#' UnionRelation
#' 
#' @export 
UnionRelation <- R6Class(
  'UnionRelation',
  public = list(
    id = NULL,
    relations = NULL,
    initialize = function(id=NULL,relations=NULL,json=NULL){
      self$id =id
      self$relations =relations
      if (!is.null(json)){
        self$id =as.character(json$id)
        self$relations=lapply(json$relations, relationFromJson)         
      }
    },
    toJson = function(){
      return (list(type=unbox('union'),
                   id=unbox(self$id),
                   relations=lapply(self$relations, function(each) each$toJson())))
    },
    toTson = function(){
      return (list(type=tson.character('union'),
                   id=tson.character(self$id),
                   relations=lapply(self$relations, function(each) each$toTson())))
    }
  )
)

#' 
#' @export 
GroupByRelation <- R6Class(
  'GroupByRelation',
  public = list(
    id = NULL,
    relation = NULL,
    group = NULL,
    initialize = function(id=NULL,relation=NULL,group=NULL,json=NULL){
      self$id =id
      self$relation =relation
      self$group =group
      if (!is.null(json)){
        self$id =as.character(json$id)
        self$group =as.character(json$group)
        self$relation=relationFromJson(json$relation)         
      }
    },
    toJson = function(){
      return (list(type=unbox('group'),
                   id=unbox(self$id),
                   group=I(self$group),
                   relation=self$relation$toJson()))
    },
    toTson = function(){
      return (list(type=tson.character('group'),
                   id=tson.character(self$id),
                   group=self$group,
                   relation=self$relation$toTson()))
    }
  )
)

#' 
#' @export 
PairWiseRelation <- R6Class(
  'PairWiseRelation',
  public = list(
    id = NULL,
    relation = NULL,
    group = NULL,
    prefix = NULL,
    isSymetric = NULL,
    hasDiagonal = NULL,
    initialize = function(id=NULL,relation=NULL,group=NULL,prefix=NULL,isSymetric = TRUE,
                          hasDiagonal = FALSE,json=NULL){
      self$id =id
      self$relation =relation
      self$group =group
      self$prefix =prefix
      self$isSymetric =isSymetric
      self$hasDiagonal =hasDiagonal
      if (!is.null(json)){
        self$id = as.character(json$id)
        self$group = as.character(json$group)
        self$prefix = as.character(json$prefix)
        self$isSymetric = as.logical(json$isSymetric)
        self$hasDiagonal = as.logical(json$hasDiagonal)
        self$relation=relationFromJson(json$relation)         
      }
    },
    toJson = function(){
      return (list(type=unbox('pairwise'),
                   id=unbox(self$id),
                   group=I(self$group),
                   prefix=unbox(self$prefix),
                   isSymetric=unbox(self$isSymetric),
                   hasDiagonal=unbox(self$hasDiagonal),
                   relation=self$relation$toJson()))
    },
    toTson = function(){
      return (list(type=tson.character('pairwise'),
                   id=tson.character(self$id),
                   group=self$group,
                   prefix=tson.character(self$prefix),
                   isSymetric=tson.scalar(self$isSymetric),
                   hasDiagonal=tson.scalar(self$hasDiagonal),
                   relation=self$relation$toTson()))
    }
  ),
  active = list(
    pairGroup = function() {
      result = removeTablePrefix(self$group)
      return(addTablePrefix(self$id, result))
    },
    idsName = function() {
      return(addTablePrefix(self$id, 'ids'))
    }
    
  )
)


#' CompositeRelation
#' 
#' @export 
CompositeRelation <- R6Class(
  'CompositeRelation',
  public = list(
    id = NULL,
    mainRelation = NULL,
    joinOperators = NULL,
    initialize = function(id=NULL,mainRelation=NULL,joinOperators=NULL, json=NULL){
      self$id =id
      self$mainRelation =mainRelation
      self$joinOperators =joinOperators
      
      if (!is.null(json)){
        self$id =as.character(json$id)
        self$mainRelation=relationFromJson(json$mainRelation) 
        self$joinOperators=lapply(json$joinOperators, joinOperatorFromJson)         
      }
    },
    addJoinOperator = function(jop){
      
      print('addJoinOperator 1')
      print(length(self$joinOperators))
      
      list = self$joinOperators
      list[[length(list)+1]] = jop
      self$joinOperators = list 
      
      print('addJoinOperator 2')
      print(length(self$joinOperators))
    },
    toJson = function(){
      return (list(type=unbox('composite'),
                   id=unbox(self$id),
                   mainRelation=self$mainRelation$toJson(),
                   joinOperators=lapply(self$joinOperators, function(each) each$toJson())))
    },
    toTson = function(){
      return (list(type=tson.character('composite'),
                   id=tson.character(self$id),
                   mainRelation=self$mainRelation$toTson(),
                   joinOperators=lapply(self$joinOperators, function(each) each$toTson())))
    }
  )
)



#' ColumnPair
#' 
#' @export  
ColumnPair <- R6Class(
  'ColumnPair',
  public = list(
    lColumns = NULL,
    rColumns = NULL,
    initialize = function(lColumns=NULL,rColumns=NULL,json=NULL){
      self$lColumns = as.character(lColumns)
      self$rColumns = as.character(rColumns)
      
      if (!is.null(json)){      
        self$lColumns =as.character(json$lColumns)
        self$rColumns =as.character(json$rColumns)
      }
    },
    toJson = function(){
      return (list(lColumns=I(self$lColumns),
                   rColumns=I(self$rColumns)))
    },
    toTson = function(){
      return (list(lColumns=self$lColumns,
                   rColumns=self$rColumns))
    }
  )
)

#' JoinOperator
#' 
#' @export  
JoinOperator1 <- R6Class(
  'JoinOperator1',
  public = list(
    leftPair = NULL,
    rightRelation = NULL,
    initialize = function(rightRelation=NULL, leftPair=NULL,json=NULL){
      
      self$rightRelation =rightRelation
      self$leftPair =leftPair
      
      if (!is.null(json)){      
        self$rightRelation = relationFromJson(json$rightRelation)
        self$leftPair = ColumnPair$new(json=json$leftPair)
      }
      
      if (is.null(self$rightRelation)) stop('rightRelation is null')
    },
    toJson = function(){
      return (list(type=unbox('join'),
                   rightRelation=self$rightRelation$toJson(),
                   leftPair=self$leftPair$toJson()))
    },
    toTson = function(){
      return (list(type=tson.character('join'),
                   rightRelation=self$rightRelation$toTson(),
                   leftPair=self$leftPair$toTson()))
    }
  )
)

#' JoinMeltOperator
#' 
#' @export  
JoinMeltOperator <- R6Class(
  'JoinMeltOperator',
  public = list(
    rightRelationId = NULL,
    names = NULL,
    valueName = NULL,
    variableName = NULL,
    initialize = function(rightRelationId=NULL, names=NULL, valueName=NULL, variableName=NULL ,json=NULL){
      self$rightRelationId =rightRelationId
      self$names =names
      self$valueName =valueName
      self$variableName =variableName
      
      if (!is.null(json)){      
        self$rightRelationId = json$rightRelationId
        self$names = as.character(json$names)
        self$valueName = json$valueName
        self$variableName = json$variableName
      }
    },
    toJson = function(){
      return (list(type=unbox('melt'), 
                   rightRelationId=unbox(self$rightRelationId),
                   names=self$names,
                   valueName=unbox(self$valueName),
                   variableName=unbox(self$variableName)
                   ))
    },
    toTson = function(){
      return (list(type=tson.character('melt'), 
                   rightRelationId=tson.character(self$rightRelationId),
                   names=self$names,
                   valueName=tson.character(self$valueName),
                   variableName=tson.character(self$variableName)
      ))
    }
  )
)
