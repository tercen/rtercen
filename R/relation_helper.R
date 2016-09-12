#' relation
#' 
#' @export 
relation = function(id) SimpleRelation$new(id=id)

#' join
#' 
#' @export 
join = function(relation, leftColumns=NULL, rightColumns=NULL){
  jop = JoinOperator1$new(rightRelation=relation,
                    leftPair=ColumnPair$new(lColumns=leftColumns,
                                            rColumns=rightColumns))
  return(jop)
}

#' join1
#' 
#' @export 
join1 = function(leftRelation, rightRelation, leftColumns=NULL, rightColumns=NULL){
  compositeRelation = NULL
  if (inherits(leftRelation, "CompositeRelation")){
    # make a copy
    compositeRelation = relationFromJson(leftRelation$toJson())
  } else {
    compositeRelation = CompositeRelation$new(mainRelation=leftRelation, joinOperators=list(), id=newUUID())
  }
   
  jop = JoinOperator1$new(rightRelation=rightRelation,
                          leftPair=ColumnPair$new(lColumns=leftColumns,
                                                  rColumns=rightColumns))
  
  compositeRelation$addJoinOperator(jop)
   
  return(compositeRelation)
}

#' prefix
#' 
#' @export 
prefix = function(relation=NULL,prefix=NULL,id=NULL){
  return(PrefixRelation$new(relation=relation,prefix=prefix,id=id))
}

#' pairwise
#' 
#' @export 
pairwise = function(relation=NULL,group=NULL, prefix=NULL,isSymetric = TRUE,
                    hasDiagonal = FALSE){
  return(PairWiseRelation$new(relation=relation,
                              group=group,
                              prefix=prefix,
                              isSymetric=isSymetric,
                              hasDiagonal=hasDiagonal, 
                              id=newUUID()))
}
