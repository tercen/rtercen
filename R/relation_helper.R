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
join1 = function(leftRelation, rightRelation, leftColumns=NULL, rightColumns=NULL, id=NULL){
  compositeRelation = NULL
  if (inherits(leftRelation, "CompositeRelation")){
    # make a copy
    compositeRelation = relationFromJson(leftRelation$toJson())
  } else {
    compositeRelation = CompositeRelation$new(mainRelation=leftRelation, joinOperators=list(), id=id)
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

# Relation prefix(Relation relation, String prefix, [String id]){
#   return new PrefixRelation(id == null ? relation.id : id, relation, prefix);
# }


# Relation join1(Relation leftRelation, Relation rightRelation,
#                List<String> leftColumns, List<String> rightColumns, [String id]) {
#   CompositeRelation compositeRelation;
#   if (leftRelation is CompositeRelation) {
#     compositeRelation = leftRelation.copy();
#   } else {
#     compositeRelation = new CompositeRelation(leftRelation, [], id);
#   }
#   compositeRelation.addJoinOperator(new JoinOperator1(
#     rightRelation, new ColumnPair(leftColumns, rightColumns)));
#   return compositeRelation;
# }
