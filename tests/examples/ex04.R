
library(rtercen)
library(dplyr)
library(reshape2)

# your Tercen workflowId and stepId
 
workflowId="c6b3d6b21d4275772b5fcf238a004ac2"
stepId="e059ac80-7340-11e6-f184-8f67f6463a1b"

# create a Tercen client object using your username and password
# client = TercenClient$new(myusername, mypassword)

# create a Tercen client object using your username and password store in .Rprofile file
# serviceUri points to local vagrant for testing
client = TercenClient$new(serviceUri="https://vagrant.tercen.com/service")
# serviceUri points to tercen.com
# client = TercenClient$new()

# get the cube query defined by your workflow
query = client$getCubeQuery(workflowId, stepId)
# inspect query as json
q =query$toJson()

# execute the query and get the data
cube = query$execute()

# get the label factors name : they are prefixed by the table id
labelFactorNames = sapply(query$labelColumns, function(each) each$name)

# remove table prefix from factor name
labelColumnNames = rtercen::removeTablePrefix(labelFactorNames)

# get the data as data.frame
df = cube$sourceTable$as.data.frame()

# add colSeq
nCubeCols = cube$columnsTable$getNRows()
colSeq=((df[['.ids']] - 1) %% nCubeCols)+1
df2 = cbind(df,.colSeq=colSeq)
 
# cast formula based on labelColumnNames ~ .colSeq
castFormula = as.formula(paste(paste(labelColumnNames,
                                     collapse='+'),
                               '.colSeq',
                               sep='~'))
castFormula
labelColumnNames

# compute the cast with mean as aggregator function
mean.mat = reshape2::acast(df2,
                           castFormula,
                           value.var='.values',
                           fun.aggregate=mean)

tail(mean.mat)
 
# compute the linear regression
doLinreg = function(dataX, dataY){
  
  nPoints = length(dataX)
  
  if (nPoints > 1){
    aLm = try(lm(dataY ~ dataX+0), silent = TRUE)
    if(!inherits(aLm, 'try-error')){
      slope = aLm$coefficients[[1]]
      intercept = 0
      ssY = sum((dataY-mean(dataY))^2)
      yFit = predict(aLm)
      R2 = 1-(sum((dataY-yFit)^2) / ssY)
    } else {
      slope = NaN
      intercept = NaN
      R2 = NaN
    }
  } else if (nPoints == 1){
    slope = dataY/dataX
    intercept = 0;
    R2 = 1
  } else if (nPoints == 0){
    slope = NaN
    intercept = NaN
    R2 = NaN
  }
  return (data.frame(slope=slope,
                     intercept = intercept,
                     R2 = R2))
}

# this function return the lower ids of a symetric matrix
symmetricMatrixIndices <- function(nrows) {
  z <- sequence(nrows)
  row = unlist(lapply(2:nrows, function(x) x:nrows), use.names = FALSE)
  col = rep(z[-length(z)], times = rev(tail(z, -1))-1)
  ids = (row -1)* nrows + col 
  ids
}
 
# generate the lower ids of a symetric matrix with nCubeCols columns/rows
mat.sym.ind = symmetricMatrixIndices(nCubeCols)

# compute the linear regression
list.linReg = lapply(mat.sym.ind , function(id){
  ri = ((id-1) %/% nCubeCols) + 1
  ci = ((id-1) %% nCubeCols) + 1
  y = mean.mat[,ri]
  x = mean.mat[,ci]
  dd = doLinreg(x,y)
  dd = data.frame(dd , ids=id-1)
  return(dd)
})

# combine all results
d.linReg = data.table::rbindlist(list.linReg)

d.linReg = d.linReg[!is.nan(d.linReg$R2)]
# filter(d.linReg, R2==NaN)
d.linReg

# prepend factor names by the namespace
factorNS = query$operator$namespace
factorNames = paste(factorNS, c('slope', 'intercept', 'R2'), sep='.')
factorNames = setNames(c('slope', 'intercept', 'R2'), factorNames)
# rename
d.linReg=rename_(d.linReg, .dots=factorNames)
 
# create table
table.linReg = Table$new(df=d.linReg)
 
# int32 is not yet supported
table.linReg$getColumn('ids')$getValues()$setSubType('uint32')

# pairwise is based on the query columns
pairwiseColumns = sapply(query$colColumns, function(c) c$name)
pairwiseColumns

# create a pairwise relation using the query relation, query columns and the auery namespace
pairWiseRelation = rtercen::pairwise(relation=query$relation,
                                     group=pairwiseColumns, 
                                     prefix=query$operator$namespace)

# create simple relation using the linreg table name
linRegRelation = rtercen::relation(table.linReg$getName())
 
# join the pairwise relation with the linreg relation base on column ids
pairWise.linReg.relation = rtercen::join1(leftRelation=pairWiseRelation,
                                     rightRelation=linRegRelation,
                                     leftColumns=pairWiseRelation$idsName,
                                     rightColumns=rtercen::addTablePrefix(linRegRelation$id, 'ids'))

# link everything to the query relation
joinOperator = rtercen::join(pairWise.linReg.relation,
                     leftColumns=pairWiseRelation$group,
                     rightColumns=pairWiseRelation$pairGroup)

# create the result object
result = CubeOperatorTableResult$new(tables=list(table.linReg),
                                     joinOperators=list(joinOperator))

# send the result to Tercen
query$setResult(result=result)
