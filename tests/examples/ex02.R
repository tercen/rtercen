
library(rtercen)
library(dplyr)
library(reshape2)

# your Tercen workflowId and stepId
workflowId="ec25c9aa533ad9f655c2e5e7c4004460"
stepId="51c62bf0-7340-11e6-9802-ef949da8ba85"

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

# tail(df2)
 
castFormula = as.formula(paste(paste(labelColumnNames,
                                     collapse='+'),
                               '.colSeq',
                               sep='~'))
  
mean.mat = reshape2::acast(df2,
                           castFormula,
                           value.var='.values',
                           fun.aggregate=mean)

tail(mean.mat)
ncol(mean.mat)

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
      Result = 1;
      
    } else {
      slope = NaN
      intercept = NaN
      R2 = NaN
      Result = 0;
    }
  }
  if (nPoints == 1){
    slope = dataY/dataX
    intercept = 0;
    R2 = 1
    Result = 1
  }
  if (nPoints == 0){
    slope = NaN
    intercept = NaN
    R2 = NaN
    Result = 0;
  }
  return (data.frame(slope=slope,
                     intercept = intercept,
                     R2 = R2,
                     nPoints = nPoints,
                     Result = Result))
}
  
symmetricMatrixIndices <- function(nrows) {
  z <- sequence(nrows)
  row = unlist(lapply(2:nrows, function(x) x:nrows), use.names = FALSE)
  col = rep(z[-length(z)], times = rev(tail(z, -1))-1)
  ids = (row -1)* nrows + col
  ids
}

mat.sym.ind = symmetricMatrixIndices(nCubeCols)
mat.sym.ind 
  
list.linReg = lapply(mat.sym.ind , function(id){
  ri = ((id-1) %/% nCubeCols) + 1
  ci = ((id-1) %% nCubeCols) + 1
  y = mean.mat[ri,]
  x = mean.mat[ci,]
  dd = doLinreg(x,y)
#   dd = data.frame(dd , ids=id)
  dd = data.frame(dd , ids=id, col=ci, row=ri)
  return(dd)
})
 
d.linReg = data.table::rbindlist(list.linReg)
d.linReg

d.join = data.table::rbindlist(list(
  data.frame(ids=d.linReg[['ids']], col=d.linReg[['col']], row=d.linReg[['row']]),
  data.frame(ids=d.linReg[['ids']], col=d.linReg[['row']], row=d.linReg[['col']])))
d.join
 
factorNS = query$operator$namespace
factorNames = paste(factorNS, c('slope', 'intercept', 'R2', 'nPoints', 'Result'), sep='.')
factorNames = setNames(c('slope', 'intercept', 'R2', 'nPoints', 'Result'), factorNames)
  
d.linReg=select(d.linReg, -col,-row)
d.linReg=rename_(d.linReg, .dots=factorNames)
d.linReg
 
d.entryPoint = cube$columnsTable$as.data.frame()
# colnames(d.entryPoint) = rtercen::addTablePrefix(factorNS, colnames(d.entryPoint))
d.entryPoint = cbind(d.entryPoint, col=seq(1:cube$columnsTable$getNRows()))

d.entryPoint2 = cube$columnsTable$as.data.frame()
colnames(d.entryPoint2) = rtercen::addTablePrefix(factorNS, colnames(d.entryPoint2))
d.entryPoint2 = cbind(d.entryPoint2, row=seq(1:cube$columnsTable$getNRows()))

d.entryPoint
d.entryPoint2
d.join
d.linReg

table.entryPoint = Table$new(df=d.entryPoint)
table.entryPoint2 = Table$new(df=d.entryPoint2)

table.join = Table$new(df=d.join)
table.linReg = Table$new(df=d.linReg)

table.entryPoint$getColumn('col')$getValues()$setSubType('uint32')
table.entryPoint2$getColumn('row')$getValues()$setSubType('uint32')

table.join$getColumn('col')$getValues()$setSubType('uint32')
table.join$getColumn('row')$getValues()$setSubType('uint32')
table.join$getColumn('ids')$getValues()$setSubType('uint32')

table.linReg$getColumn('ids')$getValues()$setSubType('uint32')
table.linReg$getColumn(paste(factorNS, 'nPoints',sep='.'))$getValues()$setSubType('uint32')
table.linReg$getColumn(paste(factorNS, 'Result',sep='.'))$getValues()$setSubType('uint32')

 
composite.relation1 = rtercen::join1(leftRelation=rtercen::relation(table.join$getName()),
                                     rightRelation=rtercen::relation(table.linReg$getName()),
                                     leftColumns=rtercen::addTablePrefix(table.join$getName(), 'ids'),
                                     rightColumns=rtercen::addTablePrefix(table.linReg$getName(), 'ids'),
                                     id='join_linReg')

composite.relation1 = rtercen::join1(leftRelation=composite.relation1,
                                     rightRelation=rtercen::relation(table.entryPoint2$getName()),
                                     leftColumns=rtercen::addTablePrefix(table.join$getName(), 'row'),
                                     rightColumns=rtercen::addTablePrefix(table.entryPoint2$getName(), 'row'))
 
composite.relation2 = rtercen::join1(leftRelation=rtercen::relation(table.entryPoint$getName()),
                                     rightRelation=composite.relation1,
                                     leftColumns=rtercen::addTablePrefix(table.entryPoint$getName(), 'col'),
                                     rightColumns=rtercen::addTablePrefix(table.join$getName(), 'col'),
                                     id='entry_join_linReg')

# join table.entryPoint to table.join
leftColumns = sapply(query$colColumns, function(c) c$name)
leftColumns
rightColumns = rtercen::removeTablePrefix(leftColumns)
# rightColumns = rtercen::addTablePrefix(factorNS, rightColumns)
rightColumns = rtercen::addTablePrefix(table.entryPoint$getName(), rightColumns)
rightColumns
  
jop3 = rtercen::join(composite.relation2,
                     leftColumns=leftColumns,
                     rightColumns=rightColumns)
 
result = CubeOperatorTableResult$new(tables=list(table.entryPoint,
                                                 table.entryPoint2,
                                                 table.join,
                                                 table.linReg),
                                     joinOperators=list(jop3))

resultTson = result$toTson()
# send the result to Tercen

query$setResult(result=result)


