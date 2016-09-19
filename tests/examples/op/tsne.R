library(rtercen)
  
# execute the query and get the data
query = TercenClient$new()$getCubeQuery()
# https://tercen.com/core/#ds/6a85a913c87ea0c3f35766d71c28aab7/17a371e0-aff0-11e5-a34f-e5d7ebf1d554
# workflowId = '6a85a913c87ea0c3f35766d71c28aab7'
# stepId = '17a371e0-aff0-11e5-a34f-e5d7ebf1d554'
# query = TercenClient$new()$getCubeQuery(workflowId, stepId)

cube = query$execute()
 
# get the data as data.frame
df = cube$sourceTable$as.data.frame()
   
# add colSeq and rowReq
nCubeCols = cube$columnsTable$getNRows()
colSeq=((df[['.ids']] - 1) %% nCubeCols)+1
rowSeq=((df[['.ids']] - 1) %/% nCubeCols)+1
df2 = cbind(df,.colSeq=colSeq,.rowSeq=rowSeq)

# convert the data into a matrix
data = reshape2::acast(df2, .colSeq ~ .rowSeq,
                           value.var='.values',
                           fun.aggregate=mean)
 
#run the operator
# Sets seed for reproducibility
set.seed(42) 
tsne_out = Rtsne::Rtsne(data,
                  initial_dims = dim(data)[2],
                  dims = 2, 
                  perplexity = 30, 
                  theta = 0.5, 
                  check_duplicates = FALSE, 
                  pca = TRUE)
dataOut <- tsne_out$Y
  
#extract resuts: tsne1, tsne2
tsne1<-dataOut[,1]
tsne2<-dataOut[,2]
 
#make data frame of restults
tsne.df = data.frame(tsne1,tsne2)
colnames(tsne.df) = paste(query$operator$namespace, c('tsne1', 'tsne2'), sep='.')
  
# create table
tsne.table = Table$new(df=data.frame(cube$columnsTable$as.data.frame(),
                                     tsne.df))
# create simple relation using the tsne.table
tsne.relation = rtercen::relation(tsne.table$getName())

# link everything to the query relation
joinOperator = rtercen::join(tsne.relation,
                             leftColumns=sapply(query$colColumns, function(each) each$name),
                             rightColumns=rtercen::addTablePrefix(tsne.relation$id,
                                                                  cube$columnsTable$getColumnNames()))
# create the result object
result = CubeOperatorTableResult$new(tables=list(tsne.table),
                                     joinOperators=list(joinOperator))

# send the result to Tercen
query$sendResult(result)
 
