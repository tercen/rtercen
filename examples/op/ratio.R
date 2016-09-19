library(rtercen)
library(plyr)

# client = TercenClient$new()
client = TercenClient$new(username=getOption('tercen.username'),password=getOption('tercen.password'))
# https://tercen.com/core/#ds/8ffe06b48d05228062f07eeda50077ab/25283050-7340-11e6-e7d4-39bce918e1ef
workflowId = '8ffe06b48d05228062f07eeda50077ab'
stepId='25283050-7340-11e6-e7d4-39bce918e1ef'
# get the cube query defined by your workflow
query = client$getCubeQuery(workflowId, stepId)

# query = client$getCubeQuery()

xAxiscolum = query$xaxisColumn
if (is.null(xAxiscolum)) stop('A x axis is required')

df = query$execute()$sourceTable$as.data.frame()

ids  = cube$sourceTable$getColumn(".ids")$getValues()$getData()
values  = cube$sourceTable$getColumn(".values")$getValues()$getData()
x = cube$sourceTable$getColumn(rtercen::removeTablePrefix(xAxiscolum$name))$getValues()$getData()

ratio = values / x

computed.df <- data.frame(.ids=ids,ratio=ratio)
query$setResult(computed.df)
