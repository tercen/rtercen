library(rtercen)
library(plyr)

query = TercenClient$new()$getCubeQuery()

xAxiscolum = query$xaxisColumn
if (is.null(xAxiscolum)) stop('A x axis is required')

cube = query$execute()

ids  = cube$sourceTable$getColumn(".ids")$getValues()$getData()
values  = cube$sourceTable$getColumn(".values")$getValues()$getData()
x = cube$sourceTable$getColumn(rtercen::removeTablePrefix(xAxiscolum$name))$getValues()$getData()

ratio = values / x

computed.df <- data.frame(.ids=ids,ratio=ratio)
query$setResult(computed.df)
