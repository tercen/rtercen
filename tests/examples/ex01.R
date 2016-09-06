 
library(rtercen)
library(dplyr)

# your Tercen workflowId and stepId
workflowId="ec25c9aa533ad9f655c2e5e7c4004460"
stepId="47e61df0-4c30-11e6-ad8a-795d6db3b40c"

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

# get the color factors name : they are prefix by the table id
colorFactorNames = sapply(query$colorColumns, function(each) each$name)
# remove table prefix from factor name
colorColumnNames = rtercen::removeTablePrefix(colorFactorNames)
    
# execute some computation
computed.df = cube$sourceTable$as.data.frame() %>%
  group_by_(.dots=colorColumnNames) %>%
  summarise_(.dots = setNames(list(~mean(.values)),
                              paste0(query$operator$namespace, '.meanByColor')))
   
table = Table$new(df=computed.df)
# a uuid is auto-generated to make table name unique
# table$getName()
tlbName = table$getName()
  
# add table prefix
colorColumnNamesWithPrefix = rtercen::addTablePrefix(tlbName, colorColumnNames)

jop = rtercen::join(rtercen::relation(tlbName),
                    leftColumns=colorFactorNames,
                    rightColumns=colorColumnNamesWithPrefix))

result = CubeOperatorTableResult$new(tables=list(table),joinOperators=list(jop))

# send the result to Tercen
 
query$setResult(result=result)
 

 