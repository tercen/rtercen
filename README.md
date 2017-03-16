# Tercen R client

## Install rtercen package

```
devtools::install_github("tercen/rtercen", ref = "2.10")
```

## Optional configuration
save the following in .Rprofile file
```
options("tercen.username"="myusername")
options("tercen.password"="mypassword")
```

## Running Tercen R operator locally

Access tercen.com and upload some data, create a workflow and a data step.
Configure your data step, and retreive the workflowId and stepId from the url.
Example : https://tercen.com/core/index.html#ds/{workflowId}/{stepId}

```R
library(rtercen)
library(plyr)

# your Tercen workflowId and stepId
workflowId="ec25c9aa533ad9f655c2e5e7c4004460"
stepId="47e61df0-4c30-11e6-ad8a-795d6db3b40c"

# create a Tercen client object using your username and password
# client = TercenClient$new(myusername, mypassword)

# create a Tercen client object using your username and password store in .Rprofile file
# serviceUri points to local vagrant for testing
# client = TercenClient$new(serviceUri="https://vagrant.tercen.com/service")
# serviceUri points to tercen.com
client = TercenClient$new()

# get the cube query defined by your workflow
query = client$getCubeQuery(workflowId, stepId)
# inspect query as json
# query$toJson()

# execute the query and get the data
cube = query$execute()

# execute some computation
computed.df <- ddply(cube$sourceTable$as.data.frame(), c(".ids"), summarize, mean = mean(.values))
 
# send the result to Tercen
query$setResult(computed.df)

```

## Deploy on Tercen

Go on tercen.com and create an operator, set its source code to the following.

```R
library(rtercen)
library(plyr)
  
query = TercenClient$new()$getCubeQuery()
computed.df <- ddply(query$execute()$sourceTable$as.data.frame(), c(".ids"), summarize, mean = mean(.values))
query$setResult(computed.df)

```
