# Tercen R client

## Install rtercen package

```
devtools::install_github("tercen/rtercen")
```

## Running Tercen R operator locally

Go on tercen.com upload some data, create a workflow and a data step.
Configure your data step, and retreive the workflowId and stepId from the url.
Example : https://tercen.com/core/index.html#ds/{workflowId}/{stepId}

```
library(rtercen)
library(plyr)

# your Tercen workflowId and stepId
workflowId="8ffe06b48d05228062f07eeda50077ab"
stepId="9b0d7ee0-3ac0-11e5-e069-0972630e148d"

# create a Tercen client object using your username and password
client = TercenClient$new(myusername, mypassword)

# get the cube query defined by your workflow
query = client$getCubeQuery(workflowId, stepId)

# execute the query and get the data
cube = query$execute()

# execute some computation
computed.df <- ddply(cube$sourceTable$as.data.frame(), c("ids"), summarize, mean = mean(values))

# send the result to Tercen
query$setResult(computed.df)

```

## Deploy on Tercen

Go on tercen.com and create an operator, set its source code to the following.

```
library(rtercen)
library(plyr)
  
query = TercenClient$new()$getCubeQuery()
computed.df <- ddply(query$execute()$sourceTable$as.data.frame(), c("ids"), summarize, mean = mean(values))
query$setResult(computed.df)

```
