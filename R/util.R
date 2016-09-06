#' parseCommandArgs
#' 
#' @export 
parseCommandArgs <- function(){
  args = commandArgs(trailingOnly = TRUE)
  index = 1
  list = list()
  while (index <= length(args)){
    argv = args[[index]]
    if (argv == "--token"){
      index = index + 1
      if (index > length(args)) showUsage()
      list[["token"]] = args[[index]]
    } else if (argv == "--taskId"){
      index = index + 1
      if (index > length(args)) showUsage()
      list[["taskId"]] = args[[index]]
    } else if (argv == "--slaveUri"){
      index = index + 1
      if (index > length(args)) showUsage()
      list[["slaveUri"]] = args[[index]]
    } else if (argv == "--serviceUri"){
      index = index + 1
      if (index > length(args)) showUsage()
      list[["serviceUri"]] = args[[index]]
    } else if (argv == "--username"){
      index = index + 1
      if (index > length(args)) showUsage()
      list[["username"]] = args[[index]]
    } else if (argv == "--password"){
      index = index + 1
      if (index > length(args)) showUsage()
      list[["password"]] = args[[index]]
    }
    index = index + 1
  }
  return (list);
}
#' current.taskId
#' 
#' @export 
current.taskId = function() parseCommandArgs()$taskId

showUsage <- function(){
  stop("Wrong args")
}

#' newUUID
#' 
#' @export 
newUUID = function(){
  baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
  
  uuid = paste(
    substr(baseuuid,1,8),
    "-",
    substr(baseuuid,9,12),
    "-",
    "4",
    substr(baseuuid,13,15),
    "-",
    sample(c("8","9","a","b"),1),
    substr(baseuuid,16,18),
    "-",
    substr(baseuuid,19,30),
    sep="",
    collapse=""
  )
  return(uuid)
}

#' removeTablePrefix
#' 
#' @export 
removeTablePrefix = function(names){
  return(sapply(names, function(name){
     
    l = unlist(strsplit(name, '.', fixed=TRUE))
    if (length(l) < 2) stop(paste0('Cannot remove table prefix from name : ', name))
    return(paste0(l[2:length(l)], collapse='.'))
    
  },  USE.NAMES = FALSE))
}

 

#' addTablePrefix
#' 
#' @export 
addTablePrefix = function(prefix, names) {
  return(sapply(names, function(name){
    return(paste(prefix, name, sep='.'))
  }))
}