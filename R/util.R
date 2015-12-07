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