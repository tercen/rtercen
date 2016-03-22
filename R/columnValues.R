 
#' ColumnValues
#' 
#' @export 
ColumnValues <- R6Class(
  "ColumnValues",
  private = list(
    type = NULL,
    subtype = NULL,
    data = NULL,
    dictionary = NULL,
    fromType = function( aType, aSubtype, aData, dictionary = NULL) {
      if (is.null(aType)) stop("ColumnValues : type is required.")
      if (is.null(aData)) stop("ColumnValues : data is required.")
      private$type <- aType
      private$subtype <- aSubtype
      if (private$type == "factor") {
        if (is.factor(aData)){
          private$data <- as.integer(aData) - 1
          private$dictionary <- levels(aData)
        } else {
          if (is.null(dictionary)) stop("dictionary is null")
          private$data <- as.integer(aData)
          private$dictionary <- as.character(dictionary)
        }
      } else if (private$type == "num"){
        if (is.null(private$subtype)) stop("ColumnValues : subtype is required.")
        if (private$subtype == "float32"){
          private$data = as.double(as.vector(aData))
        } else if (private$subtype == "uint32" || private$subtype == "int32" private$subtype == "uint16" || private$subtype == "uint8") {
          private$data = as.integer(as.vector(aData))
        } else {
          stop(paste("ColumnValues : unknown subtype : ", private$subtype))
        }
      } else if (private$type == "string") {
        private$data <- as.vector(aData)
      } else {
        stop(paste("ColumnValues : unknown type : ", private$type))
      }
    },
    fromVector = function(list){
      vect = as.vector(list)
      type = NULL
      subtype = NULL
      dictionary = NULL
      if (is.double(vect) || is.integer(vect)){
        type = "num"
        if (is.integer(vect)){
          subtype = "int32"
        } else {
          subtype = "float32"
        }
      } else if (is.character(vect)) {
        type = "string"
      } else if (is.factor(vect) ) {
        type = "factor"
      } else {
        stop(paste0("ColumnValues : unknwon vector type."))
      }
      private$fromType(type,subtype,vect)
    },
    fromJson = function(json){
      private$fromType(json$type, json$subtype, unlist(json$data), dictionary=json$dictionary)
    }
  ),
  public = list(
    initialize = function(type=NULL,subType=NULL,data=NULL,vector=NULL,json=NULL) {
      if (!is.null(json)) {
        private$fromJson(json)
      } else if (!is.null(vector)) {
        private$fromVector(vector)
      } else {
        private$fromType(type,subType,data)
      }
    },
    getType = function() private$type,
    getSubType = function() private$subtype,
    setSubType = function(subtype){
      private$subtype = subtype
    },
    getData = function() private$data,
    getDictionary = function() private$dictionary,
    toTson = function() {
      tsonData = NULL
      if (private$type == "factor"){
        len = length(private$dictionary)
        if (len < 256){
          tsonData = tson.uint8.vec(private$data)
        } else if (len < 65536){
          tsonData = tson.uint16.vec(private$data)
        } else {
          tsonData = tson.uint32.vec(private$data)
        }
        return (list(type=tson.character(private$type),
                     subtype=tson.character(private$subtype),
                     data=tsonData,
                     dictionary=private$dictionary))
      } else  if (is.null(private$subtype)){
        tsonData = private$data
      } else if (private$subtype == "float32"){
        tsonData = tson.float32.vec(private$data)
      } else if (private$subtype == "uint32"){
        tsonData = tson.uint32.vec(private$data)
      } else {
        tsonData = private$data
      }
      return (list(type=tson.character(private$type), subtype=tson.character(private$subtype), data=tsonData))
    },
    length = function() length(private$data)
  )
)
