library(rtercen)
library(rtson)
  
context("Table test")
  
# Utility function 

test_vector_column_equals <- function(vector,column){
  values = column$getValues()
  data = values$getData()
  expect_equal(length(data), length(vector))
  type = NULL
  subtype = NULL
  if (is.character(vector)){
    type = "string"
  } else if (is.integer(vector)){
    type = "num"
    subtype = "int32"
  } else if (is.double(vector)){
    type = "num"
    subtype = "float32"
  }
  expect_equal(values$getType(), type)
  if (!is.null(subtype)){
    expect_equal(values$getSubType(), subtype)
  }
   
  for (i in seq(1:length(vector))){
    if (is.numeric(data[[i]])){
      expect_less_than(abs(data[[i]] - vector[[i]]), 1e-7)
    } else {
      expect_equal(data[[i]], vector[[i]] )
    }
  }
}
 
test_dataframe_table_equals <- function(df,table){
#   print("eee")
  expect_equal(table$getNRows(), dim(df)[[1]], label="nRows")
  columns = table$getColumns()
  expect_equal(length(columns), dim(df)[[2]], label="nCols")
  
  for (i in seq(1,length(table$getColumns()))){
    column = columns[[i]]
    vector = as.vector(df[[column$getName()]])
    test_vector_column_equals(vector, column)
  }
}

# -----------------------------------------  TESTS

test_that("Table from data.frame", {
  df = data.frame(c1=c(1.2,2.0), c2= as.integer(c(1,2)), c3=c("v1","v2"))
  table = Table$new(df=df)
  test_dataframe_table_equals(df, table)
})

test_that("Table as.data.frame", {
  df = data.frame(c1=c(1.2,2.0), c2= as.integer(c(1,2)), c3=c("v1","v2"))
  table = Table$new(df=df)
  expect_equal(table$as.data.frame(), df)
})

test_that("Table from raw", {
  df = data.frame(c1=c(1.2,2.0), c2= as.integer(c(1,2)), c3=c("v1","v2"))
  table = Table$new(df=df)
  bytes = toTSON(table$toTson())
  json = fromTSON(bytes)
  table = Table$new(json=json)
  test_dataframe_table_equals(df, table)
})



