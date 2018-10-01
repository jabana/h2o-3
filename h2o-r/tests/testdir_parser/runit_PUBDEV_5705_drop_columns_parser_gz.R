setwd(normalizePath(dirname(
  R.utils::commandArgs(asValues = TRUE)$"f"
)))
source("../../scripts/h2o-r-test-setup.R")

# Tests parsing with skipped columns
test.parseSkippedColumnsgz<- function() {
  f1 <-
    h2o.importFile(locate("smalldata/airlines/year2005.csv"))
  fileName <- locate("smalldata/airlines/year2005.csv.gz")

  fullFrameR <- as.data.frame(f1) # takes too long
  skip_front <- c(1)
  skip_end <- c(h2o.ncol(f1))
  set.seed <- 12345
  onePermute <- sample(h2o.ncol(f1))
  skipall <- onePermute
  skip10Per <- onePermute[1:floor(h2o.ncol(f1) * 0.1)]
  skip20Per <- onePermute[1:floor(h2o.ncol(f1) * 0.2)]
  skip90Per <- onePermute[1:floor(h2o.ncol(f1) * 0.9)]
  
  # test skipall for h2o.importFile
  e <-
    tryCatch(
      assertCorrectSkipColumns(fileName, fullFrameR, skipall, TRUE, h2o.getTypes(f1)),
      error = function(x)
        x
    )
  print(e)
  # test skipall for h2o.uploadFile
  e2 <-
    tryCatch(
      assertCorrectSkipColumns(fileName, fullFrameR, skipall, FALSE, h2o.getTypes(f1)),
      error = function(x)
        x
    )
  print(e2)
  
  # # skip 10% of the columns randomly
  # print("Testing skipping 10% of columns")
  # assertCorrectSkipColumns(fileName, fullFrameR, skip10Per, TRUE, h2o.getTypes(f1)) # test importFile
  # assertCorrectSkipColumns(fileName, fullFrameR, skip10Per, FALSE, h2o.getTypes(f1)) # test uploadFile
  # 
  # skip 20% of the columns randomly
  #print("Testing skipping 20% of columns")
  #assertCorrectSkipColumns(fileName, fullFrameR, skip20Per, TRUE, h2o.getTypes(f1)) # test importFile
  #assertCorrectSkipColumns(fileName, fullFrameR, skip20Per, FALSE, h2o.getTypes(f1)) # test uploadFile
  
  # skip 90% of the columns randomly
  print("Testing skipping 90% of columns")
  assertCorrectSkipColumns(fileName, fullFrameR, skip90Per, TRUE, h2o.getTypes(f1)) # test importFile
  assertCorrectSkipColumns(fileName, fullFrameR, skip90Per, FALSE, h2o.getTypes(f1)) # test uploadFile
}

doTest("Test Orc Parse with skipped columns", test.parseSkippedColumnsgz)