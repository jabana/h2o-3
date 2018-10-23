setwd(normalizePath(dirname(
  R.utils::commandArgs(asValues = TRUE)$"f"
)))
source("../../scripts/h2o-r-test-setup.R")

# Tests parsing with skipped columns
test.parseSkippedColumnsgz<- function() {
  f1 <-
    h2o.importFile(locate("smalldata/synthetic_perfect_separation"))
  fileName <- locate("smalldata/synthetic_perfect_separation")

  fullFrameR <- as.data.frame(f1) # takes too long
  skip_front <- c(1)
  skip_end <- c(h2o.ncol(f1))
  set.seed <- 12345
  onePermute <- sample(h2o.ncol(f1))
  skipall <- onePermute
  skip50Per <- onePermute[1:floor(h2o.ncol(f1) * 0.5)]
  
  # test skipall for h2o.importFile
  e <-
    tryCatch(
      assertCorrectSkipColumns(fileName, fullFrameR, skipall, TRUE, h2o.getTypes(f1)),
      error = function(x)
        x
    )
  print(e)

    # skip 50% of the columns randomly
  print("Testing skipping 50% of columns")
  assertCorrectSkipColumns(fileName, fullFrameR, skip50Per, TRUE, h2o.getTypes(f1)) # test importFile
 }

doTest("Test Orc Parse with skipped columns", test.parseSkippedColumnsgz)