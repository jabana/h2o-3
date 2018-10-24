setwd(normalizePath(dirname(
  R.utils::commandArgs(asValues = TRUE)$"f"
)))
source("../../scripts/h2o-r-test-setup.R")

# Tests parsing with skipped columns
test.parseSkippedColumnsNameType <- function() {
  browser()
  csvWithHeader <-
    h2o.importFile(locate("smalldata/airlines/allyears2k_headers.csv"))
  allColnames <- h2o.names(csvWithHeader)
  allTypeDict <- h2o.getTypes(csvWithHeader)
  n <- which(allTypeDict=='int')
  allTypeDict[n] <- "Numeric"
  n<-which(allTypeDict=='enum')
  allTypeDict[n] <- "Enum"
  pathHeader <- locate("smalldata/airlines/allyears2k_headers.csv")
  
  # upload file with  no header but fixed column types.
  csvWithNoHeader <-
    h2o.uploadFile(locate("smalldata/airlines/allyears2k.csv"))
  allNewColnames <- h2o.names(csvWithNoHeader)
  allNewTypeDict <- h2o.getTypes(csvWithNoHeader)
  n <- which(allNewTypeDict=='int')
  allNewTypeDict[n] <- "Numeric"
  n <- which(allNewTypeDict=='enum')
  allNewTypeDict[n] <- "Enum"
  pathNoHeader <- locate("smalldata/airlines/allyears2k.csv")
  
  browser()
  skip_front <- c(1)
  skip_end <- c(h2o.ncol(csvWithHeader))
  set.seed <- 12345
  onePermute <- sample(h2o.ncol(csvWithHeader))
  skipall <- onePermute
  skip50Per <- onePermute[1:floor(h2o.ncol(csvWithHeader) * 0.5)]
  skip50Per <- c(1,2)

  # skip 50% of the columns randomly
  print("Testing skipping 50% of columns")
  assertCorrectSkipColumns(csvWithHeader, pathNoHeader, skip50Per, allColnames, allTypeDict,0) 
  assertCorrectSkipColumns(csvWithHeader, pathNoHeader, skip50Per, allColnames, allTypeDict,1) 
  assertCorrectSkipColumns(csvWithHeader, pathNoHeader, skip50Per, allColnames, allTypeDict,2) 
  assertCorrectSkipColumns(csvWithNoHeader, pathHeader, skip50Per, allNewColnames, allNewTypeDict,0) 
  assertCorrectSkipColumns(csvWithNoHeader, pathHeader, skip50Per, allNewColnames, allNewTypeDict,1) 
  assertCorrectSkipColumns(csvWithNoHeader, pathHeader, skip50Per, allNewColnames, allNewTypeDict,2) 
}

assertCorrectSkipColumns <-
  function(origialFile,
           parsePath,
           skippedColumns,
           all_column_names,
           all_column_types,
           mode) {
    colnames <- c()
    coltype <- c()
    colidx <- c()
  
    for (cind in c(1:length(all_column_names)))  {
      if (!(cind %in% skippedColumns)) {
        colnames <- c(colnames, all_column_names[cind])
        coltype <- c(coltype, all_column_types[cind])
        colidx <- c(colidx, cind)
      }
    }
    if (mode == 0) # use by by.col.names
      coltypes <- list(by.col.name=colnames, types=coltype)
    else if (mode==1) # use by.col.idx
      coltypes <- list(by.col.idx=colidx, types=coltype)
    else
      coltypes <- coltype
    
    browser()
    if (mode == 0)  {
      # use both name and type
      f1 <<-
        h2o.importFile(parsePath, col.names = colnames, col.types = coltypes, skipped_columns=skippedColumns)
      f2 <<-
        h2o.uploadFile(parsePath, col.names = colnames, col.types = coltypes, skipped_columns=skippedColumns)
    } else if (mode == 1) {
      f1 <<- h2o.importFile(parsePath, col.names = colnames, skipped_columns=skippedColumns)
      f2 <<- h2o.uploadFile(parsePath, col.names = colnames, skipped_columns=skippedColumns)
    } else {
      f1 <<- h2o.importFile(parsePath, col.types = coltypes, skipped_columns=skippedColumns)
      f2 <<- h2o.uploadFile(parsePath, col.types = coltypes, skipped_columns=skippedColumns)
    }
    
    expect_true(h2o.nrow(originalFile) == h2o.nrow(f1))
    expect_true(h2o.nrow(f2) == h2o.nrow(f1))
    cfullnames <- names(originalFile)
    f2R <- as.data.frame(originalFile)
    f1R <- as.data.frame(f1)
    f2RR <- as.data.frame(f2)
    cskipnames <- names(f2R)
    skipcount <- 1
    rowNum <- h2o.nrow(f1)
    for (ind in c(1:length(cfullnames))) {
      if (cfullnames[ind] == cskipnames[skipcount]) {
        if (allFrameTypes[ind] == "uuid")
          continue
        for (rind in c(1:rowNum)) {
          if (is.na(f1R[rind, ind])) {
            expect_true(
              is.na(f2RR[rind, skipcount]),
              info = paste0(
                "expected NA but received: ",
                f2RR[rind, skipcount],
                " in row: ",
                rind,
                " with column name: ",
                cfullnames[ind],
                " and skipped column name ",
                cskipnames[skipcount],
                sep = " "
              )
            )
            expect_true(
              is.na(f2R[rind, skipcount]),
              info = paste0(
                "expected NA but received: ",
                f2R[rind, skipcount],
                " in row: ",
                rind,
                " with column name: ",
                cfullnames[ind],
                " and skipped column name ",
                cskipnames[skipcount],
                sep = " "
              )
            )
          } else if (is.numeric(f1R[rind, ind])) {
            if (allFrameTypes[ind] == 'time') {
              expect_true(
                abs(f1R[rind, ind] - f2R[rind, skipcount]) < 10,
                info = paste0(
                  "expected: ",
                  f1R[rind, ind],
                  " but received: ",
                  f2R[rind, skipcount],
                  " in row: ",
                  rind,
                  " with column name: ",
                  cfullnames[ind],
                  " and skipped column name ",
                  cskipnames[skipcount],
                  sep = " "
                )
              )
              expect_true(
                abs(f1R[rind, ind] - f2RR[rind, skipcount]) < 10,
                info = paste0(
                  "expected: ",
                  f1R[rind, ind],
                  " but received: ",
                  f2RR[rind, skipcount],
                  " in row: ",
                  rind,
                  " with column name: ",
                  cfullnames[ind],
                  " and skipped column name ",
                  cskipnames[skipcount],
                  sep = " "
                )
              )
            } else {
              expect_true(
                abs(f1R[rind, ind] - f2R[rind, skipcount]) < 1e-10,
                info = paste0(
                  "expected: ",
                  f1R[rind, ind],
                  " but received: ",
                  f2R[rind, skipcount],
                  " in row: ",
                  rind,
                  " with column name: ",
                  cfullnames[ind],
                  " and skipped column name ",
                  cskipnames[skipcount],
                  sep = " "
                )
              )
              expect_true(
                abs(f1R[rind, ind] - f2RR[rind, skipcount]) < 1e-10,
                info = paste0(
                  "expected: ",
                  f1R[rind, ind],
                  " but received: ",
                  f2RR[rind, skipcount],
                  " in row: ",
                  rind,
                  " with column name: ",
                  cfullnames[ind],
                  " and skipped column name ",
                  cskipnames[skipcount],
                  sep = " "
                )
              )
              
            }
          } else {
            expect_true(
              f1R[rind, ind] == f2R[rind, skipcount],
              info = paste0(
                "expected: ",
                f1R[rind, ind],
                " but received: ",
                f2R[rind, skipcount],
                " in row: ",
                rind,
                " with column name: ",
                cfullnames[ind],
                " and skipped column name ",
                cskipnames[skipcount],
                sep = " "
              )
            )
            expect_true(
              f1R[rind, ind] == f2RR[rind, skipcount],
              info = paste0(
                "expected: ",
                f1R[rind, ind],
                " but received: ",
                f2RR[rind, skipcount],
                " in row: ",
                rind,
                " with column name: ",
                cfullnames[ind],
                " and skipped column name ",
                cskipnames[skipcount],
                sep = " "
              )
            )
            
          }
        }
        skipcount <- skipcount + 1
        if (skipcount > h2o.ncol(f2R))
          break
      }
    }
    print("Test completed!")
  }

doTest("Test Orc Parse with skipped columns", test.parseSkippedColumnsNameType)