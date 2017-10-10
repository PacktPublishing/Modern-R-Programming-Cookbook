# Creating an XDF file from CSV input

setwd("D:/AllSync/Drive/Book-3/codeBundle/ch9")

library(RevoScaleR)
csvFiles <- dir(pattern = ".csv")

xdfFile <- file.path(getwd(), "USAirlines2016.xdf")

for(i in 1:length(csvFiles)){
  print(csvFiles[i])
  if(i!=1){
    rxImport(inData = csvFiles[i], outFile = xdfFile, overwrite = TRUE, append = "rows")
  }
  else {
    rxImport(inData = csvFiles[i], outFile = xdfFile)
  }
}

rxGetInfo(xdfFile, getVarInfo = TRUE)

# Processing data as a chunk
rxDataStep(inData = xdfFile,
           outFile = xdfFile,
           transforms = list(binDelay = ifelse(DEP_DELAY>0,1,0)),
           overwrite = TRUE)

rxGetInfo(xdfFile, numRows = 5)
rxGetInfo(xdfFile1, getVarInfo = TRUE)

# Comparing computation time with data frame and XDF

# Step-1
setwd("D:/AllSync/Drive/Book-3/codeBundle/ch9")

# Step-2
system.time(
  usairlineCSV <- read.csv("csv_USAairlines2016.csv")
)

# Step-3
system.time(
  meanDelay<- with(usairlineCSV, aggregate(DEP_DELAY, by=list(ORIGIN, DEST), FUN= "mean", na.rm=T))
)

# Step-4
system.time(
  xdfFile <- file.path(getwd(), "USAirlines2016.xdf")
)

# Step-5
system.time(
  sumstatxdf <- rxSummary(DEP_DELAY~ORIGIN:DEST, summaryStats = "Mean", data = xdfFile)
)

# Linear regression with larger data (rxFastLiner)

setwd("D:/AllSync/Drive/Book-3/codeBundle/ch9")
xdfFile <- file.path(getwd(), "USAirlines2016.xdf")
linMod <- rxFastLinear(ARR_DELAY~DEP_DELAY+ORIGIN+DEST+DAY_OF_WEEK, type = "regression", data = xdfFile)
predArrDelay <- rxPredict(linMod, data = xdfFile, extraVarsToWrite = "ARR_DELAY")
rxLinePlot(ARR_DELAY~Score, type = c("p","r"), data = predArrDelay)
