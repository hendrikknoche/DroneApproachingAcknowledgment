FetchData <- function() {
  #RawData <<- FetchUrl("https://docs.google.com/spreadsheets/d/1mQ2Qptjp2WcrQ3LtRUllz7ibVnEfYiu1oQRgipSTLs4/export?format=csv")
  
  #Still data
  #RawDataStill0123 <<- FetchSpreadsheet("1hOSkx6_UaHHGwcqdPi790D8hQsQkc8O5talezbtw89c")
  RawDataStill0123 <- FetchSpreadsheet2("Drone Q 0123 Still (Responses) - Form Responses 1.csv")
  # Exclude bad response
  RawDataStill0123 <- RawDataStill0123[c(-6),]
  RawDataStill0123$PID <- seq.int(nrow(RawDataStill0123))
  
  RawDataStill1230 <- FetchSpreadsheet2("Drone Q 1230 Still (Responses) - Form Responses 1.csv")
  startPID = max(RawDataStill0123$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataStill1230)
  RawDataStill1230$PID <- seq.int(from=startPID, to=endPID)
  
  RawDataStill2301 <- FetchSpreadsheet2("Drone Q 2301 Moving (Responses) - Form Responses 1.csv")
  ## Exclude bad response
  RawDataStill2301 <- RawDataStill2301[c(-10),]
  
  startPID = max(RawDataStill1230$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataStill2301)
  RawDataStill2301$PID <- seq.int(from=startPID, to=endPID)

  RawDataStill3012 <- FetchSpreadsheet2("Drone Q 3012 Still (Responses) - Form Responses 1.csv")
  ## Exclude bad response
  RawDataStill3012 <- RawDataStill3012[c(-1),]
  startPID = max(RawDataStill2301$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataStill3012)
  RawDataStill3012$PID <- seq.int(from=startPID, to=endPID)
  
  RawDataMov0123 <- FetchSpreadsheet2("Drone Q 0123 Moving (Responses) - Form Responses 1.csv")
  ## Exclude bad response
  RawDataMov0123 <- RawDataMov0123[c(-13),]
  #RawDataMov0123 <- RawDataMov0123[sample(nrow(RawDataMov0123), 13),]
  
  startPID = max(RawDataStill3012$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataMov0123)
  RawDataMov0123$PID <- seq.int(from=startPID, to=endPID)
  
  RawDataMov1230 <- FetchSpreadsheet2("Drone Q 1230 Moving (Responses) - Form Responses 1.csv")
  ## Exclude bad response
  RawDataMov1230 <- RawDataMov1230[c(-4),]

  startPID = max(RawDataMov0123$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataMov1230)
  RawDataMov1230$PID <- seq.int(from=startPID, to=endPID)

  RawDataMov2301 <- FetchSpreadsheet2("Drone Q 2301 Moving (Responses) - Form Responses 1.csv")
  RawDataMov2301[13,2] = "Male"
  startPID = max(RawDataMov1230$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataMov2301)
  RawDataMov2301$PID <- seq.int(from=startPID, to=endPID)
  
  RawDataMov3012 <- FetchSpreadsheet2("Drone Q 3012 Moving (Responses) - Form Responses 1.csv")
  ## Fix attack helicopter response
  RawDataMov3012[6,2] = "Male"
  ## Exclude bad response
  RawDataMov3012 <- RawDataMov3012[c(-2),]
  startPID = max(RawDataMov2301$PID, na.rm=TRUE)+1
  endPID = startPID-1 + nrow(RawDataMov3012)
  RawDataMov3012$PID <- seq.int(from=startPID, to=endPID)
  
  RawData <<- Reduce(function(x,y) merge(x,y, all=TRUE), list(RawDataStill0123, RawDataStill1230, RawDataStill2301, RawDataStill3012, RawDataMov0123, RawDataMov1230, RawDataMov2301, RawDataMov3012))
  
  CleanedData1 <- CleanDataFrame(RawDataStill0123, "NoPan","0123", "Rotation", "Nod", "Toss", "Waggle")
  CleanedData2 <- CleanDataFrame(RawDataStill1230, "NoPan","1230", "Nod", "Toss", "Waggle", "Rotation")
  CleanedData3 <- CleanDataFrame(RawDataStill2301, "NoPan","2301", "Toss", "Waggle", "Rotation", "Nod")
  CleanedData4 <- CleanDataFrame(RawDataStill3012, "NoPan","3012", "Waggle", "Rotation", "Nod", "Toss")

  CleanedData5 <- CleanDataFrame(RawDataMov0123, "Pan","0123", "Rotation", "Nod", "Toss", "Waggle")
  CleanedData6 <- CleanDataFrame(RawDataMov1230, "Pan","1230", "Nod", "Toss", "Waggle", "Rotation")
  CleanedData7 <- CleanDataFrame(RawDataMov2301, "Pan","2301", "Toss", "Waggle", "Rotation", "Nod")
  CleanedData8 <- CleanDataFrame(RawDataMov3012, "Pan","3012", "Waggle", "Rotation", "Nod", "Toss")
  
  CleanedData <<- Reduce(function(x,y) merge(x,y, all=TRUE), 
                         list(CleanedData1, CleanedData2, CleanedData3, CleanedData4, 
                              CleanedData5, CleanedData6, CleanedData7, CleanedData8))
  
  headers = c("NoPan", "Pan");
  rNames = c("0123","1230", "2301", "3012")
  cPan = c(nrow(RawDataMov0123), nrow(RawDataMov1230), nrow(RawDataMov2301), nrow(RawDataMov3012))
  cNoPan = c(nrow(RawDataStill0123), nrow(RawDataStill1230), nrow(RawDataStill2301), nrow(RawDataStill3012))
  responses <<- data.frame(cNoPan, cPan, row.names = rNames);
  colnames(responses) <- headers;
  }

FetchSpreadsheet <- function(id){
  require(RCurl)
  ssUrl <- paste("https://docs.google.com/spreadsheets/d/",id,"/export?format=csv", sep = "")
  return (read.csv(textConnection(getURL(ssUrl,.opts=list(ssl.verifypeer=FALSE)))))
}

FetchSpreadsheet2 <- function(id){
  #require(RCurl)
  fileName <- paste("combined/data/", id, sep = "")
  return (read.csv(fileName))
}

CleanDataFrame <- function(dataFrame, cameraPan, motionOrder, motion1, motion2, motion3, motion4){
  headers = c("PID", "Gender", "Age","Pan", "Encounter","Order", "Motion","What did the drone do","What means to you?","Any additional comments?",  "Degree")
  order <- rep(motionOrder, nrow(dataFrame))
  cPan <- rep(cameraPan, nrow(dataFrame))
  
  motion <- rep(motion1, nrow(dataFrame))
  first <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan, 1, order, motion, dataFrame[,6], dataFrame[,7], dataFrame[,9],dataFrame[,8])
  colnames(first) <- headers
  
  motion <- rep(motion2, nrow(dataFrame))
  second <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan, 2, order,motion, dataFrame[,10], dataFrame[,11], dataFrame[,13],dataFrame[,12])
  colnames(second) <- headers
  
  motion <- rep(motion3, nrow(dataFrame))
  third <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan,  3, order,motion, dataFrame[,14], dataFrame[,15], dataFrame[,17],dataFrame[,16])
  colnames(third) <- headers
  
  motion <- rep(motion4, nrow(dataFrame))
  fourth <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan,  4, order,motion, dataFrame[,18], dataFrame[,19], dataFrame[,21],dataFrame[,20])
  colnames(fourth) <- headers
  
  return(Reduce(function(x,y) merge(x,y, all=TRUE), list(first, second, third, fourth)))
  rm(order,motion,first,second, third, fourth)
  
}

CleanDataFrame2 <- function(dataFrame, cameraPan, motionOrder, motion1, motion2, motion3, motion4){
  headers = c("PID", "Gender", "Age","Pan", "Encounter","Order", "Motion", "Degree")
  order <- rep(motionOrder, nrow(dataFrame))
  cPan <- rep(cameraPan, nrow(dataFrame))
  
  motion <- rep(motion1, nrow(dataFrame))
  first <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan, 1, order,motion,dataFrame[,8])
  colnames(first) <- headers
  
  motion <- rep(motion2, nrow(dataFrame))
  second <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan, 2, order,motion,dataFrame[,12])
  colnames(second) <- headers
  
  motion <- rep(motion3, nrow(dataFrame))
  third <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan,  3, order,motion,dataFrame[,16])
  colnames(third) <- headers
  
  motion <- rep(motion4, nrow(dataFrame))
  fourth <- data.frame(dataFrame[,22], dataFrame[,2],dataFrame[,3],cPan,  4, order,motion,dataFrame[,20])
  colnames(fourth) <- headers
  
  return(Reduce(function(x,y) merge(x,y, all=TRUE), list(first, second, third, fourth)))
  rm(order,motion,first,second, third, fourth)
  
}
