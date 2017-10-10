# Writing your first function in R
V1 <- c(3, 7, 2, 6, 9)

fsdByMean <- function(vectorInput){          
  average <- mean(vectorInput)          
  std <- sd(vectorInput)          
  stdByMean <- std/average          
  return(stdByMean)
}

fsdByMean(vectorInput = V1)        

fsdByMean <- function(vectorInput){
  average <- mean(vectorInput)
  std <- sd(vectorInput)     
  stdByMean <- std/average     
  return(average)
}
  
fsdByMean(vectorInput = V1)    

  
# Writing functions with multiple arguments and use of default values
  
  
V1 <- c(3, 7, 2, 6, 9)

fDescriptive <- function(numVec, type = "classical"){      
    avg <- mean(numVec)      
    std <- sd(numVec)      
    med <- median(numVec)      
    medad <- mad(numVec)      
    out1 <- c(mean = avg, sd = std)      
    out2 <-c(median = med, mad = medad)      
    if(type== "classical")        
     return(out1)      
    else if(type == "robust")       
     return(out2)    
}

fDescriptive(numVec = V1)       

fDescriptive(numVec = V1, type = "classical")    
fDescriptive(numVec = V1, type = "robust")
               
# Handling data types in input argument  
               
fDescriptive <- function(numVec, type = "classical"){
  type <- tolower(type)      
  if(is.numeric(numVec) & is.vector(numVec)){
    avg <- mean(numVec)        
    std <- sd(numVec)        
    med <- median(numVec)        
    medad <- mad(numVec)       
    out1 <- c(mean = avg, sd = std)       
    out2 <-c(median = med, mad = medad)        
    if(type== "classical")
      return(out1) 
    else if (type == "robust")          
      return(out2)
  }
}
               
               
# Output types and return values
               
V1 <- c(3, 7, 2, 6, 9)
               
meanSe <- function(numVec){  
  if(is.numeric(numVec) & is.vector(numVec)){      
    avg <- mean(numVec)        
    std <- sd(numVec)        
    se <- std/sqrt(length(numVec))        
    out <- paste("Mean of the input vector is = ",               
                 paste(avg), " with a standard error = ",               
                 paste(se), sep = "")       
    return(out)
  }
}

# Recursive call to a function
               
fDescriptive <- function(numVec, type = "classical"){
  avg <- mean(numVec)               
  std <- sd(numVec)               
  med <- median(numVec)             
  medad <- mad(numVec)              
  out1 <- c(mean = avg, sd = std)              
  out2 <-c(median = med, mad = medad)               
  if(type== "classical")                 
    return(out1)               
  else if (type == "robust")                 
    return(out2)             
}

robustSummary <- apply(X = iris[,-5], MARGIN = 2, FUN = fDescriptive, type = "robust")

cubicSum <- function(n){ 
  if(n==0)                   
    return(0) 
  else                  
    return(n^3 + cubicSum(n-1))             
}

cubicSum(n = 3)

               
# Handling exceptions and error messages
V1 <- c(3, 7, -2, 6, 9, 0, -4)
               
logX <- function(numVec1){ 
  if(any(numVec1<0))            
    stop("A negative value exists")          
  else logV1 <- log(numVec1)         
   return(logV1)
}
               
logX1 <- function(numVec1){
  try(logV1 <- log(numVec1))          
  return(logV1)  
}
               
logX2 <- function(numVec1){ 
  tryCatch( 
    return(logV1 <- log(numVec1)), 
     warning = function(w) print("A negative input occurred")) 
}

               