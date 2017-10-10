# Defining a new S3 class

x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17, 18,   
       11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 16, 19, 23, 15,    
       25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19, 14, 27, 22, 26)

class(x)

class(x) <- "robustSummary"
class(x)

y<- unclass(x)
class(y)

if(class(x)=="robustSummary") print("A new class has been defined")

# Defining methods for S3 class
x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17, 18,   
       11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 16, 19, 23, 15,
       25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19, 14, 27, 22, 26)

class(x) <- "robustSummary"
print(x)
print.default(x)
print.robustSummary(x)

print.robustSummary <- function(obj){
  cat("Median ", median(obj), "\n")
  cat("Median Absolute Deviation (MAD)", mad(obj), "\n")
  cat("First Quartile (Q1)", as.numeric(quantile(obj, probs = 0.25)), "\n")
  cat("Third Quartile (Q3)", as.numeric(quantile(obj, probs = 0.75)))
}

#Writing a generic function and defining method for S3 class

robSum <- function(obj) {
  UseMethod("robSum")
}

robSum.default <- function(obj){
  cat("This is a generic function for the object class 
          'robustSummary'")
}

# robustSummary
robSum.robustSummary <- function(obj){
  cat("Median ", median(obj), "\n")
  cat("Median Absolute Deviation (MAD)", mad(obj), "\n")
  cat("First Quartile (Q1)", as.numeric(quantile(obj, probs = 0.25)), "\n")
  cat("Third Quartile (Q3)", as.numeric(quantile(obj, probs = 0.75)))
}

set.seed(123) # to make the result reproducible 
newX <- rnorm(50) # generating 50 ranodm numbers from standard normal distribution
robSum.default(obj = newX)

class(newX) <- "robustSummary"
robSum.default(obj = newX)
robSum.robustSummary(obj = newX)

# Defining a new S4 class
x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17, 18, 
       11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 16, 19, 23, 15, 
       25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19, 14, 27, 22, 26)




robSum <- function(obj){
  med <- median(obj)
  mad <- mad(obj)
  q1 <- as.numeric(quantile(obj, probs = 0.25))
  q3 <- as.numeric(quantile(obj, probs = 0.75))
  return(list(median=med, mad=mad, q1= q1, q3=q3))
}

rStats <- robSum(obj=x)
rStatsS4 <- new("robustSummary", median=rStats$median, 
                mad=rStats$mad, q1=rStats$q1, q3=rStats$q3)

isS4(rStatsS4)
rStatsS4

robustSummary <- setClass("robustSummary",  
                          slots=list(median="numeric", mad="numeric",   
                                     q1="numeric", q3="numeric"))

robustSummary

# Defining methods for a S4 class
x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17, 18,   
       11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 16, 19, 23, 
       15, 25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19,
       14, 27, 22, 26)

robSum <- function(obj){
  med <- median(obj)
  mad <- mad(obj)
  q1 <- as.numeric(quantile(obj, probs = 0.25))
  q3 <- as.numeric(quantile(obj, probs = 0.75))
  return(list(median=med, mad=mad, q1= q1, q3=q3))
}

rStats <- robSum(obj=x)
rStatsS4 <- new("robustSummary", median=rStats$median,  
                mad=rStats$mad, q1=rStats$q1, q3=rStats$q3)


isS4(rStatsS4)

setMethod("show",
          "robustSummary",
          function(object) {
            cat("The median is ",object@median, " with median  
                absolute deviation (MAD) = ", object@mad, "\n")
            cat("First and Third Quartile is", object@q1, "and",  
                object@q3)
          }
        )
rStatsS4
show(rStatsS4)
rStatsS4

# Writing a function to return object of S4 class
x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17, 18, 
       11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 16, 19, 23, 15,
       25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19, 14, 27, 22,
       26)

desStat <- function(numVec, type="classical"){
  if(!is.numeric(numVec))
    stop("The input must be numeric")
  average <- mean(numVec, na.rm = T)
  std <- sd(numVec, na.rm = T)
  med <- median(numVec, na.rm = T)
  mad <- mad(numVec, na.rm = T)
  descriptiveStats <- setClass("descriptiveStats",   
                               slots=list(centre="numeric",    
                                          spread="numeric"))
  if(type=="classical")
    return(new("descriptiveStats", centre=average, spread=std))
  if(type=="robust")
    return(new("descriptiveStats", centre=med, spread=mad))
}

desStat(x)
