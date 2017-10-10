# The use of the if conditional statement

x <- c(13, 21, 19, NA, 21, 16, 21, NA, 17, 18)   
y<- c("Football", "Cricket", NA, "Rugby")

if(any(is.na(x))){      
  cat("There is missing values in x at \n")     
  which(is.na(x))    
}


if(any(is.na(y))){
  cat("There is missing values in y at \n")      
  which(is.na(y))   
}


if(is.na(x)){     
  cat("There is missing values in x at \n")    
  which(is.na(x))   
}

# The use of if.else conditional operator

a <- 9        

if(a %% 2==0){         
    print("This is an even number")        
  } else {          
      print("This is an odd number")        
}  
  

if(a %% 2==0){         
  print("This is an even number")        
} 
else {          
  print("This is an odd number")        
}  


# A first use of a conditional statement if.else
  
  x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17, 18,    
         11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 16, 19, 23, 15,    
         25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19, 14, 27,    
         22, 26)
  
  

  medX <- median(x)        
  medX # to see the value of median in R console       

  
  newX <- ifelse(x>medX,1,0)        
  newX  # Print the value of new variable into R console       

  

    
# Writing a function using the switch operator

sym <- 1       
switch(sym, 
       function(x) c(mean=mean(x), std=sd(x)),
       function(x) c(med=median(x), mad=mad(x)))
                 
Fun1 <- switch(sym, 
               function(x) c(mean=mean(x), std=sd(x)),
               function(x) c(med=median(x), mad=mad(x)))       

x <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18, 29, 17,
       18, 11, 13, 20, 25, 18, 15, 19, 21, 21, 7, 12, 23, 31, 
       16, 19, 23, 15, 25, 19, 15, 25, 25, 16, 29, 15, 26, 29, 
       23, 24, 20, 19, 14, 27, 22, 26) 

Fun1(x)
                 
sym <- 2       
Fun2 <- switch(sym, function(x) c(mean=mean(x), std=sd(x)),
               function(x) c(med=median(x), mad=mad(x)))        

Fun2(x)
                      
# Comparing performance of switch and series of if.else if.else statement

option1 <- function(x, sym){          
  switch(sym, classical = c(mean= mean(x, na.rm = T), std = sd(x, na.rm = T)), 
         robust = c(med = median(x, na.rm = T), mad = mad(x, na.rm = T)))        
}
                      
                      
                      
option2 <- function(x, sym){          
  if(sym=="classical"){ 
    out <- c(mean = mean(x,na.rm = T), std= sd(x,na.rm = T))           
    return(out)         
  }          
  else if(sym=="robust"){            
    out <- c(med = median(x, na.rm = T), mad = mad(x, na.rm =  T))           
    return(out)          
    }        
   else return(NULL)        
  }
                      
                      
inputVec <- c(13, 21, 19, 18, 21, 16, 21, 24, 17, 18, 12, 18,        
              29, 17, 18, 11, 13, 20, 25, 18, 15, 19, 21, 21, 
              7, 12, 23, 31,        16, 19, 23, 15, 25, 19, 15, 
              25, 25, 16, 29, 15, 26, 29, 23, 24, 20, 19, 14, 27, 22, 26)

library(microbenchmark)
microbenchmark(option1(x,"classical"), option2(x,"classical"),times = 100000)       
microbenchmark(option1(x,"robust"), option2(x,"robust"),times = 100000)       


# Using "for" loop for iterations
set.seed(1234)    
mat<-matrix(sample(c(0,1),50,replace = T),nrow = 10,ncol=5)    
rownames(mat) <- paste("patient", paste(1:10),sep="")    
colnames(mat) <- c("diabetes", "hypertension", "asthma","jointPain", "fever")

out<-matrix(NA, nrow = ncol(mat), ncol = ncol(mat))      
for(i in 1:ncol(mat)){
  for(j in 1:ncol(mat)){          
    colI <- mat[,i]          
    colJ <- mat[,j]          
    out[i,j] <- t(colI) %*% colJ        
  }      
}

colI <- mat[,i]    
colJ <- mat[,j]
                                  
# Vectorised operation versus for loop
                                  
                                  
bigBinaryData <- read.csv("loopVectorization.csv")    
binMat <- as.matrix(bigBinaryData) # to convert data frame into a    matrix
loopOperation <- function(mat){          
  out<-matrix(NA, nrow = ncol(mat), ncol = ncol(mat))          
  for(i in 1:ncol(mat)){            
    for(j in 1:ncol(mat)){              
      colI <- mat[,i]              
      colJ <- mat[,j]             
      out[i,j] <- t(colI) %*% colJ            
    }          
  }         
 return(out)        
}
                                  
vectorizedOp <- function(mat){          
  return(t(mat) %*% mat)        
}

library(microbenchmark) # To compare performance of the functions
microbenchmark(loopOperation(binMat), vectorizedOp(binMat))       
