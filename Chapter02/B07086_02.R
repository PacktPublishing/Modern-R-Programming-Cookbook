# Creating a vector and accessing its properties

cVec <- c("Cricket", "Football", "Basketball", "Rugby")
cVec <- c(game1="Cricket", game2="Football",
          game3="Basketball", game4="Rugby")
nVec <- c(1:10)
Lvec <- c(TRUE, FALSE, FALSE, TRUE)
nVec2 <- runif( n=5, min=10, max=20)
cVec2 <- sample(x=letters, size= 5, replace = F)
Lvec2 <- nVec2>=13

is.vector(cVec)
is.vector(nVec)
is.vector(Lvec)
is.vector(cVec2)
is.vector(nVec2)
is.vector(Lvec2)

cVec[3]
cVec[c(1,3,5)]

# Creating a matrix and accessing its properties

matA <- matrix(1, nrow=2, ncol=2)       
matB <- matrix(1:9, nrow=3, ncol=3, byrow=TRUE) 
Vec1 <- 1:3        
Vec2 <- 3:1
matC <- rbind(Vec1, Vec2)
matD <- cbind(Vec1, Vec2)
matE <- array(1:9, dim=c(3,3))

is.matrix(matA)
nrow(matA)        
ncol(matA)
matA[1, 2]
matA[2, ] 
matADD <- matB + matE
matMult <- matA %*% matC
matMult2 <- matB * matE #element-wise multiplication    
matMult2 <- matB %*% matE #Matrix multiplication
rownames(matA) <- c("row1", "row2")    
colnames(matA) <- c("col1", "col2")

# Creating a data frame and accessing its properties

datA <- data.frame(ID = 1:5,
                   hourSpetOnInternet = c(5,3,4,1,2),
                   GENDER = c("M", "F", "F", "M", "F"))

str(datA)       


nrow(datA) # to know number of rows in the data frame        
ncol(datA) # to know number of columns in the data frame        
head(datA, n=2)  # print first 2 rows of the data frame        
tail(datA, n=2) # print last 2 rows of the data frame        
datA$ID # to get access to ID variable only        datA[["ID"]] # to get access to ID variable only        names(datA) # to get column names of the data frame        
colnames(datA) # to get column names of the data frame
datA[,1] # this is similar to extracting elements from a matrix


datB <- data.frame(ID = 1:5, hourSpetOnInternet = c(5,3,4,1,2),   
                   GENDER = c("M", "F", "F", "M", "F"), stringsAsFactors=FALSE)
                   
                   
# Creating an array and accessing its properties
                   
arrayA <- array(1:16, dim=c(2,2,4))
arrayA

arrayA2 <- arrayA[, , 2]
arrayA[1,1,2]
                   
                   
column.names <- c("COL1","COL2")    
row.names <- c("ROW1","ROW2")    
matrix.names <- c("m1","m2", "m3", "m4")    
arrayB <- array(1:16, dim=c(2,2,4), 
                dimnames =list(row.names,
                               column.names, 
                               matrix.names))
dim(arrayA) <- c(2,4,2)
                   
                   
# Creating a list from a combination of vector,matrix, and data frame
                   
cVec <- c("Cricket", "Football", "Basketball", "Rugby")    
nVec <- c(1:10)    
Lvec <- c(TRUE, FALSE, FALSE, TRUE)    
matA <- matrix(1, nrow=2, ncol=2)    
datA <- data.frame(ID = 1:5, 
                   hourSpetOnInternet = c(5,3,4,1,2),
                   GENDER = c("M", "F", "F", "M", "F"))    
                   arrayA <- array(1:16, dim=c(2,2,4))
                   
listA <- list(cVec, nVec, Lvec, matA, datA, arrayA)
listB <- list(vector1 = cVec, vector2 = nVec, vector3 = Lvec,       
matrix1 = matA, data1 = datA, array1 = arrayA)
str(listA)        
str(listB)
listA[[1]]
listB$data1    

                                 
# Converting matrix to data frame and data frame to matrix
                                 
M1 <- matrix(1:9, nrow=3, ncol= 3, byrow=TRUE)    
D1 <- data.frame(x1= c(1,3,2,4,5), x2= c("Cricket", "Football",    "Basketball", "Rugby", "Baseball" ))
                                 
M1ToData <- as.data.frame(M1)
str(M1ToData)

D1ToMatrix <- as.matrix(D1)        
str(D1ToMatrix)         
colnames(D1ToMatrix)        
