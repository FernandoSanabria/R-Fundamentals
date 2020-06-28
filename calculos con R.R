4+4
20/4
900/23
9^9

x <- 86
x

TiempoBugs <-18
TiempoImplementando <- 6
TiempoBugs + TiempoImplementando

str(mtcars)

class(mtcars$vs)

mtcars$vs = as.logical(mtcars$vs)

mtcars$am = as.logical(mtcars$am)

str(dataset)
summary(orangeec)
summary(mtcars)

wt <- (mtcars$wt*1000)/2
wt

mtcars.new <- transform(mtcars,wt=wt*1000/2)
mtcars.new



tiempoTareasJob <- c(6,7,6,9,2,1)
tiempoTareasLearning <- c(4,7,2,5,9,10)

tiempoVida <- tiempoTareasJob + tiempoTareasLearning
tiempoVida

diasSemana <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
diasSemana


diasMas20Min <- c(TRUE,FALSE,FALSE,TRUE,TRUE)
totalTiempoTareasJob = sum(tiempoTareasJob)
totalTiempoTareasLearning = sum(tiempoTareasLearning)
totalTiempoVida = sum(tiempoVida)

# matrix creation
tiempoMatrix = matrix(c(tiempoTareasJob,tiempoTareasLearning),nrow = 2,byrow = TRUE)

actividades <- c("tiempoTareasJob","tiempoLearning","videos","duplicado")


colnames(tiempoMatrix) <- diasSemana
rownames(tiempoMatrix) <- actividades


# sum of matrix columns 
colSums(tiempoMatrix)

# add a data row to matrix
tiempoMatrix <- rbind(tiempoMatrix,c(2,4,5,5,2,2))

# delete indicated  row 
tiempoMatrix <- tiempoMatrix[-c(2,4,5,5,2,2), ]

mtcars[mtcars$cyl>7,]

orangeec[orangeec$V1>20000,]
orangeec[orangeec$GDP.PC>=10000,]

newSubSet <- subset(orangeec,Internet.penetration...population >80
                    & Education.invest...GDP >=5)

# pull out a sub-matrix vista solo from target variable
newSubSet <- subset(orangeec,Internet.penetration...population >80
                    & Education.invest...GDP >=5,
                    select = Creat.Ind...GDP)

newSubSet <- subset(orangeec,Internet.penetration...population >80
                    & Education.invest...GDP >=5,
                    select = Country)

# change column dataset label
plyr::rename(orangeec,c("Education.invest...GDP"="EducationInvestGDP"))

head(mtcars)

head(orangeec)

tail(orangeec)
tail(mtcars)

# glimpse works with library dplyr
glimpse(mtcars)


myVector <- 1:8
myMatrix <- matrix(1:9,ncol = 3)
# pull out indicated rows of a dataset 
myWTF <- mtcars[1:4,]

myList <- list(myVector,myMatrix,myWTF)





















renam




