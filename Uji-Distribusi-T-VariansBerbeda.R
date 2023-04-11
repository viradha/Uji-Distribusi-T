x1 <- (c(7,6,5,6,5,4,4,6,6,7,6,5))
x2 <- (c(8,6,7,8,6,6,7,7,8,7,6,7))
UjiT <- function(x1,x2){
  n1 <- length(x1)
  n2 <- length(x2)
  
  xbar1<- mean(x1)
  xbar2<- mean(x2)
  
  Ujit <- (xbar1 - xbar2) / (sqrt((var(x1)/n1)+(var(x2)/n2))) 
  Ujit
  db <- ((var(x1)/n1)+(var(x2)/n2))^2/((var(x1)/n1)^2/(n1-1))+((var(x2)/n2)^2/(n1-1))
  
  ttabel<- qt(p=.05, df=db, lower.tail=FALSE)
  ttabel
  ttabelmin <- ttabel * -1
  
  if (Ujit>ttabel || Ujit< ttabelmin){
    print("Tolak H0")
  } else {
    print("Terima H0")
  }
}
n1 <- length(x1)
n2 <- length(x2)

xbar1<- mean(x1)
xbar2<- mean(x2)

Ujit <- (xbar1 - xbar2) / (sqrt((var(x1)/n1)+(var(x2)/n2))) 
Ujit
db <- ((var(x1)/n1)+(var(x2)/n2))^2/((var(x1)/n1)^2/(n1-1))+((var(x2)/n2)^2/(n1-1))

ttabel<- qt(p=.05, df=db, lower.tail=FALSE)
ttabel
ttabelmin <- ttabel * -1

if (Ujit>ttabel || Ujit< ttabelmin){
  print("Tolak H0")
} else {
  print("Terima H0")
}
