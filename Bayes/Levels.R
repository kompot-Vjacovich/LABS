Ng <- function(x, mu, mtx) { #n-мерное гауссовское распределение
  a11 <- mtx[1,1]
  a12 <- mtx[1,2]
  a21 <- mtx[2,1]
  a22 <- mtx[2,2]
  
  det = det(mtx)
  
  A <- a22/det
  B <- -(a12+a21)/det
  C <- a11/det
  D <- 
  
  (1/(sqrt((2*pi)^2*det(mtx))))*exp((-1/2)*...)
}  

Ng(c(1, 1), matrix(0,1,2), matrix(c(1,0,0,1),2,2))
