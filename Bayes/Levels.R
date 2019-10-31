draw_lines <- function(mu, mtx, title) { #n-мерное гауссовское распределение
  a11 <- mtx[2,2]
  a12 <- mtx[1,2]
  a21 <- mtx[2,1]
  a22 <- mtx[1,1]
  
  det = det(mtx)
  
  A <- a22/det
  B <- a11/det
  C <- (-a12-a21)/det
  D <- (-2*a22*mu[1] + mu[2]*(a12+a21))/det
  E <- (-2*a11*mu[2] + mu[1]*(a12+a21))/det
  F <- (a22*mu[1]^2 + a11*mu[2]^2 - mu[1]*mu[2]*(a12+a21))/det
  
  N <- function(x,y) (1/(sqrt((2*pi)^2*det(mtx))))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  
  x <- seq(-4, 4, 8/200)
  y <- seq(-4, 4, 8/200)
  z <- outer(x, y, N)
  
  par(pty="s")
  contour(x, y, z, main=title)
  
}  


draw_lines(c(0,0), matrix(c(1,0,0,1), 2, 2), title="Признаки имеют одинаковые дисперсии")
#draw_lines(c(0,0), matrix(c(1,1,0,1), nrow=2, ncol=2), title="Признаки коррелированы")
#draw_lines(c(0,0), matrix(c(3,0,0,1), nrow=2, ncol=2), title="Признаки некоррелированы")
#draw_lines(c(0,0), matrix(c(1,0,0,3), nrow=2, ncol=2), title="Признаки некоррелированы")
