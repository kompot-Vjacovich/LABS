

library(MASS)



calc_mu <- function(xl) {
  l <- dim(xl)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xl[,i])
  }
  return(res)
}

calc_sigma <- function(first, second, mu1, mu2) {
  rows1 <- nrow(first)
  rows2 <- nrow(second)
  cols <- ncol(first)
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows1)) {
    res <- res + t(first[i,] - mu1) %*% (first[i,] - mu1)
  }
  for (i in seq(rows2)) {
    res <- res + t(second[i,] - mu2) %*% (second[i,] - mu2)
  }
  
  return(res/(rows1 + rows2 + 2))
}

LDF <- function(xl, len1, len2, Py, mu1, mu2, sigma, l1, l2, map) {
  Nu <- function(ksi, mu, sigma) (1/(sigma*sqrt(2*pi)))*exp(-(ksi-mu)^2 / (2*sigma^2))
  
  classification <- function(x, classes, mu, sigma, Py, lyambda) {
    classSum <- rep(0, length(classes))
    names(classSum) <- classes
    sigma[1,2] <- sigma[2,1] <- 1
    newSigma <- rbind(c(sigma[1,1], sigma[1,2]), c(sigma[1,1], sigma[1,2]))
    
    for (i in 1:length(classes)) {
      tmpSum <- 0
      
      for (j in 1:length(x)) {
        tmP <- Nu(x[j], mu[i,j], newSigma[i,j])
        tmpSum <- tmpSum + log(tmP)
      }
      classSum[i] <- tmpSum + log(lyambda[i]*Py[i])
    }
    
    return(names(which.max(classSum)))
  }
  
  classify_all <- function(classes, mu, sigma, Py, lyambda) {
    classifiedObj <- c()
    
    for(i in seq(min(mu[,1]) - 5, max(mu[,1] + 5), 0.1)) {
      for(j in seq(min(mu[,2]) - 5, max(mu[,2] + 5), 0.1)) {
        classifiedObj <- rbind(classifiedObj, c(i, j, classification(c(i, j), classes, mu, sigma, Py, lyambda)))
      }
    }
    return(classifiedObj)
  }
  
  discriminant <- function(mu1, mu2, sigma, Py) {
    
    inverse <- solve(sigma)
    b <- inverse %*% t(mu1 - mu2)
    
    x1 <- b[1, 1]
    y1 <- b[2, 1] 
    m <- (mu1 + mu2)
    const <- c(m %*% b) / 2
    
    func <- function(x, y) {
      x*x1 + y*y1 - const
    }
    
    return(func)
  }
  
  getLyambda <- function(l1, l2, P1, P2) {
    log((l1*P1)/(l2*P2))
  }
  
  draw_plot <- function(xl, mu1, mu2, sigma, Py, l1, l2) {
    mu <- rbind(mu1, mu2)
    x <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    y <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    
    func <- discriminant(mu1, mu2, sigma, Py)
    z <- outer(x, y, func)
    
    lyambda <- getLyambda(l1, l2, Py[1], Py[2])
    
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 2.5, type="l", levels = lyambda, col = "black", drawlabels = F, add = T)
  }
  
  draw_map <- function(xl, classifiedObj) {
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    points(classifiedObj[, 1:(n-1)], pch = 21, col = colors[classifiedObj[, n]])
  }
  
  len <- len1 + len2
  
  if(!map) {
    draw_plot(xl, mu1, mu2, sigma, Py, l1, l2)
  }
  else {
    classes <- unique(xl[,ncol(xl)])
    lyambda <- c(l1, l2)
    mu <- rbind(mu1, mu2)
    classified <- classify_all(classes, mu, sigma, Py, lyambda)
    draw_map(xl, classified)
  }
  
}

len1 <- 30
len2 <- 40
len <- len1+len2

xl <- read.table(file = "example.txt", header = TRUE)

first <- xl[1:len1,]
second <- xl[(len1+1):len,]


mu1 <- matrix(c(0, 3), 1, 2)
mu2 <- matrix(c(4, 3), 1, 2)

sigma <- matrix(c(0.5, 0, 0, 0.6), 2, 2)
print(sigma)
P1 <- 0.5
P2 <- 1 - P1
Py <- c(P1, P2)

l1 <- 1
l2 <- 1

map <- TRUE


LDF(xl, len1, len2, Py, mu1, mu2, sigma, l1, l2, map)
if(!map) lines(c(mu1[1], mu2[1]), c(mu1[2], mu2[2]), col = 'magenta', lwd = 2)
