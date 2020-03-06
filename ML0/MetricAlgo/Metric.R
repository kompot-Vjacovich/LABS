library("plotrix")
#Euclid distancne
distance <- function(u, v)
{
	sqrt(sum((u - v)^2))
}

#Sort by distance
sortDist <- function(dots, x) {
	len <- length(dots[[1]])
	mtx <- vector("list", len)

	for(i in 1:len) {
		mtx[[i]] <- distance(dots[i,][1:2], x)
	}

	dots <- data.frame(dots, "Distance" = unlist(mtx))
	dots <- dots[order(dots$Distance), ]

	return(dots);
}

#kNN algo
kNN <- function(dots, x, k=c(6)) {
	dots <- sortDist(dots, x)
	
	class <- list()
	for (i in seq(length(k))) {
	  k_first <- head(dots, k[i])
	  count_oc <- summary(k_first$Species)
	  class[i] <- names(sort(count_oc, decreasing = TRUE))[1]
	}
	res <- unlist(class)

	return(res)
}

#kwNN algo
kwNN <-function(dots, x, q=c(0.8), k=6) {
  dots <- sortDist(dots, x)
  
  class <- list()
  k_first <- head(dots, k)
  
  classes <- as.list(rep(0, 3))
  names(classes) = levels(dots$Species)
  
  for (i in seq(length(q))) {
    for (j in seq(k)) {
      yi <- k_first[j,]
      classes[[yi$Species]] <- classes[[yi$Species]] + q[i]^j
    }
    
    class[i] <- names(sort(unlist(classes), decreasing = TRUE))[1]
  }
  
  res <- unlist(class)
  
  return(res)
}

#Combined LOO
loo <- function(dots, algo, kORq) {
  len <- length(dots[[1]])
  mark <- rep(0, length(kORq))
  
  for (i in seq(len)) {
    training <- dots[-i,]
    control <- dots[i,]
    
    resClass <- algo(training, control[1:2], kORq)
    mark <- mark + (resClass != control$Species)/len
  }
  
  return(mark)
}

#Combined LOO visualization
plotLOO <- function(dots, algo, kORq, letter, title="LOO") {
  mark <- loo(dots, algo, kORq)
  Loo <- if(letter == "k") data.frame("k"=kORq, "LOO"=mark) else if(letter == "q") data.frame("q"=kORq, "LOO"=mark) 
          else data.frame("h"=kORq, "LOO"=mark)
  
  min_point <- Loo[which.min(Loo$LOO),]
  
  plot(Loo, type = "l", main = title, xlab = letter, ylab="mark")
  points(min_point, pch=21, col="black", bg="red")
  
  txt <- paste0(paste0(letter,"="), min_point[1, letter], "\nLOO=", round(min_point[1, "LOO"], 5))
  text(min_point[1, letter]+0.7, min_point[1,"LOO"]+0.002, labels=txt)
}

#Algo visualization
plotAlg <- function(dots, algo) {
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(dots[1:2], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], ylim = c(0.0, 2.7))
  
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      class <- algo(dots, c(i,j))
      points(i, j, pch=21, col = colors[class])
    }
  }
}

#kwNN or kNN???
example <- function() {
  sl <- c(5.0, 4.9, 4.7, 5.2, 5.2, 5.9, 6.0)
  sw <- c(3.5, 3.0, 3.2, 2.7, 3.5, 3.1, 2.0)
  pl <- c(1.4, 1.4, 1.3, 3.0, 2.7, 3.2, 3.1)
  pw <- c(0.2, 0.2, 0.2, 1.4, 1, 1.5, 1)
  s <- c("setosa", "setosa", "setosa", "versicolor", "versicolor", "versicolor", "versicolor")
  
  demo_dots <- data.frame(Sepal.Length = sl, Sepal.Width = sw, Petal.Length = pl, Petal.Width = pw, Species = s)
  
  demo_plot <- par(mfrow = c(1, 2))
  demo_point <- c(1.8, 0.4)
  
  colors <- c("setosa" = "red", "versicolor" = "green3")
  plot(demo_dots[, 3:4], pch = 21, bg = colors[demo_dots$Species], col = colors[demo_dots$Species], ylim = c(0.0, 1.7), 
       main = "kNN, k=7")
  
  class_knn = kNN(demo_dots, demo_point, k=c(7))
  points(demo_point[1], demo_point[2], pch=21, col = colors[class_knn])
  text(demo_point[1]+0.6, demo_point[2], paste("class =", class_knn))
  
  plot(demo_dots[, 3:4], pch = 21, bg = colors[demo_dots$Species], col = colors[demo_dots$Species], ylim = c(0.0, 1.7), 
       main = "kwNN, k=7, q=0.8")
  
  class_kwnn = kwNN(demo_dots, demo_point, k=c(7))
  points(demo_point[1], demo_point[2], pch=21, col = colors[class_kwnn])
  text(demo_point[1]+0.6, demo_point[2], paste("class =", class_kwnn))
  
  par(demo_plot)
}

# Core functions
cR <- function(z) 0.5 * (abs(z)<=1) # Rectangle
cT <- function(z) (1 - abs(z))*(abs(z)<=1) #  Triangle
cQ <- function(z) (15/16)*(1 - z^2)^2 * (abs(z)<=1) # Quartic
cE <- function(z) (3/4)*(1-z^2) * (abs(z)<=1) # Epanechnikovo
cG <- function(z) (2*pi)^0.5 * exp(-0.5 * z*z) # Gaus

#Parsen Windows
parsen <- function(dots, x, h=c(0.35), core=cG){
  dots <- sortDist(dots, x)
  count_classes <- dim(table(dots$Species))
  
  class <- list()
  
  classes <- as.list(rep(0, count_classes))
  names(classes) = levels(dots$Species)
  
  for (i in seq(length(h))) {
    for (j in seq(length(dots[[1]]))) {
      yi <- dots[j,]
      classes[[yi$Species]] <- classes[[yi$Species]] + core(yi$Distance / h[i])
    }
    
    if(max(unlist(classes)) > 0) {
      class[i] <- names(sort(unlist(classes), decreasing = TRUE))[1]
    }
    else class[i] <- "unknown"
  }
  
  res <- unlist(class)
  return(res)
}

#Potential functions
potential <- function(dots, x, h=H, g=G, core=cG) {
  
  count_classes <- dim(table(dots$Species))
  
  classes <- rep(0, count_classes)
  names(classes) <- levels(dots$Species)
  
  for (i in seq(length(dots[[1]]))) {
    yi <- dots[i,]
    dist <- distance(x, yi[1:2])
    classes[yi$Species] <- classes[yi$Species] + core(dist / h[i]) * g[i]
  }
    
  if(max(classes) == 0) {
    res <- "unknown"
  }
  else res <- names(which.max(classes))
  
  return(res) 
}

#Error counter
error_cnt <- function(dots, h, g, core) {
  error <- 0
  
  for (i in seq(length(dots[[1]]))) {
    yi <- dots[i,]
    res <- potential(dots, yi[1:2], h, g, core)
    
    error <- error + (yi$Species != res)
  }
  return(error)
}

#Find gamma
find_gamma <- function(dots, h, maxError=5, core=cG){
  len <- length(dots[[1]])
  
  g <- rep(0, len)
  i <- 1
  
  while(error_cnt(dots, h, g, core) > maxError) {
    yi <- dots[i,]
    res <- potential(dots, yi[1:2], h, g, core)
    
    g[i] <- g[i] + (yi$Species != res)
    
    i <- sample(seq(len), 1)
  }
  return(g)
}


#Potential circles
draw_circles <- function(dots, g, h) {
  
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(dots[1:2], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], ylim = c(0.0, 2.7))
  
  max_gamma <- max(g)
  
  for (i in seq(length(g))) {
    k <- g[i]/max_gamma
    yi <- dots[i,]
    if(g[i] > 0) {
      color = adjustcolor(colors[yi$Species], k /5 )
      draw.circle(yi[,1], yi[,2], h[i], border = color, col = color)
    }
  }
  
}

#data <- rbind(iris[6:20,], iris[61:75,], iris[136:150,])
data <- iris[3:5]
len <- length(data[[1]])
#H <- c(rep(1, len/3), rep(0.25, (2*len/3)))
H <- c(rep(1, len/3), rep(0.5, (2*len/3)))
G <- find_gamma(data, H, maxError=5, core=cG)


draw_circles(iris[3:5], g=G, h=H)
#plotAlg(iris[3:5], potential)
#plotLOO(iris[3:5], parsen, kORq=seq(0.1, 4, 0.05), "h", "LOO")
#example()
