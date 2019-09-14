distance <- function(u, v)
{
	sqrt(sum((u - v)^2))
}

sortDist <- function(dots, x) {
	l <- dim(dots)[1]
	n <- dim(dots)[2] - 1
	mtx <- matrix(NA, l, 2)

	for(i in 1:l) {
		mtx[i, ] <- c(i, distance(dots[i, 1:n], x))
	}

	sorted <- dots[order(mtx[, 2]), ]

	return(sorted);
}

kNN <- function(dots, x, k) {
	sorted <- sortDist(dots, x)
	n <- dim(dots)[2] - 1
	classes <- sorted[1:k, n+1]

	counts <- table(classes)
	class <- names(which.max(counts))

	return(class)
}

colors <- c("setosa" = "red", "versicolor" = "green3",
"virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
= colors[iris$Species], asp = 1)

for(i in 1:10) {
	a <- runif(1, 0, 7)
	b <- runif(1, 0, 3)
	x <- c(a, b)
	dots <- iris[, 3:5]
	k <- 10
	class <- kNN(dots, x, k)
	points(x[1], x[2], pch = 22, bg = colors[class], asp = 1)
}

