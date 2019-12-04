library(shiny)
library(MASS)

ui <- fluidPage(
  
  titlePanel("Plug-in"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
           column(6, sliderInput("n", "Количество элементов 1-го класса", 10, 100, 30, 1)),
           column(6, sliderInput("m", "Количество элементов 2-го класса", 10, 100, 40, 1)),
           
           column(6, sliderInput("r_lmbd1", "Лямбда 1", 0.5, 2, 1, 0.01)),
           column(6, sliderInput("r_lmbd2", "Лямбда 2", 0.5, 2, 1, 0.01)),
           
           column(6, sliderInput("r_mu11", "Мю11", 0, 6, 2, 0.1)), 
           column(6, sliderInput("r_mu12", "Мю12", 0, 6, 4, 0.1)), 
           column(6, sliderInput("r_mu21", "Мю21", 0, 4, 1, 0.1)),
           column(6, sliderInput("r_mu22", "Мю22", 0, 4, 3, 0.1)),
           
           column(6, sliderInput("r_sigma11", "Сигма11", 0.1, 1, 0.7, 0.1)), 
           column(6, sliderInput("r_sigma12", "Сигма12", 0.1, 1, 0.6, 0.1)),
           
           column(12, sliderInput("r_P", "Вероятность(априорная) появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
          
      )
    ),
    
    mainPanel(
      plotOutput("plot", width = "800px", height = "600px")
    )
  )
)

calc_mu <- function(xl) {
  l <- dim(xl)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xl[,i])
  }
  return(c(res))
}

calc_sigma <- function(first, second, mu) {
  rows1 <- nrow(first)
  rows2 <- nrow(second)
  mu1 <- mu[1,]
  mu2 <- mu[2,]
  
  res <- 0
  for (i in seq(rows1)) {
    res <- res + t(first[i,] - mu1) %*% (first[i,] - mu1)
  }
  for (i in seq(rows2)) {
    res <- res + t(second[i,] - mu2) %*% (second[i,] - mu2)
  }
  
  return(res/(rows1 + rows2 + 2))
}

LDF <- function(xl, len1, len2, Py, mu, sigma, l1, l2) {
  
  discriminant <- function(mu, sigma, Py) {
    d <- det(sigma)
    inverse <- solve(sigma)
    mu1 <- mu[1,]
    mu2 <- mu[2,] 
    
    b <- inverse %*% t(mu1 - mu2)
    
    x1 <- b[1, 1]
    y1 <- b[1, 2] 
    mu <- (mu1 + mu2)
    const <- c(mu %*% b) / 2
    
    func <- function(x) {
      -x*x1/y1 + const/y1
    }
    
    return(func)
  }
  
  getLyambda <- function(l1, l2, P1, P2) {
    log((l1*P1)/(l2*P2))
  }
  
  draw_plot <- function(xl, mu, sigma, Py, l1, l2) {
    x <- seq(min(mu[,1]) - 5, max(mu[,1]) + 5, length.out = 100)
    
    func <- discriminant(mu, sigma, Py)
    
    lyambda <- getLyambda(l1, l2, Py[1], Py[2])
    
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    lines(x, func(x), lwd = 2.5, type="l")
  }
  
  len <- len1 + len2
  
  draw_plot(xl, mu, sigma, Py, l1, l2)
}

server <- function(input, output, session) {
  output$plot <- renderPlot({
    len1 <- input$n 
    len2 <- input$m
    
    len <- len1+len2
    
    sigma1 <- matrix(c(input$r_sigma11, 0, 0, input$r_sigma12), 2, 2)
    
    mu1 <- c(input$r_mu11, input$r_mu12)
    mu2 <- c(input$r_mu21, input$r_mu22)
    
    first <- mvrnorm(n=len1, mu = mu1, Sigma = sigma1)
    second <- mvrnorm(n=len2, mu = mu2, Sigma = sigma1)
    
    m1 <- calc_mu(first)
    m2 <- calc_mu(second)
    
    mu <- rbind(m1, m2)
    sigma <- calc_sigma(first, second, mu)
    
    P1 <- input$r_P
    P2 <- 1 - P1
    Py <- c(P1, P2)
    
    l1 <- input$r_lmbd1
    l2 <- input$r_lmbd2
    
    colnames(first) <- c()
    colnames(second) <- c()
    
    xl <- data.frame()
    xl <- rbind(xl, first)
    xl <- rbind(xl, second)
    
    classes <- 1:len
    classes[1:len1] <- "first"
    classes[(len1+1):len] <- "second"
    xl <- cbind(xl, classes)
    
    colnames(xl) <- c("X", "Y", "Classes")
    
    LDF(xl, len1, len2, Py, mu, sigma, l1, l2)
    lines(mu1, mu2, col = 'magenta', lwd = 2)
  })
}

shinyApp(ui, server)