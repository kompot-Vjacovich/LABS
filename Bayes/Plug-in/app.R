library(shiny)
library(MASS)

ui <- fluidPage(
  
  titlePanel("Plug-in"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        tabsetPanel(id="tab",
                    tabPanel(title = "Стандартная выборка", 
                             
                             column(6, sliderInput("s_lmbd1", "Лямбда 1", 0.5, 2, 1, 0.01)),
                             column(6, sliderInput("s_lmbd2", "Лямбда 2", 0.5, 2, 1, 0.01)),
                             
                             column(6, sliderInput("s_mu11", "Мю11", 0, 6, 2, 0.1)), 
                             column(6, sliderInput("s_mu12", "Мю12", 0, 6, 4, 0.1)), 
                             column(6, sliderInput("s_mu21", "Мю21", 0, 4, 1, 0.1)),
                             column(6, sliderInput("s_mu22", "Мю22", 0, 4, 3, 0.1)),
                             
                             column(6, sliderInput("s_sigma11", "Сигма11", 0.1, 1, 0.7, 0.1)), 
                             column(6, sliderInput("s_sigma12", "Сигма12", 0.1, 1, 0.6, 0.1)), 
                             column(6, sliderInput("s_sigma21", "Сигма21", 0.1, 1, 1, 0.1)),
                             column(6, sliderInput("s_sigma22", "Сигма22", 0.1, 1, 0.7, 0.1)),
                             
                             column(12, sliderInput("s_P", "Вероятность(априорная) появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
                    ),
                    
                    tabPanel(title = "Случайные выборки",
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
                             column(6, sliderInput("r_sigma21", "Сигма21", 0.1, 1, 1, 0.1)),
                             column(6, sliderInput("r_sigma22", "Сигма22", 0.1, 1, 0.7, 0.1)),
                             
                             column(12, sliderInput("r_P", "Вероятность(априорная) появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
                    )
        )
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

calc_sigma <- function(xl, mu) {
  sum <- 0
  for (i in 1:nrow(xl)) {
    xi <- matrix(c(xl[i,1], xl[i,2]), 1, 2)
    sum <- sum + t(xi - mu) %*% (xi - mu)
  }
  sum / (nrow(xl)-1)
}

Pyj <- function(x, mu, sigma) {
  return((1/(sigma*sqrt(2*pi))) * exp(-1 * ((x - mu)^2)/(2*sigma^2)))
}

plug_in <- function(xl, len1, len2, Py, mu, sigma, l1, l2) {
  
  discriminant <- function(mu, sigma, Py) {
    sigma1 <- sigma[1:2,]
    sigma2 <- sigma[3:4,]
    
    inverse_s1 <- solve(sigma1)
    inverse_s2 <- solve(sigma2)
    
    #xn = x^n
    x2 <- inverse_s1[1,1] - inverse_s2[1,1]
    y2 <- inverse_s1[2,2] - inverse_s2[2,2]
    xy <- 2 * inverse_s1[1,2] - 2 * inverse_s2[1,2]
    
    x1 <- 2 * inverse_s2[1,2] * mu[2,2] - 2 * inverse_s1[1,2] * mu[1,2] - 2 * inverse_s1[1,1] * mu[1,1] + 2 * inverse_s2[1,1] * mu[2,1]
    y1 <- 2 * inverse_s2[1,2] * mu[2,1] + 2 * inverse_s2[2,2] * mu[2,2] - 2 * inverse_s1[1,2] * mu[1,1] - 2 * inverse_s1[2,2] * mu[1,2]
    
    c <- -inverse_s2[1,1] * mu[2,1]^2 - 2 * inverse_s2[1,2] * mu[2,1] * mu[2,2] - inverse_s2[2,2] * mu[2,2]^2 + 
      inverse_s1[1,1] * mu[1,1]^2 + 2 * inverse_s1[1,2] * mu[1,1] * mu[1,2] + inverse_s1[2,2] * mu[1,2]^2 + 
      log(abs(det(sigma1))) - log(abs(det(sigma2))) - Py[1]/Py[2]
    
    func <- function(x, y) {
      x^2*x2 + y^2*y2 + x*y*xy + x*x1 + y*y1 + c
    }
    
    return(func)
  }
  
  getLyambda <- function(l1, l2, P1, P2) {
    log((l1*P1)/(l2*P2))
  }
  
  draw_plot <- function(xl, mu, sigma, Py, l1, l2) {
    x <- seq(min(mu[,1]) - 5, max(mu[,1]) + 5, length.out = 100)
    y <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    
    func <- discriminant(mu, sigma, Py)
    z <- outer(x, y, func)
    
    lyambda <- getLyambda(l1, l2, Py[1], Py[2])
    
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 3, levels = lyambda, col = "black", drawlabels = F, add = T)
  }
  
  len <- len1 + len2
  
  draw_plot(xl, mu, sigma, Py, l1, l2)
}

server <- function(input, output, session) {
  output$plot <- renderPlot({
    if(input$tab == "Стандартная выборка") {
      len1 <- 20
      len2 <- 30
      
      s1 <- matrix(c(input$s_sigma11, 0, 0, input$s_sigma12), 2, 2)
      s2 <- matrix(c(input$s_sigma21, 0, 0, input$s_sigma22), 2, 2)
      
      m1 <- c(input$s_mu11, input$s_mu12)
      m2 <- c(input$s_mu21, input$s_mu22)
      
      P1 <- input$s_P
      P2 <- 1 - P1
      Py <- c(P1, P2)
      
      l1 <- input$s_lmbd1
      l2 <- input$s_lmbd2
      
      xl <- read.table(file = "example.txt", header = TRUE) 
    }
    else {
      len1 <- input$n 
      len2 <- input$m
      
      len <- len1+len2
      
      sigma1 <- matrix(c(input$r_sigma11, 0, 0, input$r_sigma12), 2, 2)
      sigma2 <- matrix(c(input$r_sigma21, 0, 0, input$r_sigma22), 2, 2)
      
      mu1 <- c(input$r_mu11, input$r_mu12)
      mu2 <- c(input$r_mu21, input$r_mu22)
      
      first <- mvrnorm(n=len1, mu = mu1, Sigma = sigma1)
      second <- mvrnorm(n=len2, mu = mu2, Sigma = sigma2)
      
      m1 <- calc_mu(first)
      m2 <- calc_mu(second)
      
      s1 <- calc_sigma(first, mu1)
      s2 <- calc_sigma(second, mu2)
      
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
    }
    
    mu <- rbind(m1, m2)
    sigma <- rbind(s1, s2)
    
    plug_in(xl, len1, len2, Py, mu, sigma, l1, l2)
  })
}

shinyApp(ui, server)