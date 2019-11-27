library(shiny)

ui <- fluidPage(
  
  titlePanel("Линейный дискриминант Фишера"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        tabsetPanel(id="tab",
          tabPanel(title = "Стандартная выборка", 
           
           column(6, sliderInput("s_mu11", "Мю11", 0, 6, 2, 0.1)), 
           column(6, sliderInput("s_mu12", "Мю12", 0, 6, 4, 0.1)), 
           column(6, sliderInput("s_mu21", "Мю21", 0, 4, 1, 0.1)),
           column(6, sliderInput("s_mu22", "Мю22", 0, 4, 3, 0.1)),
           
           column(6, sliderInput("s_sigma11", "Сигма11", 0.1, 1, 0.6, 0.1)), 
           column(6, sliderInput("s_sigma12", "Сигма12", 0.1, 1, 0.6, 0.1)), 
           column(6, sliderInput("s_sigma21", "Сигма21", 0.1, 1, 0.7, 0.1)),
           column(6, sliderInput("s_sigma22", "Сигма22", 0.1, 1, 0.7, 0.1)),
           
           column(12, sliderInput("s_P", "Вероятность(априорная) появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
          ),
          
          tabPanel(title = "Случайные выборки",
           column(6, sliderInput("n", "Количество элементов 1-го класса", 10, 100, 30, 1)),
           column(6, sliderInput("m", "Количество элементов 2-го класса", 10, 100, 40, 1)),
           
           column(6, sliderInput("r_mu11", "Мю11", 0, 6, 2, 0.1)), 
           column(6, sliderInput("r_mu12", "Мю12", 0, 6, 4, 0.1)), 
           column(6, sliderInput("r_mu21", "Мю21", 0, 4, 1, 0.1)),
           column(6, sliderInput("r_mu22", "Мю22", 0, 4, 3, 0.1)),
           
           column(6, sliderInput("r_sigma11", "Сигма11", 0.1, 1, 0.7, 0.1)), 
           column(6, sliderInput("r_sigma12", "Сигма12", 0.1, 1, 0.7, 0.1)), 
           column(6, sliderInput("r_sigma21", "Сигма21", 0.1, 1, 1, 0.1)),
           column(6, sliderInput("r_sigma22", "Сигма22", 0.1, 1, 1, 0.1)),
           
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

calc_mu <- function(xl) sum(xl) / length(xl)

calc_sigma <- function(xl, mu) {
  sum <- 0
  for (i in 1:nrow(xl)) {
    xi <- matrix(c(xl[i,1], xl[i,2]), 1, 2)
    sum <- sum + t(xi - mu) %*% (xi - mu)
  }
  sum / (nrow(xl)-1)
}

LDF <- function(xl, len1, len2, Py, mu, sigma) {
  
  discriminant <- function(mu, sigma, Py) {
    sigma1 <- sigma[1:2,]
    sigma2 <- sigma[3:4,]
    
    inverse_s1 <- solve(sigma1)
    inverse_s2 <- solve(sigma2)
    
    x1 <- 2 * inverse_s2[1,2] * mu[2,2] - 2 * inverse_s1[1,2] * mu[1,2] - 2 * inverse_s1[1,1] * mu[1,1] + 2 * inverse_s2[1,1] * mu[2,1]
    y1 <- 2 * inverse_s2[1,2] * mu[2,1] + 2 * inverse_s2[2,2] * mu[2,2] - 2 * inverse_s1[1,2] * mu[1,1] - 2 * inverse_s1[2,2] * mu[1,2]
    
    c <- -inverse_s2[1,1] * mu[2,1]^2 - 2 * inverse_s2[1,2] * mu[2,1] * mu[2,2] - inverse_s2[2,2] * mu[2,2]^2 + 
      inverse_s1[1,1] * mu[1,1]^2 + 2 * inverse_s1[1,2] * mu[1,1] * mu[1,2] + inverse_s1[2,2] * mu[1,2]^2 + 
      log(det(sigma1)) - log(det(sigma2)) - Py[1]/Py[2]
    
    func <- function(x, y) {
      x*x1 + y*y1 + c
    }
    
    return(func)
  }
  
  draw_plot <- function(xl, mu, sigma, Py) {
    x <- seq(min(mu[,1]) - 5, max(mu[,1]) + 5, length.out = 100)
    y <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    
    func <- discriminant(mu, sigma, Py)
    z <- outer(x, y, func)
    
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 3, levels = 0, col = "black", drawlabels = F, add = T)
  }
  
  len <- len1 + len2
  
  classes <- unique(xl[,ncol(xl)])
  
  draw_plot(xl, mu, sigma, Py)
}

server <- function(input, output, session) {
  output$plot <- renderPlot({
    if(input$tab == "Стандартная выборка") {
      len1 <- 20
      len2 <- 30
      
      first_x <- rnorm(len1, input$s_mu11, input$s_sigma11)
      first_y <- rnorm(len1, input$s_mu21, input$s_sigma21)
      second_x <- rnorm(len2, input$s_mu12, input$s_sigma12)
      second_y <- rnorm(len2, input$s_mu22, input$s_sigma22)
      
      first <- cbind(first_x, first_y)
      second <- cbind(second_x, second_y)
      
      P1 <- input$s_P
      P2 <- 1 - P1
      Py <- c(P1, P2)
      
      xl <- read.table(file = "example.txt", header = TRUE) 
      
      test_sigma <- matrix(c(input$s_sigma11, input$s_sigma12, input$s_sigma21, input$s_sigma22), 2, 2)
    }
    else {
      len1 <- input$n 
      len2 <- input$m
      
      len <- len1+len2
      
      first_x <- rnorm(len1, input$r_mu11, input$r_sigma11)
      first_y <- rnorm(len1, input$r_mu21, input$r_sigma21)
      second_x <- rnorm(len2, input$r_mu12, input$r_sigma12)
      second_y <- rnorm(len2, input$r_mu22, input$r_sigma22)
      
      P1 <- input$r_P
      P2 <- 1 - P1
      Py <- c(P1, P2)
      
      first <- cbind(first_x, first_y)
      second <- cbind(second_x, second_y)
      
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
      
      test_sigma <- matrix(c(input$r_sigma11, input$r_sigma12, input$r_sigma21, input$r_sigma22), 2, 2)
    }
    
    mu <- rbind(c(calc_mu(first_x), calc_mu(first_y)), c(calc_mu(second_x), calc_mu(second_y)))
    sigma <- rbind(calc_sigma(first, mu[1,]), calc_sigma(second, mu[2,]))
    
    
    LDF(xl, len1, len2, Py, mu, sigma)
  })
}

shinyApp(ui, server)