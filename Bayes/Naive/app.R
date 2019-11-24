library(shiny)

ui <- fluidPage(
  
  titlePanel("Наивный байесовский классификатор"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        tabsetPanel(id="tab",
          tabPanel(title = "Стандартная выборка", 
            column(6, sliderInput("lmbd1", "Лямбда 1", 0.5, 2, 1, 0.01)),
            column(6, sliderInput("lmbd2", "Лямбда 2", 0.5, 2, 1, 0.01)),
            
            column(6, sliderInput("mu11", "Мю11", 0, 6, 2, 0.1)), 
            column(6, sliderInput("mu12", "Мю12", 0, 6, 4, 0.1)), 
            column(6, sliderInput("mu21", "Мю21", 0, 4, 1, 0.1)),
            column(6, sliderInput("mu22", "Мю22", 0, 4, 3, 0.1)),
            
            column(6, sliderInput("sigma11", "Сигма11", 0.1, 1, 0.7, 0.1)), 
            column(6, sliderInput("sigma12", "Сигма12", 0.1, 1, 0.6, 0.1)), 
            column(6, sliderInput("sigma21", "Сигма21", 0.1, 1, 1, 0.1)),
            column(6, sliderInput("sigma22", "Сигма22", 0.1, 1, 0.7, 0.1)),
            
            column(12, sliderInput("P", "Вероятность(априорная) появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
          ),
          
          tabPanel(title = "Случайные выборки",
            column(6, sliderInput("n", "Количество элементов 1-го класса", 10, 100, 30, 1)),
            column(6, sliderInput("m", "Количество элементов 2-го класса", 10, 100, 40, 1)),
            
            column(6, sliderInput("lmbd1", "Лямбда 1", 0.5, 2, 1, 0.01)),
            column(6, sliderInput("lmbd2", "Лямбда 2", 0.5, 2, 1, 0.01)),
            
            column(6, sliderInput("mu11", "Мю11", 0, 6, 2, 0.1)), 
            column(6, sliderInput("mu12", "Мю12", 0, 6, 4, 0.1)), 
            column(6, sliderInput("mu21", "Мю21", 0, 4, 1, 0.1)),
            column(6, sliderInput("mu22", "Мю22", 0, 4, 3, 0.1)),
            
            column(6, sliderInput("sigma11", "Сигма11", 0.1, 1, 0.7, 0.1)), 
            column(6, sliderInput("sigma12", "Сигма12", 0.1, 1, 0.6, 0.1)), 
            column(6, sliderInput("sigma21", "Сигма21", 0.1, 1, 1, 0.1)),
            column(6, sliderInput("sigma22", "Сигма22", 0.1, 1, 0.7, 0.1)),
            
            column(12, sliderInput("P", "Вероятность(априорная) появления Класса1|Класса2", 0.01, 0.99, 0.5, 0.01))
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

calc_sigma <- function(xl, mu) sum((xl-mu)^2)/(length(xl)-1)

naive_bayes <- function(xl, len1, len2, P, lyambda, mu, sigma) {
  p <- function(ksi, mu, sigma) (1/(sigma*sqrt(2*pi)))*exp(-(ksi-mu)^2 / (2*sigma^2))
  
  classification <- function(x, classes, mu, sigma, Py, lyambda) {
    classSum <- rep(0, length(classes))
    names(classSum) <- classes
    
    for (i in 1:length(classes)) {
      tmpSum <- 0
      
      for (j in 1:length(x)) {
        tmP <- p(x[j], mu[i,j], sigma[i,j])
        tmpSum <- tmpSum + log(tmP)
      }
      classSum[i] <- tmpSum + log(lyambda[i]*Py[i])
    }
    
    return(names(which.max(classSum)))
  }
  
  classify_all <- function(classes, mu, sigma, Py, lyambda) {
    classifiedObj <- c()
    
    for(i in seq(min(mu[, 1]) - 5, max(mu[, 1]) + 5, 0.1)) {
      for(j in seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, 0.1)) {
        classifiedObj <- rbind(classifiedObj, c(i, j, classification(c(i, j), classes, mu, sigma, Py, lyambda)))
      }
    }
    
    return(classifiedObj)
  }
  
  draw_plot <- function(xl, classifiedObj) {
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    points(classifiedObj[, 1:(n-1)], pch = 21, col = colors[classifiedObj[, n]])
  }
  
  Py <- P
  len <- len1 + len2
  
  classes <- unique(xl[,ncol(xl)])
  
  classified <- classify_all(classes, mu, sigma, Py, lyambda)
  draw_plot(xl, classified)
}

server <- function(input, output) {
  
  output$plot <- renderPlot({
    if(input$tab == "Стандартная выборка") {
      len1 <- 20
      len2 <- 30
      
      first_x <- rnorm(len1, input$mu11, input$sigma11)
      first_y <- rnorm(len1, input$mu21, input$sigma21)
      second_x <- rnorm(len2, input$mu12, input$sigma12)
      second_y <- rnorm(len2, input$mu22, input$sigma22)
      
      xl <- read.table(file = "example.txt", header = TRUE) 
    }
    else {
      len1 <- input$n 
      len2 <- input$m
      
      len <- len1+len2
      
      first_x <- rnorm(len1, input$mu11, input$sigma11)
      first_y <- rnorm(len1, input$mu21, input$sigma21)
      second_x <- rnorm(len2, input$mu12, input$sigma12)
      second_y <- rnorm(len2, input$mu22, input$sigma22)
      
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
    }
    
    mu <- rbind(c(calc_mu(first_x), calc_mu(first_y)), c(calc_mu(second_x), calc_mu(second_y)))
    sigma <- rbind(c(calc_sigma(first_x, mu[1,1]), calc_sigma(first_y, mu[1,2])), 
                   c(calc_sigma(second_x, mu[2,1]), calc_sigma(second_y, mu[2,2])))
    
    P1 <- input$P
    P2 <- 1 - P1
    Py <- c(P1, P2)
    
    l1 <- input$lmbd1
    l2 <- input$lmbd2
    lyambda <- c(l1, l2)
    
    naive_bayes(xl, len1, len2, Py, lyambda, mu, sigma)
  })
}

shinyApp(ui = ui, server = server)