library(shiny)
require("plotrix")

ui <- fluidPage(
  
  titlePanel("Линейные классификаторы"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, radioButtons("classifiers", "Классификаторы", c("ADALINE" = 0, 
                                                                   "Персептрон Розенблатта" = 1, 
                                                                   "Логистич. регрессия" = 2, 
                                                                   "Сравнение" = 3),
                                selected = 0, inline = T), style = "text-align: center"),
        column(12, checkboxInput("drow_iter", "Отображать итерации", F)),
        column(6, selectInput("eps_q", "Погрешность Q", c(0.00001, 0.0001, 0.001, 0.01, 0.1), 0.001)),
        column(6, sliderInput("max_cnt", "MAX количество итераций", 1000, 5000, 2000, 1000))
        
      )
    ),
    
    mainPanel(
      plotOutput("plot", width = "1200px", height = "600px")
    )
  )
)

normalize <- function(xl) {
  for (i in 1:(ncol(xl)-1)) xl[,i] <- (xl[,i] - mean(xl[,i])) / sd(xl[,i])
  return (xl)
}

margin <- function(w, object, class) w %*% as.matrix(object) * class

adaLoss <- function(m) (1-m)^2
adaW <- function(w, object, class, eta) w - c(eta) * (w %*% object - class) %*% object

persLoss <- function(m) max(-m,0)
persW <- function(w, object, class, eta) w + c(eta) * object * class

logLoss <- function(m) log2(1 + exp(-m))
logW <- function(w, object, class, eta) w + c(eta) * object * class * sigmaZ(c(w %*% object) * class)

sigmaZ <- function(z) 1 / (1 + exp(-z))

logMap <- function(w) {
  p <- function(x,y,w) sigmaZ(x*w[1]+y*w[2]-w[3])-sigmaZ(-x*w[1]-y*w[2]+w[3])
  
  P = matrix(0, 100, 100)
  
  for(i in seq(from=0, to=1, by=0.1)){
    for(j in seq(from=0, to=1, by=0.1)){
      P[i*10+1,j*10+1] = p(i,j,w)
    }
  }
  k = 1/max(max(P), -min(P))
  for(i in seq(from=-2.5, to=3, by=0.05)){
    for(j in seq(from=-2, to=3, by=0.05)){
      pp = p(i,j,w)
      #if(abs(pp) != 1) print(pp)
      if(pp>0){
        color = adjustcolor("red",pp*k)
        draw.circle(i, j, 0.025, 5, border = color, col = color)
      }
      if(pp<0){
        color = adjustcolor("blue",-pp*k)
        draw.circle(i, j, 0.025, 5, border = color, col = color)
      } 
    }
  }
}

gradient <- function(xl, eta, lambda, rule, loss_function, max_cnt, eps_q) {
  l <- nrow(xl)
  n <- ncol(xl)
  w <- matrix(c(runif(n-1, -1/(2*(n-1)), 1/(2*(n-1)))), 1, 3)
  objects <- xl[,-n]
  classes <- xl[, n]
  q <- sum(sapply(1:l, function(i) loss_function(margin(w, objects[i,], classes[i])))) 
  q_full <- matrix(q, 1, 1)
  
  cnt <- 0
  while (T) {
    cnt <- cnt + 1
    margins <- sapply(1:l, function(i) margin(w[cnt,], objects[i,], classes[i]))
    errors <- which(margins < 0)
    
    if (length(errors) > 0) rand <- sample(errors, 1)
    else rand <- sample(1:l, 1)
    
    eps <- loss_function(margin(w[cnt,], objects[rand,], classes[rand]))
    
    eta <- 1 / (objects[rand,] %*% objects[rand,])^2

    w <- rbind(w, rule(w[cnt,], objects[rand,], classes[rand], eta))
    
    q_prev <- q
    q <- (1 - lambda) * q + lambda * eps
    q_full <- rbind(q_full, q)
    
    if (abs(q_prev - q) / max(q_prev, q) <= eps_q) break
    else if (cnt == max_cnt) break
  }
  w <- cbind(w, q_full)
  return (w)
}

adaline <- function(xl, drow_iter, max_cnt, eps_q) {
  w <- gradient(xl, 1, 1/6, adaW, adaLoss, max_cnt, eps_q)
  q <- w[,ncol(w)]
  
  draw_change(q)
  draw_lines(xl, w, drow_iter, label="Классификатор: ADALINE")
}

perceptron <- function(xl, drow_iter, max_cnt, eps_q) {
  w <- gradient(xl, 1, 1/6, persW, persLoss, max_cnt, eps_q)
  q <- w[,ncol(w)]
  
  draw_change(q)
  draw_lines(xl, w, drow_iter, label="Классификатор: Персептрон Розенблатта")
}

logistic <- function(xl, drow_iter, max_cnt, eps_q) {
  w <- gradient(xl, 1, 1/6, logW, logLoss, max_cnt, eps_q)
  q <- w[,ncol(w)]
  
  draw_change(q)
  draw_lines(xl, w, drow_iter, label="Классификатор: Логистическая регрессия")
  n <- nrow(w)
  w_final <- c(w[n,1], w[n,2], w[n,3])
  logMap(w_final)
}

compare <- function(xl, drow_iter, max_cnt, eps_q) {
  n <- ncol(xl)
  w1 <- gradient(xl, 1, 1/6, adaW, adaLoss, max_cnt, eps_q)
  w2 <- gradient(xl, 1, 1/6, persW, persLoss, max_cnt, eps_q)
  w3 <- gradient(xl, 1, 1/6, logW, logLoss, max_cnt, eps_q)
  l1 <- nrow(w1)
  l2 <- nrow(w2)
  l3 <- nrow(w3)
  colors <- c("blue", "white", "red")
  plot(xl[,1:(n-2)], type="n", asp = 1, main = "Сравнение линейных классификаторов")
  if (drow_iter == T) {
    for (i in 1:(l1-1)) 
      abline(a = w1[i,3]/w1[i,2], b = -w1[i,1]/w1[i,2], lwd = 1, col = "black")
    for (i in 1:(l2-1)) 
      abline(a = w2[i,3]/w2[i,2], b = -w2[i,1]/w2[i,2], lwd = 1, col = "black")
    for (i in 1:(l3-1)) 
      abline(a = w3[i,3]/w3[i,2], b = -w3[i,1]/w3[i,2], lwd = 1, col = "black")
  }
  abline(a = w1[l1,3]/w1[l1,2], b = -w1[l1,1]/w1[l1,2], lwd = 3, col = "green")
  abline(a = w2[l2,3]/w2[l2,2], b = -w2[l2,1]/w2[l2,2], lwd = 3, col = "gold")
  abline(a = w3[l3,3]/w3[l3,2], b = -w3[l3,1]/w3[l3,2], lwd = 3, col = "violet")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
  legend("bottomright", c("ADALINE", "Персептрон", "Логистич. регрессия"), pch = c("l","l","l"), col = c("green", "gold", "violet"))
}

draw_change <- function(q) {
  par(mfrow=c(1, 2))
  plot(q, type = "l", bg = "red", col = "red", main = "График изменения Q", xlab = "Итерации", ylab = "Значения Q")
}

draw_lines <- function(xl, w, drow_iter, label) {
  n <- ncol(xl)
  l <- nrow(w)
  colors <- c("blue", "white", "red")
  plot(xl[,1:(n-2)], type="n", asp = 1, main = label)
  if (drow_iter == T) 
    for (i in 1:(l-1)) 
      abline(a = w[i,3]/w[i,2], b = -w[i,1]/w[i,2], lwd = 1, col = "black")
  abline(a = w[l,3]/w[l,2], b = -w[l,1]/w[l,2], lwd = 3, col = "green")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
}

server <- function(input, output, session) {
  output$plot <- renderPlot({

    len1 <- 30
    len <- 70
    
    xl <- read.table(file = "example.txt", header = TRUE)
    first_x <- xl[1:len1, 1]
    first_y <- xl[1:len1, 2]
    second_x <- xl[(len1+1):len, 1]
    second_y <- xl[(len1+1):len, 2]
    
    first <- cbind(first_x, first_y)
    second <- cbind(second_x, second_y)
    
    newxl <- rbind(cbind(first, 1), cbind(second, -1))
    
    norm_xl <- normalize(newxl)
    norm_xl <- cbind(norm_xl[,1:(ncol(xl)-1)], -1, norm_xl[, 3])
    colnames(norm_xl) <- c("X", "Y", "Wj", "Classes")
    
    drow_iter <- input$drow_iter
    
    eps_q <- as.numeric(input$eps_q)
    max_cnt <- as.numeric(input$max_cnt)
    
    if (input$classifiers == 0) {
      adaline(norm_xl, drow_iter, max_cnt, eps_q)
    }
    if (input$classifiers == 1) {
      perceptron(norm_xl, drow_iter, max_cnt,eps_q)
    }
    if (input$classifiers == 2) {
      logistic(norm_xl, drow_iter, max_cnt, eps_q)
    }
    if (input$classifiers == 3) {
      compare(norm_xl, drow_iter, max_cnt, eps_q)
    }
  })
}

shinyApp(ui, server)
