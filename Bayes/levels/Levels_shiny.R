library(shiny)

ui <- fluidPage(
  titlePanel("Линии уровня"),
  
  sidebarLayout(
    sidebarPanel(
      
      "Ковариационная матрица",
      fluidRow(
        column(6, textInput("a11", "a11", 1)),
        column(6, textInput("a12", "a12, a21", 0)),
        column(6, NULL),
        column(6, textInput("a22", "a22", 1))
      ),
      
      "Мат. ожидание",
      fluidRow(
        column(6, textInput("nu1", "nu1", 0)),
        column(6, textInput("nu2", "nu2", 0))
      ),
      
      fluidRow(
        column(12, sliderInput("step", "Шаг плотности", 0.001, 0.1, 0.01, 0.01))
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "plot", height = "600px"),
      textOutput("err")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot( { #n-мерное гауссовское распределение
    
    mtx <- matrix(c(as.numeric(input$a11), as.numeric(input$a12), as.numeric(input$a12), as.numeric(input$a22)), 2, 2)
    mu <- c(as.numeric(input$nu1), as.numeric(input$nu2))
    
    a11 <- mtx[2,2]
    a12 <- mtx[1,2]
    a21 <- mtx[2,1]
    a22 <- mtx[1,1]
    
    det = det(mtx)
    
    if (is.nan(det(mtx))) {
      output$err <- renderText("Заполните матрицу ковариации")
      return()
    }
    else if (det(mtx) <= 0) {
      output$err <- renderText("Определитель матрицы <= 0")
      return ()
    }
    if(is.na(mu[1]) || is.na(mu[2])) {
      output$err <- renderText("Заполните вектор мат. ожиданий")
      return ()
    }
    output$err <- renderText("")
    
    A <- a22/det
    B <- a11/det
    C <- (-a12-a21)/det
    D <- (-2*a22*mu[1] + mu[2]*(a12+a21))/det
    E <- (-2*a11*mu[2] + mu[1]*(a12+a21))/det
    F <- (a22*mu[1]^2 + a11*mu[2]^2 - mu[1]*mu[2]*(a12+a21))/det
    
    N <- function(x,y) (1/(sqrt((2*pi)^2*det(mtx))))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
    
    max_x <- a11 + 2
    min_x <- -a11 - 2
    max_y <- a22 + 2
    min_y <- -a22 - 2
    
    x <- seq(min_x, max_x, 8/200)
    y <- seq(min_y, max_y, 8/200)
    z <- outer(x, y, N)
    
    par(pty="s")
    #contour(x, y, z, main=title)
    
    for (i in seq(0, 0.2, input$step)) {
      contour(x, y, z, levels = i, add = add, asp = 1)
      add = T
    }
    
  }
  )
}

shinyApp(ui, server)

  


#draw_lines(c(0,0), matrix(c(1,0,0,1), 2, 2), title="Признаки имеют одинаковые дисперсии")
#draw_lines(c(0,0), matrix(c(1,1,0,1), nrow=2, ncol=2), title="Признаки коррелированы")
#draw_lines(c(0,0), matrix(c(3,0,0,1), nrow=2, ncol=2), title="Признаки некоррелированы")
#draw_lines(c(0,0), matrix(c(1,0,0,3), nrow=2, ncol=2), title="Признаки некоррелированы")