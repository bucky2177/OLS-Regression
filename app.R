###############
# Autor: bucky
# Description: shows the OLS-Minimization in Regression
###############


######### Ab hier nur noch Berechnungen und Plots #########
list.of.packages <- c("plotly")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
library(plotly)

generateSample = function(seed, n){
  set.seed(seed)
  es = rnorm(n)
  x = runif(n, -5, 5)
  y = 1 * x + 1.5 + es
  return(data.frame(x = x, y = y))
}

plotRegression = function(x, y, a, b, boolReg, boolRes) {
  plot(y ~ x, ylim = c(-6, 10), xlim = c(-6, 6), pch = 21, bg = "skyblue",
       main = "Sample & Regression")
  
  if(boolReg)
    abline(coef = c(a,b))

  if(boolRes){
    predicts = a + b*x
    segments(x0 = x, y0 = y, x1 = x, y1 = predicts)
  }
}


plotSurface = function(a, b, x, y) {
  as = seq(-6, 6, length.out = 50)
  bs = seq(-3, 3, length.out = 50)

  
  
  Q = function(oneA, oneB, x, y) { # OLS-Loss function
    out = sum((y - (oneA + oneB*x) )^2)
  }
  Q = Vectorize(Q, vectorize.args = c("oneA", "oneB"))
  test = outer(as, bs, FUN = Q, x = x, y = y) # rows ==> as, columns ==> bs
  
  fig <- plot_ly(z = ~test, y =  ~ as, x =  ~bs) %>%
    add_surface(showscale  = F) %>% 
    add_trace(x = b, y = a, z = Q(a,b,x,y), mode = "markers", type = "scatter3d", 
              marker = list(color = "red"), showlegend = F) %>% 
    layout(scene = list(xaxis=list(title = "b"),
                        yaxis = list(title = "a"),
                        zaxis = list(title = "Q")),
           title = "Loss function")
  return(fig)
}



################## Shiny Implementierung


ui <- fluidPage(
  titlePanel(
    "Visualization of the Least-Squares Criterion"
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "seed",
        h4("The seed for a sample"),
        min = 1,
        max = 50,
        value = 5,
        step = 1
      ),
      
      sliderInput(
        "n",
        h4("The sample size"),
        min = 10,
        max = 50,
        value = 20,
        step = 10
      ),
      radioButtons('boolReg',
                   'Show Regression?',
                   c('No' = FALSE,
                     'Yes' = TRUE)),
      radioButtons('boolRes',
                   'Show Residuals?',
                   c('No' = FALSE,
                     'Yes' = TRUE)),
      sliderInput(
        "a",
        h4("Intercept"),
        min = -6,
        max = 6,
        value = -3,
        step = 1
      ),
      sliderInput(
        "b",
        h4("Slope"),
        min = -3,
        max = 3,
        value = -2,
        step = 0.1
      ), width = 2
    ),
    
    mainPanel(fluidPage(
      
        fluidRow(splitLayout(
          cellWidths = c("30%", "70%"),
          plotOutput(outputId = "sample"),
          plotlyOutput(outputId = "loss", height = "600px")
        ))
      ), width = 10
    )
  )
)




server <- function(input, output) {
  
  output$sample <- renderPlot({
    dat = generateSample(seed = input$seed, n = input$n)
    plotRegression(x = dat$x, y = dat$y, a = input$a, b = input$b, boolReg = input$boolReg, boolRes = input$boolRes)
  })
  
  output$loss <- renderPlotly({
    dat = generateSample(seed = input$seed, n = input$n)
    plot1 <- plotSurface(x = dat$x, y = dat$y, a = input$a, b = input$b)
  })
}

shinyApp(ui = ui, server = server)
