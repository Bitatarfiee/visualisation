# Creating list of column names
colname <- list("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11")             
#Reading data
data1 <- read.table("SENIC.txt", 
                    header = FALSE,col.names = colname)

data <- data1[,-c(8:9)]

# part2
Quantiles <- function(x){
  d <- data[,x]
  Q1 <- quantile(d, probs = 0.25)
  Q3 <- quantile(d, probs = 0.75)
  output <- which(d > Q3+1.5*(Q3-Q1) | d < Q1-1.5*(Q3-Q1))
  return(output)
}
outliers <- Quantiles(4)
outliers

# part3
library(ggplot2)

# Get outlier indices
outliers <- Quantiles(4)

plot <- ggplot(data, aes(x = X3)) +
  geom_density(fill = "#E7B800", color = "#E7B800") +
  geom_point(data = data[outliers, ], aes(x = X3, y = 0), shape = 18, color = "blue", size = 3) +
  labs(x = "Infection Risk", y = "Density")
plot

# part4
library(gridExtra)
library(ggplot2)


# selecting the list of quantitative columns (X1 to X11 except X7 and X8)
cols <- c(2:10)

# Create a list to store the plots
plots <- list()

# Loop through each quantitative variable
for (i in cols) {
  # Get outlier indices
  outliers <- Quantiles(i)
  
  # Create density plot
  p <- ggplot(data, aes_string(x = colnames(data)[i])) +
    geom_density(fill = "#E7B800", color = "#E7B800") +
    geom_point(data = data[outliers, ], 
               aes_string(x = colnames(data)[i], y = 0), 
               shape = 18, color = "blue", size = 3) + 
    labs(x = colnames(data)[i], y = "Density")
  
  plots[[i-1]] <- p
}

# Arrange plots in a grid
grid.arrange(arrangeGrob(grobs = plots, ncol = 3))

# part5
p <- ggplot(data, aes(x=X10, y=X3, color=X6))+ 
  geom_point()+
  labs( y="Infection Risk", x="Number of Nurses", color="Number of Beds")+
  ggtitle("Dependence of Infection risk\non the Number of Nurses and Number of Beds") +
  theme(plot.title = element_text(hjust = 0.5))
p

# part6
library(plotly)
ggplotly(plot)


# Part 7
library(dplyr)
library(plotly)

fig <- data%>%plot_ly(x=~X3, type = "histogram")%>%
  add_trace(data=data %>%
              select(X3)%>%
              filter(is.element(X3, X3[outliers])),
            x = ~X3, y =0, type = "scatter", mode = "markers",marker = list(symbol = "diamond", size = 10), color=I("red"))
fig



# Part 8
library(shiny)
library(gridExtra)
library(ggplot2)

colname <- list("ID", "X1", "X2", "X3","X4","X5","X6","X9",
                "X10","X11")
col<-colname[-1]

ui <- fluidPage(
  sliderInput(inputId="ws", label="Choose bandwidth size", value=1, min=1, max=35),
  plotOutput("densPlot"),
  checkboxGroupInput(inputId = "variables", label = "Select variables:", 
                     choices = col, selected =col[1:9])
)

server <- function(input, output) {
  
  output$densPlot <- renderPlot({
    
    # Create a list to store the plots
    # Loop through each quantitative variable
    # Get outlier indices
    
    library(gridExtra)
    
    # selecting the list of quantitative columns (X1 to X11)
    cols <- which(colname %in% input$variables )
    
    
    # Create a list to store the plots
    plots <- list()
    
    # Loop through each quantitative variable
    for (i in cols) {
      # Get outlier indices
      outliers <- Quantiles(i)
      
      bandwidth <- as.numeric(input$ws)
      # Create density plot
      p <- ggplot(data, aes_string(x = colnames(data)[i])) +
        geom_density(fill = "#E7B800", color = "#E7B800", bw=bandwidth, position="identity") +
        geom_point(data = data[outliers, ], aes_string(x = colnames(data)[i], y = 0), shape = 18, color = "blue", size = 3) +
        labs(x = colnames(data)[i], y = "Density")
      
      plots[[i-1]] <- p
    }
    
    # Arrange plots in a grid
    grid.arrange(arrangeGrob(grobs = plots, ncol = 3))
    
  })
}

# Run the application 
shinyApp(ui = ui, server=server)