#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("SOTU"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "SOU Before Year:",
                     min = 1831,
                     max = 2018,
                     value = 2018)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # read data
     sou <- read_csv("sou.csv")
     presidents <- read_csv("presidents.csv")
      
      # draw the plot
     sou %>%
       left_join(presidents) %>%
       unnest_tokens(word, text) %>% 
       inner_join(get_sentiments("afinn"), by = "word") %>% 
       filter(party %in% c("Democratic", "Republican")) %>%
       filter(year(date) <= input$year) %>%
       group_by(party, date) %>% 
       summarize(rating = mean(score)) %>% 
       ggplot(aes(x = date, y = rating, color = party)) + geom_point() + geom_smooth(method = "loess") +
       xlab("Date") +
       ylab("Average Sentiment Score using AFINN Dictionary") +
       ggtitle("State of the Union Sentiment Scores")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

