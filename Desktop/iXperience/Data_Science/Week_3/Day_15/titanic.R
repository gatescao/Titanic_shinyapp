library(vcdExtra)
library(shiny)
library(rpart)
library(caret)
library(dplyr)

#Build model
fit.titanic <- rpart(survived ~ ., data = Titanicp)

ui <- fluidPage(
   titlePanel("Titanic Survival Prediction"),
   sidebarLayout(
     sidebarPanel(
       radioButtons("gender", 
                   label = "Gender",
                   choices = list("Male" = "male", "Female" = "female"),
                   selected = "male"),
       selectInput("pclass", 
                  label = "Class",
                  choices = list("First" = "1st", "Second" = "2nd", "Third" = "3rd"),
                  selected = "3rd"),
       sliderInput("age",
                  "Age",
                  min = min(Titanicp$age, na.rm = TRUE),
                  max = max(Titanicp$age, na.rm = TRUE),
                  step = 0.5,
                  value = 15),
       sliderInput("sibsp",
                   "Number of siblings and spouse",
                   min = min(Titanicp$sibsp, na.rm = TRUE),
                   max = max(Titanicp$sibsp, na.rm = TRUE),
                   step = 1,
                   value = 1),
       sliderInput("parch",
                   "Number of parents and children",
                   min = min(Titanicp$parch, na.rm = TRUE),
                   max = max(Titanicp$parch, na.rm = TRUE),
                   step = 1,
                   value = 1)
     ),
     mainPanel(
       textOutput("prediction"),
       plotOutput("tree")
     )
   )
)

server <- function(input, output) {
  output$prediction <- renderText({
    new_data = data.frame(
      pclass = input$pclass,
      sex = input$gender,
      age = input$age,
      sibsp = input$sibsp,
      parch = input$parch
    )
    titanic.predict <- predict(fit.titanic, new_data, type = "class")
    paste("This passenger", titanic.predict, ".", sep = " ")
  })
  output$tree <- renderPlot({
    plot(fit.titanic)
    text(fit.titanic)
    #fancyRpartPlot(titanic.predict)
  })
}

shinyApp(ui = ui, server = server)
