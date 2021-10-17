library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)

load("main.RData")

ui <- fluidPage(

    titlePanel("Feed Additives Register"),

    sidebarLayout(
        sidebarPanel(
            radioButtons('filter', "Filter by",
                              choices = c("None", "category", "functional group", 
                                          "subclassification"), selected = "None"),
            conditionalPanel(condition = 'input.filter=="category"',
                             selectizeInput('category', "Select category",
                                         choices = NULL)),
            conditionalPanel(condition = 'input.filter=="functional group"',
                             selectizeInput('f_group', "Select functional group",
                                            choices = NULL)),
            conditionalPanel(condition = 'input.filter=="subclassification"',
                             selectizeInput('class', "Select subclassification",
                                            choices = NULL)),
            searchInput('name', "Search by name")
        ),

        mainPanel(
            DTOutput('main_table')
        )
    )
)

server <- function(input, output, session) {
    
    updateSelectizeInput(session, 'category', selected = "",
                         choices = unique(data$Category),
                         server = T)
    
    updateSelectizeInput(session, 'f_group', selected = "",
                         choices = unique(data$Functional_Group),
                         server = T)
    
    updateSelectizeInput(session, 'class', selected = "",
                         choices = unique(data$Subclassification),
                         server = T)
    
    rval_data <- reactive({
        main_data <- data
        if (input$filter == "category") {
            if (input$category != "") {
                main_data <- main_data %>%
                    filter(Category == input$category)
            }
        } else if (input$filter == "functional group") {
            if (input$f_group != "") {
                main_data <- main_data %>%
                    filter(Functional_Group == input$f_group)
            }
        } else if (input$filter == "subclassification") {
            if (input$class != "") {
                main_data <- main_data %>%
                    filter(Subclassification == input$class)
            }
        }
        main_data
    })
    
    rval_name <- reactive({
        main_data <- rval_data()
        sub_data <- main_data %>%
            filter(str_detect(main_data$Additive, input$name))
        sub_data
    })
    
    output$main_table <- renderDT({
        final_data <- rval_name()
        final_data
    })
}

shinyApp(ui = ui, server = server)
