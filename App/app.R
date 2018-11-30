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
library(ggplot2)
library(plotly)

# Loading emergency visits data
app_data <- read_rds("app_data")

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring the MEPS Emergency Room Visits Data",
                 
                 tabPanel("Page 1",
                          fluidPage(
                            titlePanel("Page 1", windowTitle = "Page 1"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("condition", 
                                            "Select a condition",
                                            choices = c("All",
                                                        "Other injuries and conditions due to external causes", 
                                                        "Spondylosis; intervertebral disc disorders; other back problems",
                                                        "Essential hypertension",
                                                        "Other upper respiratory infections",
                                                        "Other upper respiratory disease",
                                                        "Asthma",
                                                        "Intestinal infection",
                                                        "Joint disorders and dislocations; trauma-related",
                                                        "Chronic obstructive pulmonary disease and bronchiectasis",
                                                        "Anxiety disorder"))
                              ),
                              mainPanel(
                                h1(uiOutput(outputId = "n_visits")),
                                uiOutput(outputId = "n_visits_condition"),
                                hr(),
                                h2(p("Services")),
                                plotOutput(outputId = "agg_services_plot", width = "600px", height = "400px"),
                                hr()
                              )
                            )
                          )),
                 
                 tabPanel("Page 2",
                          fluidPage(
                            titlePanel("Page 2"),
                            sidebarLayout(
                              sidebarPanel(),
                              mainPanel()
                            )
                          )),
                 
                 tabPanel("Page 3",
                          fluidPage(
                            titlePanel("Page 3"),
                            sidebarLayout(
                              sidebarPanel(),
                              mainPanel()
                            )
                          )),
                 
                 tabPanel("Page 4",
                          fluidPage(
                            titlePanel("Page 4"),
                            sidebarLayout(
                              sidebarPanel(),
                              mainPanel()
                            )
                          ))
                 )

# Define server logic required to create each page
server <- function(input, output) {
  
  #### PAGE 1 ####
  # Text that shows the total number of visits
  output$n_visits <- renderText({
    paste(nrow(app_data), "total visits")
  })
  
    output$n_visits_condition <- renderText({
      if (input$condition != "All") {
        n_visits <- app_data %>% 
        filter(condition == input$condition)
      paste(nrow(n_visits), "visits classified with this condition (", input$condition, ")")
      }
  })
  
  # Bar plot that shows popularity of services overall
  output$agg_services_plot <- renderPlot({
    aggregate_services <- emergency_visits_data %>% 
      filter(`Lab Tests` == "1 YES") %>% 
      summarize(service = "Lab Tests", n_visits = n())
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Sonogram or Ultrasound` == "1 YES") %>% 
      summarize(service = "Sonogram or Ultrasound", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`X-Rays` == "1 YES") %>% 
      summarize(service = "X-Rays", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Mammogram == "1 YES") %>% 
      summarize(service = "Mammogram", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`MRI or CT Scan` == "1 YES") %>% 
      summarize(service = "MRI or CT Scan", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`EKG or ECG` == "1 YES") %>% 
      summarize(service = "EKG or ECG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`EEG` == "1 YES") %>% 
      summarize(service = "EEG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Vaccination == "1 YES") %>% 
      summarize(service = "Vaccination", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Anesthesia == "1 YES") %>% 
      summarize(service = "Anesthesia", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Throat Swab` == "1 YES") %>% 
      summarize(service = "Throat Swab", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Other Diagnostic Test/Exam` == "1 YES") %>% 
      summarize(service = "Other Diagnostic Test/Exam", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Surgery == "1 YES") %>% 
      summarize(service = "Surgery", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Medicine Prescribed` == "1 YES") %>% 
      summarize(service = "Medicine Prescribed", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    if (input$condition != "All") {
    aggregate_services <- emergency_visits_data %>% 
      filter(`Lab Tests` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Lab Tests", n_visits = n())
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Sonogram or Ultrasound` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Sonogram or Ultrasound", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`X-Rays` == "1 YES", condition == input$condition) %>% 
      summarize(service = "X-Rays", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Mammogram == "1 YES", condition == input$condition) %>% 
      summarize(service = "Mammogram", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`MRI or CT Scan` == "1 YES", condition == input$condition) %>% 
      summarize(service = "MRI or CT Scan", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`EKG or ECG` == "1 YES", condition == input$condition) %>% 
      summarize(service = "EKG or ECG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`EEG` == "1 YES", condition == input$condition) %>% 
      summarize(service = "EEG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Vaccination == "1 YES", condition == input$condition) %>% 
      summarize(service = "Vaccination", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Anesthesia == "1 YES", condition == input$condition) %>% 
      summarize(service = "Anesthesia", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Throat Swab` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Throat Swab", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Other Diagnostic Test/Exam` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Other Diagnostic Test/Exam", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(Surgery == "1 YES", condition == input$condition) %>% 
      summarize(service = "Surgery", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- emergency_visits_data %>% 
      filter(`Medicine Prescribed` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Medicine Prescribed", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    }
    
    aggregate_services %>% 
        ggplot(aes(x = fct_reorder(service, n_visits), y = n_visits)) +
          geom_col() +
          coord_flip() +
          scale_x_discrete(name = "") +
          ylab("Number of visits that provided service") +
          ggtitle("Most Popular Emergency Room Services")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
