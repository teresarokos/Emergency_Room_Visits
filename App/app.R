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
app_data <- read_rds("../app_data")
mutate(app_data, tot_exp_for_event_erfxp16x_erdxp16x = tot_exp_for_event_erfxp16x_erdxp16x + 0.1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # MY TITLE
  titlePanel("MEPS Emergency Visits Data"),
  
  # MY SIDEBAR
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "service",
                  label = "Select a service:",
                  choices = c("Lab Tests" = "this_visit_did_p_have_lab_tests",
                              "Sonogram or Ultrasound" = "this_visit_did_p_have_sonogram_or_ultrsd",
                              "X-Rays" = "this_visit_did_p_have_x_rays",
                              "Mammogram" = "this_visit_did_p_have_a_mammogram",
                              "MRI or CTscan" = "this_visit_did_p_have_an_mri_catscan",
                              "EKG or ECG" = "this_visit_did_p_have_an_ekg_or_ecg", 
                              "EEG" = "this_visit_did_p_have_an_eeg", 
                              "Vaccination" = "this_visit_did_p_receive_a_vaccination", 
                              "Anesthesia" = "this_visit_did_p_receive_anesthesia", 
                              "Throat Swab" = "this_visit_did_p_have_a_throat_swab",
                              "Other Diagnostic Test/Exam" = "this_visit_did_p_have_oth_diag_test_exam", 
                              "Surgery" = "was_surg_proc_performed_on_p_this_visit", 
                              "Medication Prescribed" = "any_medicine_prescribed_for_p_this_visit"))
    ),
    
    # MY MAIN PANEL
    mainPanel(
      plotOutput("histPlot"),
      uiOutput(outputId = "n")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  service_subset <- reactive({
    req(input$service)
    filter(app_data, input$service == "1 YES")
  })
  
  service_subset_test <- filter(app_data, this_visit_did_p_have_lab_tests == "1 YES")
  
  output$histPlot <- renderPlot({
    
    # MY PLOT
    
    ggplot(data = service_subset_test, aes(x = tot_exp_for_event_erfxp16x_erdxp16x)) + 
      geom_histogram() + 
      scale_x_continuous(name = "Total Expenditure for Event", 
                         breaks = c(100, 1000, 10000, 100000),
                         labels = c('$100', '$1,000', '$10,000', '$100,000'),
                         trans = 'log10') +
      scale_y_continuous(name = "Number of Visits", limits = c(0,500)) +
      expand_limits(x = c(0.1, 150000))
  })
  
  output$n <- renderUI({
    paste0("There were a total of ", nrow(service_subset_test), " emergency visits which provided this service")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
