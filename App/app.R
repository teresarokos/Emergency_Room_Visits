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
app_data <- read_rds("app_data")
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
                  choices = c("Lab Tests",
                              "Sonogram or Ultrasound",
                              "X-Rays",
                              "Mammogram",
                              "MRI or CTscan",
                              "EKG or ECG", 
                              "EEG", 
                              "Vaccination", 
                              "Anesthesia", 
                              "Throat Swab",
                              "Other Diagnostic Test/Exam", 
                              "Surgery", 
                              "Prescribed Medication"))
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
  
  # The reactive subset does not appear to be working, so creating a test one for the sake of a graphic.
  service_subset_test <- filter(app_data, this_visit_did_p_have_lab_tests == "1 YES")
  
  # Creating a plot using a subset of the data filtered by the service the user selects.
  output$histPlot <- renderPlot({
    
    # Filtering the data based on the selected service. 
    service_subset <- switch(input$service,
                             "Lab Tests" = app_data %>% filter(this_visit_did_p_have_lab_tests == "1 YES"),
                             "Sonogram or Ultrasound" = app_data %>% filter(this_visit_did_p_have_sonogram_or_ultrsd == "1 YES"),
                             "X-Rays" = app_data %>% filter(this_visit_did_p_have_x_rays == "1 YES"),
                             "Mammogram" = app_data %>% filter(this_visit_did_p_have_a_mammogram == "1 YES"),
                             "MRI or CTscan" = app_data %>% filter(this_visit_did_p_have_an_mri_catscan == "1 YES"),
                             "EKG or ECG" = app_data %>% filter(this_visit_did_p_have_an_ekg_or_ecg == "1 YES"), 
                             "EEG" = app_data %>% filter(this_visit_did_p_have_an_eeg == "1 YES"), 
                             "Vaccination" = app_data %>% filter(this_visit_did_p_receive_a_vaccination == "1 YES"), 
                             "Anesthesia" = app_data %>% filter(this_visit_did_p_receive_anesthesia == "1 YES"), 
                             "Throat Swab" = app_data %>% filter(this_visit_did_p_have_a_throat_swab == "1 YES"),
                             "Other Diagnostic Test/Exam" = app_data %>% filter(this_visit_did_p_have_oth_diag_test_exam == "1 YES"), 
                             "Surgery" = app_data %>% filter(was_surg_proc_performed_on_p_this_visit == "1 YES"), 
                             "Prescribed Medication" = app_data %>% filter(any_medicine_prescribed_for_p_this_visit == "1 YES")
    )
    
    # MY PLOT
    ggplot(data = service_subset, aes(x = tot_exp_for_event_erfxp16x_erdxp16x)) + 
      geom_histogram(bins = 30) + 
      scale_x_continuous(name = "Total Expenditure for Event", 
                         breaks = c(100, 1000, 10000, 100000),
                         labels = c('$100', '$1,000', '$10,000', '$100,000'),
                         trans = 'log10') +
      scale_y_continuous(name = "Number of Visits", limits = c(0,500)) +
      expand_limits(x = c(0.1, 150000)) +
      ggtitle("Distribution of expenditure by service")
  })
  
  # This demonstrates whether or not my data is reading in properly (the reactive subset does not work).
  output$n <- renderUI({
    service_subset <- switch(input$service,
                             "Lab Tests" = app_data %>% filter(this_visit_did_p_have_lab_tests == "1 YES"),
                             "Sonogram or Ultrasound" = app_data %>% filter(this_visit_did_p_have_sonogram_or_ultrsd == "1 YES"),
                             "X-Rays" = app_data %>% filter(this_visit_did_p_have_x_rays == "1 YES"),
                             "Mammogram" = app_data %>% filter(this_visit_did_p_have_a_mammogram == "1 YES"),
                             "MRI or CTscan" = app_data %>% filter(this_visit_did_p_have_an_mri_catscan == "1 YES"),
                             "EKG or ECG" = app_data %>% filter(this_visit_did_p_have_an_ekg_or_ecg == "1 YES"), 
                             "EEG" = app_data %>% filter(this_visit_did_p_have_an_eeg == "1 YES"), 
                             "Vaccination" = app_data %>% filter(this_visit_did_p_receive_a_vaccination == "1 YES"), 
                             "Anesthesia" = app_data %>% filter(this_visit_did_p_receive_anesthesia == "1 YES"), 
                             "Throat Swab" = app_data %>% filter(this_visit_did_p_have_a_throat_swab == "1 YES"),
                             "Other Diagnostic Test/Exam" = app_data %>% filter(this_visit_did_p_have_oth_diag_test_exam == "1 YES"), 
                             "Surgery" = app_data %>% filter(was_surg_proc_performed_on_p_this_visit == "1 YES"), 
                             "Prescribed Medication" = app_data %>% filter(any_medicine_prescribed_for_p_this_visit == "1 YES")
    )
    
    paste0("There were a total of ", nrow(service_subset), " emergency visits surveyed that provided this service.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
