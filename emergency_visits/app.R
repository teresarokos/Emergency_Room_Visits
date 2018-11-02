#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
load("/Users/teresarokos/Desktop/GOV 1005/GOV1005-Final_Project/emergency_visits.Rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # MY TITLE
   titlePanel("MEP Emergency Visits Data"),
  
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
        plotOutput("histPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$histPlot <- renderPlot({
     
      # MY PLOT
     services_data <- filter(emergency_visits_data, input$service == "1 YES")
       
        ggplot(data = services_data, aes(x = tot_exp_for_event_erfxp16x_erdxp16x)) + 
        geom_histogram() + 
        scale_x_continuous(name = "Total Expenditure for Event", 
                           breaks = c(100, 500, 1000, 5000, 10000, 50000, 100000),
                           labels = c(100, 500, 1000, 5000, 10000, 50000, 100000),
                           trans = 'log10') +
        scale_y_continuous(name = "Number of Visits", limits = c(0,500)) +
        expand_limits(x = c(0, 150000))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

