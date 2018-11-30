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
app_data <- read_rds("app_data")
aggregate_services <- read_rds("aggregate_services")
mutate(app_data, tot_exp_for_event_erfxp16x_erdxp16x = tot_exp_for_event_erfxp16x_erdxp16x + 0.1)

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring the MEPS Emergency Room Visits Data",
                 
                 tabPanel("Page 1",
                          fluidPage(
                            titlePanel("Page 1"),
                            sidebarLayout(
                              sidebarPanel(),
                              mainPanel(
                                h1(uiOutput(outputId = "n_visits")),
                                h2(p("Services")),
                                plotOutput(outputId = "agg_services_plot", width = "600px", height = "400px")
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

#   fluidPage(
#   
#   # MY TITLE
#   titlePanel("Exploring MEPS Emergency Visits Data"),
#   
#   # MY SIDEBAR
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(inputId = "service",
#                   label = "Select a service:",
#                   choices = c("Lab Tests",
#                               "Sonogram or Ultrasound",
#                               "X-Rays",
#                               "Mammogram",
#                               "MRI or CTscan",
#                               "EKG or ECG", 
#                               "EEG", 
#                               "Vaccination", 
#                               "Anesthesia", 
#                               "Throat Swab",
#                               "Other Diagnostic Test/Exam", 
#                               "Surgery", 
#                               "Prescribed Medication")),
#       
#         selectInput(inputId = "condition",
#                     label = "Select a condition:",
#                     choices = c("Other injuries and conditions due to external causes", 
#                                 "Spondylosis; intervertebral disc disorders; other back problems",
#                                 "Essential hypertension",
#                                 "Other upper respiratory infections",
#                                 "Other upper respiratory disease",
#                                 "Asthma",
#                                 "Intestinal infection",
#                                 "Joint disorders and dislocations; trauma-related",
#                                 "Chronic obstructive pulmonary disease and bronchiectasis",
#                                 "Anxiety disorder"))
#     ),
#     
#     # MY MAIN PANEL
#     mainPanel(
#       plotOutput("servicesPlot"),
#       plotOutput("conditionPlot"),
#       uiOutput(outputId = "n_services"),
#       uiOutput(outputId = "n_condition")
#     )
#   )
# )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # PAGE 1
  output$n_visits <- renderText({
    paste(nrow(app_data), "total visits")
  })
  
  output$agg_services_plot <- renderPlot({
    aggregate_services %>% 
        ggplot(aes(x = fct_reorder(service, n_visits), y = n_visits)) +
          geom_col() +
          coord_flip() +
          scale_x_discrete(name = "", labels = c("Mammogram", "Vaccination", "EEG", "Anesthesia",
                                      "Throat Swab", "Surgery", "Sonogram or Ultrasound",
                                      "Other Diagnostic Test or Exam", "MRI or CTscan", "EKG or ECG",
                                      "X-Rays", "Medicine Prescribed", "Lab Tests")) +
          ylab("Number of visits that provided service") +
          ggtitle("Most Popular Emergency Room Services")
  })
  
  # # The reactive subset does not appear to be working, so creating a test one for the sake of a graphic.
  # service_subset_test <- filter(app_data, this_visit_did_p_have_lab_tests == "1 YES")
  # 
  # # Creating a plot using a subset of the data filtered by the service the user selects.
  # output$servicesPlot <- renderPlot({
  #   
  #   # Filtering the data based on the selected service. 
  #   service_subset <- switch(input$service,
  #                            "Lab Tests" = app_data %>% filter(this_visit_did_p_have_lab_tests == "1 YES"),
  #                            "Sonogram or Ultrasound" = app_data %>% filter(this_visit_did_p_have_sonogram_or_ultrsd == "1 YES"),
  #                            "X-Rays" = app_data %>% filter(this_visit_did_p_have_x_rays == "1 YES"),
  #                            "Mammogram" = app_data %>% filter(this_visit_did_p_have_a_mammogram == "1 YES"),
  #                            "MRI or CTscan" = app_data %>% filter(this_visit_did_p_have_an_mri_catscan == "1 YES"),
  #                            "EKG or ECG" = app_data %>% filter(this_visit_did_p_have_an_ekg_or_ecg == "1 YES"), 
  #                            "EEG" = app_data %>% filter(this_visit_did_p_have_an_eeg == "1 YES"), 
  #                            "Vaccination" = app_data %>% filter(this_visit_did_p_receive_a_vaccination == "1 YES"), 
  #                            "Anesthesia" = app_data %>% filter(this_visit_did_p_receive_anesthesia == "1 YES"), 
  #                            "Throat Swab" = app_data %>% filter(this_visit_did_p_have_a_throat_swab == "1 YES"),
  #                            "Other Diagnostic Test/Exam" = app_data %>% filter(this_visit_did_p_have_oth_diag_test_exam == "1 YES"), 
  #                            "Surgery" = app_data %>% filter(was_surg_proc_performed_on_p_this_visit == "1 YES"), 
  #                            "Prescribed Medication" = app_data %>% filter(any_medicine_prescribed_for_p_this_visit == "1 YES")
  #   )
  #   
  #   # MY PLOT
  #   ggplot(data = service_subset, aes(x = tot_exp_for_event_erfxp16x_erdxp16x)) + 
  #     geom_histogram(bins = 30) + 
  #     scale_x_continuous(name = "Total Expenditure for Event", 
  #                        breaks = c(100, 1000, 10000, 100000),
  #                        labels = c('$100', '$1,000', '$10,000', '$100,000'),
  #                        trans = 'log10') +
  #     scale_y_continuous(name = "Number of Visits", limits = c(0,500)) +
  #     expand_limits(x = c(0.1, 150000)) +
  #     ggtitle("Distribution of expenditure by service")
  # })
  # 
  # # Creating an interactive histogram plot of expenditure, services, and conditions
  # output$conditionPlot <- renderPlot({
  #   service_subset <- switch(input$service,
  #                            "Lab Tests" = app_data %>% filter(this_visit_did_p_have_lab_tests == "1 YES"),
  #                            "Sonogram or Ultrasound" = app_data %>% filter(this_visit_did_p_have_sonogram_or_ultrsd == "1 YES"),
  #                            "X-Rays" = app_data %>% filter(this_visit_did_p_have_x_rays == "1 YES"),
  #                            "Mammogram" = app_data %>% filter(this_visit_did_p_have_a_mammogram == "1 YES"),
  #                            "MRI or CTscan" = app_data %>% filter(this_visit_did_p_have_an_mri_catscan == "1 YES"),
  #                            "EKG or ECG" = app_data %>% filter(this_visit_did_p_have_an_ekg_or_ecg == "1 YES"), 
  #                            "EEG" = app_data %>% filter(this_visit_did_p_have_an_eeg == "1 YES"), 
  #                            "Vaccination" = app_data %>% filter(this_visit_did_p_receive_a_vaccination == "1 YES"), 
  #                            "Anesthesia" = app_data %>% filter(this_visit_did_p_receive_anesthesia == "1 YES"), 
  #                            "Throat Swab" = app_data %>% filter(this_visit_did_p_have_a_throat_swab == "1 YES"),
  #                            "Other Diagnostic Test/Exam" = app_data %>% filter(this_visit_did_p_have_oth_diag_test_exam == "1 YES"), 
  #                            "Surgery" = app_data %>% filter(was_surg_proc_performed_on_p_this_visit == "1 YES"), 
  #                            "Prescribed Medication" = app_data %>% filter(any_medicine_prescribed_for_p_this_visit == "1 YES")
  #   )
  #   
  #   condition_subset <- service_subset %>% 
  #     filter(condition == input$condition) %>% 
  #     mutate(expensiveness = rank(tot_exp_for_event_erfxp16x_erdxp16x))
  #   
  #   ggplot(data = condition_subset, aes(x = tot_exp_for_event_erfxp16x_erdxp16x, y = expensiveness)) + 
  #     geom_point() + 
  #     scale_x_continuous(name = "Total Expenditure for Event", 
  #                        breaks = c(100, 1000, 10000, 100000),
  #                        labels = c('$100', '$1,000', '$10,000', '$100,000'),
  #                        trans = 'log10') +
  #     scale_y_continuous(name = "Number of Visits", limits = c(0, 200)) +
  #     expand_limits(x = c(0.1, 100000)) +
  #     ggtitle("Distribution of expenditure by condition")
  # })
  # 
  # # This demonstrates whether or not my data is reading in properly (the reactive subset does not work).
  # output$n_services <- renderUI({
  #   service_subset <- switch(input$service,
  #                            "Lab Tests" = app_data %>% filter(this_visit_did_p_have_lab_tests == "1 YES"),
  #                            "Sonogram or Ultrasound" = app_data %>% filter(this_visit_did_p_have_sonogram_or_ultrsd == "1 YES"),
  #                            "X-Rays" = app_data %>% filter(this_visit_did_p_have_x_rays == "1 YES"),
  #                            "Mammogram" = app_data %>% filter(this_visit_did_p_have_a_mammogram == "1 YES"),
  #                            "MRI or CTscan" = app_data %>% filter(this_visit_did_p_have_an_mri_catscan == "1 YES"),
  #                            "EKG or ECG" = app_data %>% filter(this_visit_did_p_have_an_ekg_or_ecg == "1 YES"), 
  #                            "EEG" = app_data %>% filter(this_visit_did_p_have_an_eeg == "1 YES"), 
  #                            "Vaccination" = app_data %>% filter(this_visit_did_p_receive_a_vaccination == "1 YES"), 
  #                            "Anesthesia" = app_data %>% filter(this_visit_did_p_receive_anesthesia == "1 YES"), 
  #                            "Throat Swab" = app_data %>% filter(this_visit_did_p_have_a_throat_swab == "1 YES"),
  #                            "Other Diagnostic Test/Exam" = app_data %>% filter(this_visit_did_p_have_oth_diag_test_exam == "1 YES"), 
  #                            "Surgery" = app_data %>% filter(was_surg_proc_performed_on_p_this_visit == "1 YES"), 
  #                            "Prescribed Medication" = app_data %>% filter(any_medicine_prescribed_for_p_this_visit == "1 YES")
  #   )
  #   
  #   paste0("There were a total of ", nrow(service_subset), " emergency visits surveyed that provided this service.")
  # })
  # 
  # output$n_condition <- renderUI({
  #   service_subset <- switch(input$service,
  #                            "Lab Tests" = app_data %>% filter(this_visit_did_p_have_lab_tests == "1 YES"),
  #                            "Sonogram or Ultrasound" = app_data %>% filter(this_visit_did_p_have_sonogram_or_ultrsd == "1 YES"),
  #                            "X-Rays" = app_data %>% filter(this_visit_did_p_have_x_rays == "1 YES"),
  #                            "Mammogram" = app_data %>% filter(this_visit_did_p_have_a_mammogram == "1 YES"),
  #                            "MRI or CTscan" = app_data %>% filter(this_visit_did_p_have_an_mri_catscan == "1 YES"),
  #                            "EKG or ECG" = app_data %>% filter(this_visit_did_p_have_an_ekg_or_ecg == "1 YES"), 
  #                            "EEG" = app_data %>% filter(this_visit_did_p_have_an_eeg == "1 YES"), 
  #                            "Vaccination" = app_data %>% filter(this_visit_did_p_receive_a_vaccination == "1 YES"), 
  #                            "Anesthesia" = app_data %>% filter(this_visit_did_p_receive_anesthesia == "1 YES"), 
  #                            "Throat Swab" = app_data %>% filter(this_visit_did_p_have_a_throat_swab == "1 YES"),
  #                            "Other Diagnostic Test/Exam" = app_data %>% filter(this_visit_did_p_have_oth_diag_test_exam == "1 YES"), 
  #                            "Surgery" = app_data %>% filter(was_surg_proc_performed_on_p_this_visit == "1 YES"), 
  #                            "Prescribed Medication" = app_data %>% filter(any_medicine_prescribed_for_p_this_visit == "1 YES")
  #   )
  #   
  #   condition_subset <- service_subset %>% 
  #     filter(condition == input$condition)
  #   
  #   paste0("There were a total of ", nrow(condition_subset), " emergency visits surveyed where patients required this service and had ", input$condition, ".")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
