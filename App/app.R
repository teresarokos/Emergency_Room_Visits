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
                 
                 tabPanel("Services",
                          fluidPage(
                            titlePanel("Emergency Room Services", windowTitle = "Services"),
                            sidebarLayout(
                              sidebarPanel(
                                p("Here you can explore which services tend to be provided at emergency rooms 
                                  across America by the type of care and condition."),
                        
                                br(),
                                
                                checkboxGroupInput("care_category", 
                                                   "Select categories of care:",
                                                   choices = c("Diagnosis or treatment" = "1 DIAGNOSIS OR TREATMENT",
                                                               "Emergency (e.g. accident or injury)" = "2 EMERGENCY (E.G., ACCIDENT OR INJURY)",
                                                               "Phychotherapy/mental health counseling" = "3 PSYCHOTHERAPY/MENTAL HEALTH COUNSELING",
                                                               "Follow-up or post-operative visit" = "4 FOLLOW-UP OR POST-OPERATIVE VISIT",
                                                               "Immunizations or shots" = "5 IMMUNIZATIONS OR SHOTS",
                                                               "Pregnancy-related" = "6 PREGNANCY-RELATED (INC PRENATAL/ DELV)",
                                                               "Other" = "91 OTHER",
                                                               "Not ascertained" = "-9 NOT ASCERTAINED"),
                                                   selected = "2 EMERGENCY (E.G., ACCIDENT OR INJURY)"),
                                
                                selectInput("condition", 
                                            "Select a condition:",
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
                                plotOutput(outputId = "agg_services_plot"),
                                hr(),
                                h2(p("Expenditures")),
                                plotOutput(outputId = "agg_expenditure_plot"),
                                hr()
                              )
                            )
                          )),
                 
                 tabPanel("Patients",
                          fluidPage(
                            titlePanel("Patients"),
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
  
#### SERVICES PAGE ####
  # Text that shows the total number of visits in the selected care categories
  output$n_visits <- renderText({
    app_data <- app_data %>% 
      filter(`Category of Care` %in% c(input$care_category))
    paste(nrow(app_data), "total visits")
  })
  
  # Text that shows the number of visits which had the selected condition
    output$n_visits_condition <- renderText({
      if (input$condition != "All") {
        n_visits <- app_data %>% 
          filter(`Category of Care` %in% c(input$care_category), condition == input$condition)
        paste(nrow(n_visits), "visits with ", str_to_lower(input$condition))
        }

  })
  
  # Bar plot that shows popularity of services overall. First must create summary statistics of services used
  output$agg_services_plot <- renderPlot({
    app_data <- app_data %>% 
      filter(`Category of Care` %in% c(input$care_category))
    
    aggregate_services <- app_data %>% 
      filter(`Lab Tests` == "1 YES") %>% 
      summarize(service = "Lab Tests", n_visits = n())
    
    aggregate_services <- app_data %>% 
      filter(`Sonogram or Ultrasound` == "1 YES") %>% 
      summarize(service = "Sonogram or Ultrasound", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`X-Rays` == "1 YES") %>% 
      summarize(service = "X-Rays", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Mammogram == "1 YES") %>% 
      summarize(service = "Mammogram", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`MRI or CT Scan` == "1 YES") %>% 
      summarize(service = "MRI or CT Scan", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`EKG or ECG` == "1 YES") %>% 
      summarize(service = "EKG or ECG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`EEG` == "1 YES") %>% 
      summarize(service = "EEG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Vaccination == "1 YES") %>% 
      summarize(service = "Vaccination", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Anesthesia == "1 YES") %>% 
      summarize(service = "Anesthesia", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`Throat Swab` == "1 YES") %>% 
      summarize(service = "Throat Swab", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`Other Diagnostic Test/Exam` == "1 YES") %>% 
      summarize(service = "Other Diagnostic Test/Exam", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Surgery == "1 YES") %>% 
      summarize(service = "Surgery", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`Medicine Prescribed` == "1 YES") %>% 
      summarize(service = "Medicine Prescribed", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    if (input$condition != "All") {
    aggregate_services <- app_data %>% 
      filter(`Lab Tests` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Lab Tests", n_visits = n())
    
    aggregate_services <- app_data %>% 
      filter(`Sonogram or Ultrasound` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Sonogram or Ultrasound", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`X-Rays` == "1 YES", condition == input$condition) %>% 
      summarize(service = "X-Rays", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Mammogram == "1 YES", condition == input$condition) %>% 
      summarize(service = "Mammogram", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`MRI or CT Scan` == "1 YES", condition == input$condition) %>% 
      summarize(service = "MRI or CT Scan", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`EKG or ECG` == "1 YES", condition == input$condition) %>% 
      summarize(service = "EKG or ECG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`EEG` == "1 YES", condition == input$condition) %>% 
      summarize(service = "EEG", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Vaccination == "1 YES", condition == input$condition) %>% 
      summarize(service = "Vaccination", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Anesthesia == "1 YES", condition == input$condition) %>% 
      summarize(service = "Anesthesia", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`Throat Swab` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Throat Swab", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`Other Diagnostic Test/Exam` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Other Diagnostic Test/Exam", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(Surgery == "1 YES", condition == input$condition) %>% 
      summarize(service = "Surgery", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    
    aggregate_services <- app_data %>% 
      filter(`Medicine Prescribed` == "1 YES", condition == input$condition) %>% 
      summarize(service = "Medicine Prescribed", n_visits = n()) %>% 
      bind_rows(aggregate_services)
    }
    
    aggregate_services %>% 
        ggplot(aes(x = fct_reorder(service, n_visits), y = n_visits, fill = service)) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          scale_x_discrete(name = "") +
          ylab("Number of visits that provided service")
  })
  
  output$agg_expenditure_plot <- renderPlot({
    app_data <- switch(input$condition,
                       "All" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category)),
                       "Other injuries and conditions due to external causes" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Other injuries and conditions due to external causes"), 
                       "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Spondylosis; intervertebral disc disorders; other back problems"),
                       "Essential hypertension" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Essential hypertension"),
                       "Other upper respiratory infections" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Other upper respiratory infections"),
                       "Other upper respiratory disease" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Other upper respiratory disease"),
                       "Asthma" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Asthma"),
                       "Intestinal infection" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Intestinal infection"),
                       "Joint disorders and dislocations; trauma-related" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Joint disorders and dislocations; trauma-related"),
                       "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Chronic obstructive pulmonary disease and bronchiectasis"),
                       "Anxiety disorder" = app_data %>% 
                         filter(`Category of Care` %in% c(input$care_category), condition == "Anxiety disorder"))
    
      app_data %>%
        filter(`Category of Care` %in% c(input$care_category)) %>% 
        select(person_id_duid_pid, `Lab Tests`, `Sonogram or Ultrasound`, `X-Rays`, Mammogram, 
               `MRI or CT Scan`, `EKG or ECG`, EEG, Vaccination, Anesthesia, `Throat Swab`, 
               `Other Diagnostic Test/Exam`, Surgery, `Medicine Prescribed`, `Total Expenditure`, 
               condition, condition_2, condition_3, condition_4, `Total Charge`) %>% 
        gather(key = "service", value = "provided", -`Total Expenditure`, -person_id_duid_pid, 
               -condition, -condition_2, -condition_3, -condition_4) %>% 
        arrange(person_id_duid_pid) %>%  
        filter(provided == "1 YES") %>% 
        ggplot(aes(x = fct_reorder(service, `Total Expenditure`), y = `Total Expenditure`, color = service)) +
        geom_boxplot(show.legend = FALSE) +
        ylab("Total expenditure on events that included service ($)") +
        xlab("") +
        coord_flip()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
