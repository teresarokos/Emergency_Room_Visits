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
library(scales)

# Loading emergency visits data
app_data <- read_rds("app_data")

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring MEPS Emergency Room Visits Data",
                 
                 tabPanel("About",
                          fluidPage(
                            titlePanel("About"),
                            fluidRow(
                                  h3("What is the MEPS emergency room visits data?"),
                                    p("The Medical Expenditure Panel Survey (MEPS) is a set of large-scale 
                                    surveys of families and individuals, their medical providers, and 
                                    employers across the United States. The 2016 Emergency Room (ER) visits data include
                                    information on the health conditions requiring emergency
                                    room care, medical services provided, any surgical procedures performed, 
                                    prescribed medicines, and the physicians and surgeons providing emergency 
                                    room care for any visit the surveyed person made during 2016 to a hospital emergency 
                                    room. ER expenditures in MEPS are defined as the sum of payments for 
                                    care received for each emergency room visit, including out-of-pocket 
                                    payments and payments made by private insurance, Medicaid, Medicare and 
                                    other sources."),
                                    br(),
                                    h3("What does this app let you do?"),
                                    p("This app lets you explore what sort of services are provided by emergency
                                    rooms, what kinds of expenditures emergency visits tend to require, and what
                                    types of conditions, services, and expenditures are associated with
                                    high-use patients.")
                                )
                            )),
                 
                 tabPanel("Services",
                          fluidPage(
                            titlePanel("Emergency Room Services", windowTitle = "Services"),
                            sidebarLayout(
                              sidebarPanel( 
                                p("To see which services are most often provided for a given medical condition,"),
                                selectInput("condition", 
                                            "Select a medical condition:",
                                            choices = c("All",
                                                        "Asthma",
                                                        "Anxiety disorder",
                                                        "Calculus of urinary tract",
                                                        "Chronic obstructive pulmonary disease and bronchiectasis",
                                                        "Essential hypertension",
                                                        "Fracture of upper limb",
                                                        "Headache; including migraine",
                                                        "Intestinal infection",
                                                        "Joint disorders and dislocations; trauma-related",
                                                        "Open wounds of extremities",
                                                        "Other connective tissue disease",
                                                        "Other injuries and conditions due to external causes",
                                                        "Other upper respiratory disease",
                                                        "Other upper respiratory infections",
                                                        "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)",
                                                        "Spondylosis; intervertebral disc disorders; other back problems",
                                                        "Urinary tract infections")),
                                hr(),
                                p("To see which services are most often provided in conjuction with a
                                  particular service (given the condition selected above),"),
                                radioButtons("service", 
                                                   "Select a service (and scroll down):",
                                                   choices = c("Anesthesia",
                                                               "EEG",
                                                               "EKG or ECG", 
                                                               "Lab Tests",
                                                               "Mammogram",
                                                               "Medicine Prescribed",
                                                               "MRI or CT Scan",
                                                               "Other Diagnostic Test/Exam",
                                                               "Sonogram or Ultrasound", 
                                                               "Surgery",
                                                               "Throat Swab",
                                                               "Vaccination",
                                                               "X-Rays"),
                                                   selected = "Lab Tests")
                                ),
                              mainPanel(
                                h1(uiOutput(outputId = "n_visits")),
                                uiOutput(outputId = "n_visits_condition"),
                                hr(),
                                h2(p("Services")),
                                plotOutput(outputId = "agg_services_plot"),
                                hr(),
                                h4(uiOutput(outputId = "selected_service")),
                                uiOutput(outputId = "service_all_conditions"),
                                uiOutput(outputId = "related_to_condition"),
                                plotOutput(outputId = "conj_service_plot"),
                                hr())
                            )
                          )),
                 
                 tabPanel("Expenditures",
                          fluidPage(
                            titlePanel("Emergency Room Expenditures"),
                            sidebarLayout(
                              sidebarPanel(
                                p("To see the distribution of expenditures for a given condition,"),
                                selectInput("condition_exp", 
                                            "Select a medical condition:",
                                            choices = c("All",
                                                        "Asthma",
                                                        "Anxiety disorder",
                                                        "Calculus of urinary tract",
                                                        "Chronic obstructive pulmonary disease and bronchiectasis",
                                                        "Essential hypertension",
                                                        "Fracture of upper limb",
                                                        "Headache; including migraine",
                                                        "Intestinal infection",
                                                        "Joint disorders and dislocations; trauma-related",
                                                        "Open wounds of extremities",
                                                        "Other connective tissue disease",
                                                        "Other injuries and conditions due to external causes",
                                                        "Other upper respiratory disease",
                                                        "Other upper respiratory infections",
                                                        "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)",
                                                        "Spondylosis; intervertebral disc disorders; other back problems",
                                                        "Urinary tract infections")),
                                br(),
                                p("To see the distribution of expenditures for a particular service(s),"),
                                checkboxGroupInput("service_exp",
                                                   "Select a service:",
                                                   choices = c("Anesthesia",
                                                               "EEG",
                                                               "EKG or ECG", 
                                                               "Lab Tests",
                                                               "Mammogram",
                                                               "Medicine Prescribed",
                                                               "MRI or CT Scan",
                                                               "Other Diagnostic Test/Exam",
                                                               "Sonogram or Ultrasound", 
                                                               "Surgery",
                                                               "Throat Swab",
                                                               "Vaccination",
                                                               "X-Rays"),
                                                   selected = c("Anesthesia",
                                                                "EEG",
                                                                "EKG or ECG", 
                                                                "Lab Tests",
                                                                "Mammogram",
                                                                "Medicine Prescribed",
                                                                "MRI or CT Scan",
                                                                "Other Diagnostic Test/Exam",
                                                                "Sonogram or Ultrasound", 
                                                                "Surgery",
                                                                "Throat Swab",
                                                                "Vaccination",
                                                                "X-Rays"))
                              ),
                              mainPanel(
                                h1(uiOutput(outputId = "max_expenditure")),
                                uiOutput(outputId = "median_expenditure"),
                                hr(),
                                h2(p("Expenditures")),
                                plotOutput(outputId = "expenditure_histogram")
                              )
                              )
                            )),
                 
                 tabPanel("High-Use Patients",
                          fluidPage(
                            titlePanel("Emergency Room High-Use Patients"),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("care_category2", 
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
                                
                                selectInput("condition2", 
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
                              mainPanel(h1(uiOutput(outputId = "n_patients")),
                                        uiOutput(outputId = "n_households"),
                                        hr())
                            )
                          ))
                 )

# Define server logic required to create each page
server <- function(input, output) {
  
#### SERVICES PAGE ####
  # Text that shows the total number of visits that received any services
  output$n_visits <- renderText({
    n_visits_total <- app_data %>% 
      count(event_id) 
    paste(nrow(n_visits_total), "total visits")
  })
  
  # Text that shows the number of visits which had the selected condition
  output$n_visits_condition <- renderText({
      if (input$condition != "All") {
        n_visits <- app_data %>% 
          filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition) %>% 
          count(event_id)
        paste(nrow(n_visits), "visits related to ", str_to_lower(input$condition))
        }
  })
  
  # Bar plot that shows popularity of services dependent on condition selected
  output$agg_services_plot <- renderPlot({
    
    # Filtering data by selected condition
    services_data <- switch(input$condition,
                            "All" = app_data,
                            "Asthma" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Anxiety disorder" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Calculus of urinary tract" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Essential hypertension" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Fracture of upper limb" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Headache; including migraine" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Intestinal infection" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Joint disorders and dislocations; trauma-related" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Open wounds of extremities" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other connective tissue disease" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other injuries and conditions due to external causes" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other upper respiratory disease" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other upper respiratory infections" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Urinary tract infections" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition))
      
      # producing a bar plot based on the filtered data that shows popularity of services
      services_data %>% 
        count(service_received) %>% 
        ggplot(aes(x = fct_reorder(service_received, n), y = n, fill = service_received)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_x_discrete(name = "") +
        ylab("Number of visits that provided service") +
        ggtitle("Most Popular Emergency Room Services") +
        scale_fill_manual(values = c("Lab Tests" = "#F8766D", "X-Rays" = "#24B700", 
                                     "Medicine Prescribed" = "#00ACFC", "MRI or CT Scan" = "#FF65AC", 
                                     "EKG or ECG" = "#E18A00", "Other Diagnostic Test/Exam" = "#00BE70", 
                                     "Sonogram or Ultrasound" = "#8B93FF",
                                     "Surgery" = "#BE9C00", "Anesthesia" = "#00C1AB", "Throat Swab" = "#D575FE",
                                     "EEG" = "#8CAB00", "Vaccination" = "#00BBDA", "Mammogram" = "#F962DD"))
    
   
  })
  
  # Text displaying what the pie charts below will show
  output$selected_service <- renderText({
    paste("Services provided in conjunction with ", input$service)
  })
  
  output$service_all_conditions <- renderText({
    if (input$condition == "All") {
      service_data <- app_data %>% 
        filter(service_received == input$service) %>% 
        count(event_id)
      paste("(", nrow(service_data), "total visits with", input$service, ")")
    }
  })
  
  output$related_to_condition <- renderText({
    if (input$condition != "All") {
      condition_data <- app_data %>% 
        filter(service_received == input$service, condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition) %>% 
        count(event_id)
      paste("for visits related to ", str_to_lower(input$condition), "(", nrow(condition_data), 
            "visits with", input$service, ")")
        }
  })
  
  
  output$conj_service_plot <- renderPlot({
    
    # Filtering data by selected condition
    services_data <- switch(input$condition,
                            "All" = app_data,
                            "Asthma" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Anxiety disorder" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Calculus of urinary tract" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Essential hypertension" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Fracture of upper limb" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Headache; including migraine" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Intestinal infection" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Joint disorders and dislocations; trauma-related" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Open wounds of extremities" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other connective tissue disease" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other injuries and conditions due to external causes" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other upper respiratory disease" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Other upper respiratory infections" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition),
                            "Urinary tract infections" = app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | condition_3 == input$condition | condition_4 == input$condition))
    
    # filtering for events that had the selected service
    with_service <- services_data %>% 
      filter(service_received == input$service) %>% 
      select(event_id)
    
    services_data %>% 
      semi_join(with_service, by = "event_id") %>% 
      filter(service_received != input$service) %>% 
      ggplot(aes(x = service_received, fill = service_received)) +
      geom_bar(show.legend = FALSE, width = 1) +
      scale_fill_manual(values = c("Lab Tests" = "#F8766D", "X-Rays" = "#24B700", 
                                   "Medicine Prescribed" = "#00ACFC", "MRI or CT Scan" = "#FF65AC", 
                                   "EKG or ECG" = "#E18A00", "Other Diagnostic Test/Exam" = "#00BE70", 
                                   "Sonogram or Ultrasound" = "#8B93FF",
                                   "Surgery" = "#BE9C00", "Anesthesia" = "#00C1AB", "Throat Swab" = "#D575FE",
                                   "EEG" = "#8CAB00", "Vaccination" = "#00BBDA", "Mammogram" = "#F962DD")) +
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL) +
      coord_polar()
  })
  
  
#### EXPENDITURES PAGE ####  
  # Text that displays maximum expenditure for selected condition and services
  output$max_expenditure <- renderText({
    expenditures_data <- switch(input$condition_exp,
                                "All" = app_data,
                                "Asthma" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Anxiety disorder" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Calculus of urinary tract" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Essential hypertension" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Fracture of upper limb" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Headache; including migraine" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Intestinal infection" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Joint disorders and dislocations; trauma-related" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Open wounds of extremities" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other connective tissue disease" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other injuries and conditions due to external causes" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other upper respiratory disease" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other upper respiratory infections" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Urinary tract infections" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp))
    
    expenditures_data <- expenditures_data %>% 
      filter(service_received %in% input$service_exp) %>% 
      arrange(desc(`Total Expenditure`)) %>% 
      slice(1:1)
   
   expenditures_data$`Total Expenditure` <- comma(expenditures_data$`Total Expenditure`)
   
   paste0("max expenditure: $", expenditures_data$`Total Expenditure`)
  })
  
  # Text that displays the median expenditure for selected condition and services
  output$median_expenditure <- renderText({
    expenditures_data <- switch(input$condition_exp,
                                "All" = app_data,
                                "Asthma" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Anxiety disorder" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Calculus of urinary tract" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Essential hypertension" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Fracture of upper limb" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Headache; including migraine" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Intestinal infection" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Joint disorders and dislocations; trauma-related" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Open wounds of extremities" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other connective tissue disease" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other injuries and conditions due to external causes" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other upper respiratory disease" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other upper respiratory infections" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Urinary tract infections" = app_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp))
    
    expenditures_data <- expenditures_data %>% 
      filter(service_received %in% input$service_exp) %>% 
      count(`Total Expenditure`) %>% 
      summarize(median_expenditure = median(`Total Expenditure`))
    
    expenditures_data$median_expenditure <- comma(expenditures_data$median_expenditure)
    
    paste0("median expenditure: $", expenditures_data$median_expenditure)
  })
  
  # Histogram that shows the distribution of expenditures for visits with the selected characteristics
  output$expenditure_histogram <- renderPlot({
    expenditures_data <- switch(input$condition_exp,
                            "All" = app_data,
                            "Asthma" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Anxiety disorder" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Calculus of urinary tract" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Essential hypertension" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Fracture of upper limb" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Headache; including migraine" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Intestinal infection" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Joint disorders and dislocations; trauma-related" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Open wounds of extremities" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Other connective tissue disease" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Other injuries and conditions due to external causes" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Other upper respiratory disease" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Other upper respiratory infections" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                            "Urinary tract infections" = app_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp))
    
    expenditures_data %>% 
      filter(service_received %in% input$service_exp) %>% 
      mutate(`Total Expenditure` = `Total Expenditure` + 1) %>% 
      ggplot(aes(x = `Total Expenditure`, fill = service_received)) +
      geom_histogram() +
      scale_x_continuous(name = "Total Expenditure", 
                         breaks = c(100, 1000, 10000, 100000), 
                         labels = c("$100", "$1,000", "$10,000", "$100,000"), 
                         trans = "log10") +
      ylab("Number of Visits") +
      scale_fill_manual(name = "Service", 
                        values = c("Lab Tests" = "#F8766D", "X-Rays" = "#24B700", 
                                 "Medicine Prescribed" = "#00ACFC", "MRI or CT Scan" = "#FF65AC", 
                                 "EKG or ECG" = "#E18A00", "Other Diagnostic Test/Exam" = "#00BE70", 
                                 "Sonogram or Ultrasound" = "#8B93FF",
                                 "Surgery" = "#BE9C00", "Anesthesia" = "#00C1AB", "Throat Swab" = "#D575FE",
                                 "EEG" = "#8CAB00", "Vaccination" = "#00BBDA", "Mammogram" = "#F962DD")) +
      labs(title = "Distribution of Expenditures by Service", caption = "***Note: If a visit required multiple services, it is counted once for each service provided.")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
