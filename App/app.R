# Exploring MEPS Emergency Room Visits Data (2016)


# Loading the necessary packages

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)


# Loading emergency visits data and expenditure data created in the App_Background file

app_data <- read_rds("app_data")
expenditure_data <- read_rds("expenditure_data")


################################################################ UI ###############################################################

ui <- navbarPage("Exploring MEPS Emergency Room Visits Data",
                 
                 ##################### ABOUT PAGE #####################
                 
                 tabPanel("About",
                          fluidPage(
                            titlePanel("About"),
                            fluidRow(
                              column(12,
                                     includeMarkdown("about_page.Rmd"))
                                )
                            )
                          ),
                 
                 
                 ##################### SERVICES PAGE #####################
                 
                 tabPanel("Services",
                          fluidPage(
                            titlePanel("Emergency Room Services"),
                            fluidRow(
                              column(12,
                                     br(),
                                     p("The MEPS collects information on whether or not patients (surveyed)
                                       received any one of the following 13 services: anesthesia, EEGs, EKGs
                                       or ECGs, lab tests, mammograms, MRI or CT scans, other diagnostic 
                                       tests/exams, precription of medicine, sonograms or ultrasounds, 
                                       surgery, throat swabs, vaccinations, and/or x-rays. Use this page to
                                       explore which services tend to be provided for which conditions and 
                                       which services tend to be provided in conjunction with one another."),
                                     hr())
                              ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Visits which provided services")),
                                     p("The bar plot below shows how many visits provided a particular
                                       service given the selected medical condition. Keep in mind that many
                                       visits require multiple services."),
                                     h1(uiOutput(outputId = "n_visits")),
                                     uiOutput(outputId = "n_visits_condition"),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
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
                                         br(),
                                         helpText("Note: the conditions you are allowed to select are only some of
                                           the most common of the 153 conditions in the MEPS 2016 Emergency
                                           Room Visits file."),
                                         br()
                                         ),
                                       mainPanel(
                                         plotOutput(outputId = "agg_services_plot")
                                         )
                                       ),
                                     hr()
                                     )
                              ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Services provided in conjunction with one another")),
                                     p("The plot below shows the number of visits that provide a particular
                                       service in conjuction with the service selected and given the selected
                                       medical condition."),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("condition2", 
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
                                         radioButtons("service", 
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
                                                      selected = "Lab Tests")
                                       ),
                                       mainPanel(
                                         h4(uiOutput(outputId = "selected_service")),
                                         uiOutput(outputId = "service_all_conditions"),
                                         uiOutput(outputId = "related_to_condition"),
                                         hr(),
                                         plotOutput(outputId = "conj_service_plot")
                                         )
                                       ),
                                     br()
                                     )
                            )
                            )
                          ),
                 
                 
                 ##################### EXPENDITURES PAGE #####################
                 
                 tabPanel("Expenditures",
                          fluidPage(
                            titlePanel("Emergency Room Expenditures"),
                            fluidRow(
                              column(12,
                                     br(),
                                     p("The MEPS collects information on how much was charged for each 
                                       patient visit and how much was spent on each patient visit (as
                                       hospitals often charge much more than they actually receive in
                                       reimbursement). This page lets you explore the distribution of 
                                       expenditures for a given condition by the services provided."),
                                     hr())
                            ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Distribution of expenditures by payer")),
                                     p("The bar plot below shows who paid for what proportion of each non-zero expenditure. You 
                                       can look at this data for a particular condition, price range, and payer(s)."),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
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
                                         sliderInput("exp_range",
                                                     "Select an expenditure range:",
                                                     min = 0.01,
                                                     max = 110000,
                                                     pre = "$",
                                                     sep = ",",
                                                     value = c(0, 1000)),
                                         br(),
                                         checkboxGroupInput("payer_type",
                                                            "Select payer(s):",
                                                            choices = c("Out of Pocket",
                                                                        "Medicaid",
                                                                        "Medicare",
                                                                        "Private Insurance",
                                                                        "Other Insurance"),
                                                            selected = c("Out of Pocket",
                                                                         "Medicaid",
                                                                         "Medicare",
                                                                         "Private Insurance",
                                                                         "Other Insurance"))
                                       ),
                                       mainPanel(
                                         h1(uiOutput(outputId = "payer_n_visits")),
                                         hr(),
                                         plotOutput(outputId = "payer_barplot")
                                       )
                                     ),
                                     hr()
                              )
                            ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Distribution of expenditures by service")),
                                     p("The histogram below shows the distribution of expenditures for the
                                       selected condition and services. Note that the x-axis is on a
                                       logarithmic scale."),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("condition_exp2",
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
                                                                         "X-Rays"))),
                              mainPanel(
                                h1(uiOutput(outputId = "max_expenditure")),
                                uiOutput(outputId = "median_expenditure"),
                                hr(),
                                plotOutput(outputId = "expenditure_histogram")
                                )
                              ),
                              hr()
                              )
                            ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Distribution of payments between physicians and facilities")),
                                     p("The barplot below shows which proportion of payments went to physicians versus facilities
                                       for a particular medical condition."),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("condition_exp3", 
                                                     "Select a condition:",
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
                                                       sliderInput("exp_range2",
                                                                   "Select an overall expenditure range:",
                                                                   min = 0.01,
                                                                   max = 110000,
                                                                   pre = "$",
                                                                   sep = ",",
                                                                   value = c(0, 1000)),
                                                      hr(),
                                                      helpText(uiOutput(outputId = "dr_facility_stats"))
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "dr_facility_barplot")
                                       )
                                     ),
                                     br()
                                     )
                            )
                            )),
                 
                 
                 ##################### HIGH-USE PATIENTS PAGE #####################
                 
                 tabPanel("High-Use Patients",
                          fluidPage(
                            titlePanel("Emergency Room High-Use Patients"),
                            fluidRow(
                              column(12,
                                     br(),
                                     p("MEPS denotes each individual with a unique code so that individuals who visited the 
                                       emergency room multiple times in 2016 can be linked across visits. This page allows you to
                                       explore whether there are any obvious differences between high-use patients and one-time
                                       patients in terms of costs and conditions."),
                                     hr())
                              ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Comparing costs for high-use and low-use patients")),
                                     p("For visits that did require expenditures, do patients who go to the emergency room more 
                                       frequently tend to require different expenditures?"),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         helpText("Define the lower limit for high-use patients"),
                                         numericInput("n_definition",
                                                      "Number of visits in 2016:",
                                                      min = 2,
                                                      max = 13,
                                                      value = 2),
                                         br(),
                                         radioButtons("payment_type",
                                                      "Compare by payment type:",
                                                      choices = c("Total Expenditure",
                                                                  "Out of Pocket",
                                                                  "Medicaid",
                                                                  "Medicare",
                                                                  "Private Insurance",
                                                                  "Other Insurance"),
                                                      selected = "Total Expenditure")
                                       ),
                                       mainPanel(
                                         uiOutput(outputId = "high_low_med_exp"),
                                         hr(),
                                         plotOutput(outputId = "high_low_exp")
                                       )
                                     ),
                                     hr()
                                     )
                            ),
                            
                            fluidRow(
                              column(12,
                                     h2(p("Comparing conditions of high-use and low-use patients")),
                                     p("Are high-use patients more likely to have certain conditions compared to low-use 
                                       patients?"),
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         numericInput("top_n",
                                                      "Number of top conditions:",
                                                      min = 1,
                                                      max = 10,
                                                      value = 5),
                                         br(),
                                         helpText("Define the lower limit for high-use patients"),
                                         numericInput("n_definition2",
                                                      "Number of visits in 2016:",
                                                      min = 2,
                                                      max = 13,
                                                      value = 2),
                                         br()
                                       ),
                                       mainPanel(
                                         h3(uiOutput(outputId = "one_condition")),
                                         h3(uiOutput(outputId = "multiple_conditions")),
                                         plotOutput(outputId = "high_low_conditions")
                                       )
                                     ),
                                     br()
                                     )
                            )
                            
                          ))
                 )

############################################################## SERVER #############################################################

server <- function(input, output) {
  
##################### SERVICES PAGE #####################
  
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
    if (input$condition2 == "All") {
      service_data <- app_data %>% 
        filter(service_received == input$service) %>% 
        count(event_id)
      paste0("(", nrow(service_data), " total visits with ", input$service, ")")
    }
  })
  
  output$related_to_condition <- renderText({
    if (input$condition2 != "All") {
      condition_data <- app_data %>% 
        filter(service_received == input$service, condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2) %>% 
        count(event_id)
      paste("for visits related to ", str_to_lower(input$condition2), "(", nrow(condition_data), 
            "visits with", input$service, ")")
        }
  })
  
  
  output$conj_service_plot <- renderPlot({
    
    # Filtering data by selected condition
    services_data <- switch(input$condition2,
                            "All" = app_data,
                            "Asthma" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Anxiety disorder" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Calculus of urinary tract" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Essential hypertension" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Fracture of upper limb" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Headache; including migraine" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Intestinal infection" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Joint disorders and dislocations; trauma-related" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Open wounds of extremities" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Other connective tissue disease" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Other injuries and conditions due to external causes" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Other upper respiratory disease" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Other upper respiratory infections" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2),
                            "Urinary tract infections" = app_data %>% 
                              filter(condition == input$condition2  | condition_2 == input$condition2 | condition_3 == input$condition2 | condition_4 == input$condition2))
    
    # filtering for events that had the selected service
    with_service <- services_data %>% 
      filter(service_received == input$service) %>% 
      select(event_id)
    
    services_data %>% 
      semi_join(with_service, by = "event_id") %>% 
      mutate(conj_service = case_when(n_services_visit != 1 ~ service_received,
                                      n_services_visit == 1 ~ "No other services")) %>% 
      filter(conj_service != input$service) %>% 
      count(conj_service) %>% 
      ggplot(aes(x = fct_reorder(conj_service, -n), y = n, fill = fct_reorder(conj_service, -n))) +
      geom_col(width = 1) +
      scale_fill_manual(values = c("Lab Tests" = "#F8766D", "X-Rays" = "#24B700", 
                                   "Medicine Prescribed" = "#00ACFC", "MRI or CT Scan" = "#FF65AC", 
                                   "EKG or ECG" = "#E18A00", "Other Diagnostic Test/Exam" = "#00BE70", 
                                   "Sonogram or Ultrasound" = "#8B93FF",
                                   "Surgery" = "#BE9C00", "Anesthesia" = "#00C1AB", "Throat Swab" = "#D575FE",
                                   "EEG" = "#8CAB00", "Vaccination" = "#00BBDA", "Mammogram" = "#F962DD",
                                   "No other services" = "grey")) +
      labs(fill = "Additional Service") +
      scale_x_discrete(labels = c("", "", "", "","", "", "", "", "", "", "", "", "")) + 
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL) +
      coord_polar()
  })
  
  
##################### EXPENDITURES PAGE #####################
  ## Distribution of expenditures by payer
        # Text displaying the number of visits being viewed on the barplot
        output$payer_n_visits <- renderText({
          payers_data <- switch(input$condition_exp,
                                "All" = expenditure_data,
                                "Asthma" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Anxiety disorder" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Calculus of urinary tract" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Chronic obstructive pulmonary disease and bronchiectasis" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Essential hypertension" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Fracture of upper limb" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Headache; including migraine" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Intestinal infection" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Joint disorders and dislocations; trauma-related" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Open wounds of extremities" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other connective tissue disease" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other injuries and conditions due to external causes" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other upper respiratory disease" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Other upper respiratory infections" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Spondylosis; intervertebral disc disorders; other back problems" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                "Urinary tract infections" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp))
          
          payer_visits <- payers_data %>%
            filter(!payment_type %in% c("Total Facility Payment", "Total Doctor Payment"),
                   payment_type %in% input$payer_type,
                   `Total Expenditure` > input$exp_range[1], `Total Expenditure` <= input$exp_range[2]) %>% 
            count(event_id)
          
          paste("Number of visits:", comma(nrow(payer_visits)))
        })
        
        # Barplot that displays proportion each payer paid
        output$payer_barplot <- renderPlot({
          payers_data <- switch(input$condition_exp,
                                      "All" = expenditure_data,
                                      "Asthma" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Anxiety disorder" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Calculus of urinary tract" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Chronic obstructive pulmonary disease and bronchiectasis" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Essential hypertension" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Fracture of upper limb" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Headache; including migraine" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Intestinal infection" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Joint disorders and dislocations; trauma-related" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Open wounds of extremities" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Other connective tissue disease" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Other injuries and conditions due to external causes" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Other upper respiratory disease" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Other upper respiratory infections" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Spondylosis; intervertebral disc disorders; other back problems" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp),
                                      "Urinary tract infections" = expenditure_data %>% filter(condition == input$condition_exp  | condition_2 == input$condition_exp | condition_3 == input$condition_exp | condition_4 == input$condition_exp))
          
          payers_data %>%
            filter(!payment_type %in% c("Total Facility Payment", "Total Doctor Payment"),
                   payment_type %in% input$payer_type,
                   `Total Expenditure` > input$exp_range[1], `Total Expenditure` <= input$exp_range[2]) %>% 
            mutate(fct_payment_type = case_when(payment_type == "Out of Pocket" ~ 1,
                                                payment_type == "Medicaid" ~ 2,
                                                payment_type == "Medicare" ~ 3,
                                                payment_type == "Private Insurance" ~ 4,
                                                payment_type == "Other Insurance" ~ 5)) %>%
            ggplot(aes(x = event_id, y = amount, fill = fct_reorder(payment_type, -fct_payment_type))) +
            geom_col(width = 1) +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) +
            scale_fill_manual(name = "Payer",
                              values = c("Out of Pocket" = "#F27D53", "Medicaid" = "#61B200", 
                                         "Medicare" = "#00B0F0", "Private Insurance" = "#B186FF", 
                                         "Other Insurance" = "#FF62BC")) +
            scale_y_continuous(name = "Amount Paid", labels = dollar)
        })
  
  ## Distribution of expenditures by service
        # Text that displays maximum expenditure for selected condition and services
        output$max_expenditure <- renderText({
          expenditures_data <- switch(input$condition_exp2,
                                      "All" = app_data,
                                      "Asthma" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Anxiety disorder" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Calculus of urinary tract" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Essential hypertension" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Fracture of upper limb" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Headache; including migraine" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Intestinal infection" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Joint disorders and dislocations; trauma-related" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Open wounds of extremities" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other connective tissue disease" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other injuries and conditions due to external causes" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other upper respiratory disease" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other upper respiratory infections" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Urinary tract infections" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2))
          
          expenditures_data <- expenditures_data %>% 
            filter(service_received %in% input$service_exp) %>% 
            arrange(desc(`Total Expenditure`)) %>% 
            slice(1:1)
         
         expenditures_data$`Total Expenditure` <- comma(expenditures_data$`Total Expenditure`)
         
         paste0("Maximum expenditure: $", expenditures_data$`Total Expenditure`)
        })
        
        # Text that displays the median expenditure for selected condition and services
        output$median_expenditure <- renderText({
          expenditures_data <- switch(input$condition_exp2,
                                      "All" = app_data,
                                      "Asthma" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Anxiety disorder" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Calculus of urinary tract" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Essential hypertension" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Fracture of upper limb" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Headache; including migraine" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Intestinal infection" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Joint disorders and dislocations; trauma-related" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Open wounds of extremities" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other connective tissue disease" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other injuries and conditions due to external causes" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other upper respiratory disease" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Other upper respiratory infections" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                      "Urinary tract infections" = app_data %>% filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2))
          
          expenditures_data <- expenditures_data %>% 
            filter(service_received %in% input$service_exp) %>% 
            count(`Total Expenditure`) %>% 
            summarize(median_expenditure = median(`Total Expenditure`))
          
          expenditures_data$median_expenditure <- comma(expenditures_data$median_expenditure)
          
          paste0("Median expenditure: $", expenditures_data$median_expenditure)
        })
        
        # Histogram that shows the distribution of expenditures for visits with the selected characteristics
        output$expenditure_histogram <- renderPlot({
          expenditures_data <- switch(input$condition_exp2,
                                  "All" = app_data,
                                  "Asthma" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Anxiety disorder" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Calculus of urinary tract" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Chronic obstructive pulmonary disease and bronchiectasis" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Essential hypertension" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Fracture of upper limb" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Headache; including migraine" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Intestinal infection" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Joint disorders and dislocations; trauma-related" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Open wounds of extremities" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Other connective tissue disease" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Other injuries and conditions due to external causes" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Other upper respiratory disease" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Other upper respiratory infections" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Spondylosis; intervertebral disc disorders; other back problems" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2),
                                  "Urinary tract infections" = app_data %>% 
                                    filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2))
          
          expenditures_data %>% 
            filter(service_received %in% input$service_exp) %>% 
            mutate(`Total Expenditure` = `Total Expenditure` + 1) %>% 
            ggplot(aes(x = `Total Expenditure`, fill = service_received)) +
            geom_histogram() +
            scale_x_continuous(name = "Total Expenditure", 
                               breaks = c(100, 1000, 10000, 100000), 
                               labels = c("$100", "$1,000", "$10,000", "$100,000"), 
                               trans = "log10") +
            ylab("Number of times service was provided") +
            scale_fill_manual(name = "Service", 
                              values = c("Lab Tests" = "#F8766D", "X-Rays" = "#24B700", 
                                       "Medicine Prescribed" = "#00ACFC", "MRI or CT Scan" = "#FF65AC", 
                                       "EKG or ECG" = "#E18A00", "Other Diagnostic Test/Exam" = "#00BE70", 
                                       "Sonogram or Ultrasound" = "#8B93FF",
                                       "Surgery" = "#BE9C00", "Anesthesia" = "#00C1AB", "Throat Swab" = "#D575FE",
                                       "EEG" = "#8CAB00", "Vaccination" = "#00BBDA", "Mammogram" = "#F962DD")) +
            labs(caption = "***Note: Many expenditures included multiple services.")
        })
        
    ## Distribution of payments between doctors and facilities
        # Text showing median and maximum payments to doctors and facilities
        output$dr_facility_stats <- renderText({
          payers_data <- switch(input$condition_exp3,
                                "All" = expenditure_data,
                                "Asthma" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Anxiety disorder" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Calculus of urinary tract" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Chronic obstructive pulmonary disease and bronchiectasis" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Essential hypertension" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Fracture of upper limb" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Headache; including migraine" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Intestinal infection" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Joint disorders and dislocations; trauma-related" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Open wounds of extremities" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other connective tissue disease" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other injuries and conditions due to external causes" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other upper respiratory disease" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other upper respiratory infections" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Spondylosis; intervertebral disc disorders; other back problems" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Urinary tract infections" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3))
          
          dr_stats <- payers_data %>%
            filter(payment_type == "Total Doctor Payment",
                   `Total Expenditure` > input$exp_range2[1], `Total Expenditure` <= input$exp_range2[2]) %>% 
            group_by(payment_type) %>% 
            summarize(median = median(amount), maximum = max(amount))
          
          facility_stats <- payers_data %>%
            filter(payment_type == "Total Facility Payment",
                   `Total Expenditure` > input$exp_range2[1], `Total Expenditure` <= input$exp_range2[2]) %>% 
            group_by(payment_type) %>% 
            summarize(median = median(amount), maximum = max(amount))
          
          paste0("Median payment to doctor: $", comma(dr_stats$median), br(), 
                 "Maximum payment to doctor: $", comma(dr_stats$maximum), br(), br(), 
                 "Median payment to facility: $", comma(facility_stats$median), br(),
                 "Maximum payment to facility: $", comma(facility_stats$maximum))
        })
          
        # Barplot that shows the distribution of payments between doctors and facilities
        output$dr_facility_barplot <- renderPlot({
          payers_data <- switch(input$condition_exp3,
                                "All" = expenditure_data,
                                "Asthma" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Anxiety disorder" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Calculus of urinary tract" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Chronic obstructive pulmonary disease and bronchiectasis" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Essential hypertension" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Fracture of upper limb" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Headache; including migraine" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Intestinal infection" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Joint disorders and dislocations; trauma-related" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Open wounds of extremities" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other connective tissue disease" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other injuries and conditions due to external causes" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other upper respiratory disease" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Other upper respiratory infections" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Spondylosis; intervertebral disc disorders; other back problems" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3),
                                "Urinary tract infections" = expenditure_data %>% filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3))
          
          payers_data %>%
            filter(payment_type %in% c("Total Facility Payment", "Total Doctor Payment"),
                   `Total Expenditure` > input$exp_range2[1], `Total Expenditure` <= input$exp_range2[2]) %>% 
            ggplot(aes(x = event_id, y = amount, fill = payment_type)) +
            geom_col(width = 1) +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) +
            scale_fill_manual(name = "",
                              values = c("Total Facility Payment" = "#FF6666", "Total Doctor Payment" = "#FF9900")) +
            scale_y_continuous(name = "Amount Paid", labels = dollar) +
            scale_x_discrete(name = "")
        })
        
##################### HIGH-USE PATIENTS PAGE #####################
  ## Comparing costs for high-use and low-use patients
      # Text that shows median amount paid for high-use vs low-use patients
        output$high_low_med_exp <- renderText({
          text_data <- app_data %>% 
            group_by(event_id, condition, condition_2, condition_3, condition_4, n_visits_2016) %>% 
            summarize(`Total Expenditure` = mean(`Total Expenditure`),
                      `Out of Pocket` = mean(`Out of Pocket`),
                      `Medicaid` = mean(`Medicaid`),
                      `Medicare` = mean(`Medicare`),
                      `Private Insurance` = mean(`Private Insurance`),
                      `Other Insurance` = mean(`Other Insurance`)) %>% 
            ungroup() %>% 
            gather(-event_id, -condition, -condition_2, -condition_3, -condition_4, -n_visits_2016,
                   key = payment_type, value = `Amount Paid`) %>% 
            filter(payment_type == input$payment_type, `Amount Paid` != 0) %>% 
            mutate(`Amount Paid` = `Amount Paid` + 1,
                   type = case_when(n_visits_2016 < input$n_definition ~ "Low-use",
                                     n_visits_2016 >= input$n_definition ~ "High-use")) %>% 
            group_by(type) %>% 
            summarize(median = median(`Amount Paid`)) %>% 
            ungroup()
          
          paste0("Median for high-use patients: $", comma(subset(text_data, type == "High-use")$median), br(),
                 "Median for low-use patients: $", comma(subset(text_data, type == "Low-use")$median))
        })
        
      # Histogram that shows distribution of payments for high-use vs low-use patients
        output$high_low_exp <- renderPlot({
          
          plot_data <- app_data %>% 
            group_by(event_id, condition, condition_2, condition_3, condition_4, n_visits_2016) %>% 
            summarize(`Total Expenditure` = mean(`Total Expenditure`),
                      `Out of Pocket` = mean(`Out of Pocket`),
                      `Medicaid` = mean(`Medicaid`),
                      `Medicare` = mean(`Medicare`),
                      `Private Insurance` = mean(`Private Insurance`),
                      `Other Insurance` = mean(`Other Insurance`)) %>% 
            ungroup() %>% 
            gather(-event_id, -condition, -condition_2, -condition_3, -condition_4, -n_visits_2016,
                   key = payment_type, value = `Amount Paid`) %>% 
            filter(payment_type == input$payment_type, `Amount Paid` != 0) %>% 
            mutate(`Amount Paid` = `Amount Paid` + 1,
                   type = case_when(n_visits_2016 < input$n_definition ~ "Low-use",
                                     n_visits_2016 >= input$n_definition ~ "High-use"))
            
          
          ggplot(plot_data, aes(x = `Amount Paid`)) + 
            geom_histogram(data = subset(plot_data, type == "Low-use"), aes(fill = type), alpha = 0.2) +
            geom_histogram(data = subset(plot_data, type == "High-use"), aes(fill = type), alpha = 0.2) +
            scale_fill_manual(name = "Type", values = c("red", "blue"), labels = c("High-use","Low-use")) +
            scale_x_continuous(name = "Amount Paid",
                               breaks = c(100, 1000, 10000, 100000), 
                               labels = c("$100", "$1,000", "$10,000", "$100,000"),
                               trans = "log10") +
            ylab("Number of Visits")
        })
        
  ## Comparing conditions of high-use and low-use patients
      # Title for bar plots if only one condition selected
      output$one_condition <- renderText({
        if (input$top_n == 1) {
          paste("Top condition for high-use vs. low-use patients")
        }
      })
      
      # Title for bar plots if multiple conditions selected
      output$multiple_conditions <- renderText({
        if (input$top_n != 1) {
          paste("Top", input$top_n,"conditions for high-use vs. low-use patients")
        }
      })
        
      # Bar plots that show top conditions for high-use and low use patients
      output$high_low_conditions <- renderPlot({
        
        high_low_conditions <- app_data %>% 
          mutate(type = case_when(n_visits_2016 < input$n_definition2 ~ "Low-use",
                                  n_visits_2016 >= input$n_definition2 ~ "High-use")) %>% 
          group_by(type) %>% 
          count(event_id, condition, condition_2, condition_3, condition_4) %>% 
          gather(-event_id, -n, -type, key = "condition_number", value = "conditions", na.rm = TRUE) %>% 
          arrange(event_id) %>% 
          select(type, event_id, conditions) %>% 
          mutate(total_type_visits = n_distinct(event_id)) %>% 
          count(conditions, total_type_visits) %>% 
          mutate(condition_percent = n/total_type_visits) %>% 
          arrange(type, desc(condition_percent)) %>% 
          slice(1:input$top_n)
        
        ggplot(high_low_conditions, aes(x = fct_reorder(conditions, condition_percent), y = condition_percent, fill = type)) +
          geom_col(alpha = 0.2, position = "dodge") +
          scale_fill_manual(name = "Type", values = c("red", "blue"), labels = c("High-use","Low-use")) +
          coord_flip() +
          scale_x_discrete(name = "") +
          scale_y_continuous(name = "Percent of visits with condition", labels = percent) +
          facet_wrap(~type)
        
      })

}

# Run the application 
shinyApp(ui = ui, server = server)
