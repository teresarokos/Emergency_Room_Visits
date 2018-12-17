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


# Creating some vectors that the app uses often: conditions (most common ones), services, and payers.

conditions <- c("All", "Asthma", "Anxiety disorder", "Calculus of urinary tract", 
                "Chronic obstructive pulmonary disease and bronchiectasis", "Essential hypertension", "Fracture of upper limb",
                "Headache; including migraine", "Intestinal infection", "Joint disorders and dislocations; trauma-related",
                "Open wounds of extremities", "Other connective tissue disease", 
                "Other injuries and conditions due to external causes", "Other upper respiratory disease",
                "Other upper respiratory infections", 
                "Pneumonia (except that caused by tuberculosis or sexually transmitted disease)",
                "Spondylosis; intervertebral disc disorders; other back problems", "Urinary tract infections")

services <- c("Anesthesia", "EEG", "EKG or ECG", "Lab Tests", "Mammogram", "Medicine Prescribed", "MRI or CT Scan",
              "Other Diagnostic Test/Exam", "Sonogram or Ultrasound", "Surgery", "Throat Swab", "Vaccination", "X-Rays")

payers <- c("Out of Pocket", "Medicaid", "Medicare", "Private Insurance", "Other Insurance")

services_fill <- c("Lab Tests" = "#F8766D", "X-Rays" = "#24B700", "Medicine Prescribed" = "#00ACFC", "MRI or CT Scan" = "#FF65AC", 
                   "EKG or ECG" = "#E18A00", "Other Diagnostic Test/Exam" = "#00BE70", "Sonogram or Ultrasound" = "#8B93FF",
                   "Surgery" = "#BE9C00", "Anesthesia" = "#00C1AB", "Throat Swab" = "#D575FE", "EEG" = "#8CAB00", 
                   "Vaccination" = "#00BBDA", "Mammogram" = "#F962DD", "No other services" = "grey")


################################################################ UI ################################################################

ui <- navbarPage("Exploring MEPS Emergency Room Visits Data",
                 
                 ##################### ABOUT PAGE ##################################################################################
                 
                 tabPanel("About",
                          fluidPage(
                            titlePanel("About"),
                            fluidRow(
                              column(12,
                                     includeMarkdown("about_page.Rmd"))
                                )
                            )
                          ),
                 
                 
                 ##################### SERVICES PAGE ###############################################################################
                 
                 tabPanel("Services",
                          fluidPage(
                            
                            ##### PAGE HEADER ################################################################
                            
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
                            
                            
                            ##### VISITS WHICH PROVIDED SERVICES ##############################################
                            
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
                                                     choices = conditions),
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
                            
                            
                            ##### SERVICES PROVIDED IN CONJUNCTION WITH ONE ANOTHER ############################
                            
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
                                                     choices = conditions),
                                         br(),
                                         radioButtons("service", 
                                                      "Select a service:",
                                                      choices = services,
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
                 
                 
                 ##################### EXPENDITURES PAGE ###########################################################################
                 
                 tabPanel("Expenditures",
                          fluidPage(
                            
                            ##### PAGE HEADER ##########################################################
                            
                            titlePanel("Emergency Room Expenditures"),
                            fluidRow(
                              column(12,
                                     br(),
                                     p("The MEPS collects information on how much was charged for each 
                                       patient visit and how much was spent on each patient visit (as
                                       hospitals often charge much more than they actually receive in
                                       reimbursement). This page lets you explore the distribution of 
                                       expenditures for a given condition by the services provided. It
                                       also may take a few seconds to load initially..."),
                                     hr())
                            ),
                            
                            
                            ##### DISTRIBUTION OF EXPENDITURES BY PAYER ##################################
                            
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
                                                     choices = conditions),
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
                                                            choices = payers,
                                                            selected = payers)
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
                            
                            
                            ##### DISTRIBUTION OF EXPENDITURES BY SERVICE ##################################
                            
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
                                                     choices = conditions),
                                         br(),
                                         checkboxGroupInput("service_exp",
                                                            "Select a service:",
                                                            choices = services,
                                                            selected = services)
                                         ),
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
                            
                            
                            ##### DISTRIBUTION OF PAYMENTS BETWEEN PHYSICIANS AND FACILITIES ###############
                            
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
                                                     choices = conditions),
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
                 
                 
                 ##################### HIGH-USE PATIENTS PAGE ######################################################################
                 
                 tabPanel("High-Use Patients",
                          fluidPage(
                            
                            ##### PAGE HEADER #####################################################################
                            
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
                            
                            
                            ##### COMPARING COSTS FOR HIGH-USE AND LOW-USE PATIENTS ###############################
                            
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
                            
                            
                            ##### COMPARING CONDITIONS OF HIGH-USE AND LOW-USE PATIENTS ################################
                            
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


############################################################## SERVER ##############################################################

server <- function(input, output) {
  
      ##################### SERVICES PAGE ##########################################################################################
            
            ##### VISITS WHICH PROVIDED SERVICES ##############################################
  
                  # output$n_visits: Text that shows the total number of visits that received any services
  
                        output$n_visits <- renderText({
                          
                          # Creating table that has one observation per visit
                          
                          n_visits_total <- app_data %>% 
                            count(event_id) 
                          
                          # Counting rows in table to get total number of distinct visits
                          
                          paste(nrow(n_visits_total), "total visits")
                        })
                  
                        
                  # output$n_visits_condition: Text that shows the number of visits which had the selected condition
                        
                        output$n_visits_condition <- renderText({
                          
                          # Filtering data by selected condition and only displays if condition != "All"
                          
                            if (input$condition != "All") {
                              n_visits <- app_data %>% 
                                filter(condition == input$condition  | condition_2 == input$condition | 
                                         condition_3 == input$condition | condition_4 == input$condition) %>% 
                                
                                # Creating table that has one observation per visit
                                
                                count(event_id)
                              
                              # Counting rows in table to get total number of distinct visits
                              
                              paste(nrow(n_visits), "visits related to ", str_to_lower(input$condition))
                              }
                        })
                        
                  
                  # output$agg_services_plot: Bar plot that shows popularity of services for visits related to selected condition
                        
                        output$agg_services_plot <- renderPlot({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition == "All") {
                            services_data <- app_data
                          }
                          
                          if (input$condition != "All") {
                            services_data <- app_data %>% 
                              filter(condition == input$condition  | condition_2 == input$condition | 
                                       condition_3 == input$condition | condition_4 == input$condition)
                          }
                          
                          services_data %>% 
                            
                            # Creating table that has one observation per service and the number of times that service was received
                            
                            count(service_received) %>% 
                            
                            # Creating a horizontal bar plot that shows most popular services at the top & each service color coded
                            
                            ggplot(aes(x = fct_reorder(service_received, n), y = n, fill = service_received)) +
                            geom_col(show.legend = FALSE) +
                            coord_flip() +
                            scale_x_discrete(name = "") +
                            ylab("Number of visits that provided service") +
                            scale_fill_manual(values = services_fill)
                        })
            
                        
            ##### SERVICES PROVIDED IN CONJUNCTION WITH ONE ANOTHER ##############################################
                        
                  # output$selected_service: Title for conjunctive services chart (changes with selected service)
                        
                        output$selected_service <- renderText({
                          paste("Services provided in conjunction with ", input$service)
                        })
                  
                        
                  # output$service_all_conditions: Text displaying the total number of visits that had the selected service
                  # when condition2 == "All"
                        
                        output$service_all_conditions <- renderText({
                          
                          if (input$condition2 == "All") {
                            service_data <- app_data %>% 
                              
                              # Filtering data for selected service
                              
                              filter(service_received == input$service) %>% 
                              
                              # Creating a table that has one observation per visit that selected service was received
                              
                              count(event_id)
                            
                            # Counting the number of visits that provided selected service
                            
                            paste0("(", nrow(service_data), " total visits with ", input$service, ")")
                          }
                        })
                  
                       
                  # output$related_to_condition: Text displaying number of visits related to selected condition that had 
                  # the selected service  
                        
                        output$related_to_condition <- renderText({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition2 != "All") {
                            condition_data <- app_data %>% 
                              
                              # Filtering data for selected service and condition
                              
                              filter(service_received == input$service, condition == input$condition2  | 
                                       condition_2 == input$condition2 | condition_3 == input$condition2 | 
                                       condition_4 == input$condition2) %>% 
                              
                              # Creating a table that has one observation per visit related to condition
                              # that selected service was received
                              
                              count(event_id)
                            
                            # Counting the number of visits related to selected condition that provided selected service
                            
                            paste("for visits related to ", str_to_lower(input$condition2), "(", nrow(condition_data), 
                                  "visits with", input$service, ")")
                              }
                        })
                  
                  
                  # output$conj_service_plot: Polar bar plot of services provided in conjunction with selected sevice
                        
                        output$conj_service_plot <- renderPlot({
                          
                            # Filtering data by selected condition
                          
                            if (input$condition2 == "All") {
                              services_data <- app_data
                            }
                            
                            if (input$condition2 != "All") {
                              services_data <- app_data %>% 
                                filter(condition == input$condition2  | condition_2 == input$condition2 | 
                                         condition_3 == input$condition2 | condition_4 == input$condition2)
                            }
                            
                          
                            # Filtering for events that had the selected service using a semi_join
                          
                            with_service <- services_data %>% 
                              filter(service_received == input$service) %>% 
                              select(event_id)
                            
                            services_data %>% 
                              semi_join(with_service, by = "event_id") %>% 
                              mutate(conj_service = case_when(n_services_visit != 1 ~ service_received,
                                                              n_services_visit == 1 ~ "No other services")) %>% 
                              filter(conj_service != input$service) %>% 
                              
                              # Creating a table that has one observation per service different from selected service
                              # and the number of times that service was provided with selected service
                              
                              count(conj_service) %>% 
                              
                              # Creating a polar bar plot that shows most popular services to be provided with selected service,
                              # arranged by decreasing popularity
                              
                              ggplot(aes(x = fct_reorder(conj_service, -n), y = n, fill = fct_reorder(conj_service, -n))) +
                              geom_col(width = 1) +
                              scale_fill_manual(values = services_fill) +
                              labs(fill = "Additional Service") +
                              scale_x_discrete(labels = c("", "", "", "","", "", "", "", "", "", "", "", "")) + 
                              theme(aspect.ratio = 1) +
                              labs(x = NULL, y = NULL) +
                              coord_polar()
                        })
  
  
      ##################### EXPENDITURES PAGE ######################################################################################
            
            ##### DISTRIBUTION OF EXPENDITURES BY PAYER ##############################################
                  
                  # output$payer_n_visits: Text displaying the number of visits being viewed on the barplot
                        
                        output$payer_n_visits <- renderText({
                      
                          # Filtering data by selected condition
                          
                          if (input$condition_exp == "All") {
                            payers_data <- expenditure_data
                          }
                          
                          if (input$condition_exp != "All") {
                            payers_data <- expenditure_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | 
                                       condition_3 == input$condition_exp | condition_4 == input$condition_exp)
                          }
                          
                          
                          payer_visits <- payers_data %>%
                            
                            # Filtering data for selected payer types and total expenditure range
                            
                            filter(!payment_type %in% c("Total Facility Payment", "Total Doctor Payment"),
                                   payment_type %in% input$payer_type,
                                   `Total Expenditure` > input$exp_range[1], `Total Expenditure` <= input$exp_range[2]) %>%
                            
                            # Creating a table that has one observation per visit
                            
                            count(event_id)
                          
                          # Counting the number of visits that are within the expenditure range
                          
                          paste("Number of visits:", comma(nrow(payer_visits)))
                        })
              
                        
                  # output$payer_barplot: Barplot that displays proportion each payer paid for each visit
                        
                        output$payer_barplot <- renderPlot({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition_exp == "All") {
                            payers_data <- expenditure_data
                          }
                          
                          if (input$condition_exp != "All") {
                            payers_data <- expenditure_data %>% 
                              filter(condition == input$condition_exp  | condition_2 == input$condition_exp | 
                                       condition_3 == input$condition_exp | condition_4 == input$condition_exp)
                          }
                          
                          payers_data %>%
                            
                            # Filtering data for selected payer types and total expenditure range
                            
                            filter(!payment_type %in% c("Total Facility Payment", "Total Doctor Payment"),
                                   payment_type %in% input$payer_type,
                                   `Total Expenditure` > input$exp_range[1], `Total Expenditure` <= input$exp_range[2]) %>% 
                            
                            # Creating levels for payer type that make more sense than alphabetical
                            
                            mutate(fct_payment_type = case_when(payment_type == "Out of Pocket" ~ 1,
                                                                payment_type == "Medicaid" ~ 2,
                                                                payment_type == "Medicare" ~ 3,
                                                                payment_type == "Private Insurance" ~ 4,
                                                                payment_type == "Other Insurance" ~ 5)) %>%
                            
                            # Creating a barplot that shows the proportion each payer paid for each visit
                            
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
        
                        
            ##### DISTRIBUTION OF EXPENDITURES BY SERVICE ##############################################
       
                  # output$max_expenditure: Text that displays maximum expenditure for selected condition and services
                        
                        output$max_expenditure <- renderText({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition_exp2 == "All") {
                            expenditures_data <- app_data
                          }
                          
                          if (input$condition_exp2 != "All") {
                            expenditures_data <- app_data %>% 
                              filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | 
                                       condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2)
                          }
                          
                          expenditures_data <- expenditures_data %>% 
                            
                            # Filtering for selected service(s)
                            
                            filter(service_received %in% input$service_exp) %>% 
                            
                            # Isolating maximum expenditure
                            
                            arrange(desc(`Total Expenditure`)) %>% 
                            slice(1:1)
                         
                         paste0("Maximum expenditure: $", comma(expenditures_data$`Total Expenditure`))
                        })
              
                        
                  # output$median_expenditure: Text that displays the median expenditure for selected condition and services
                        
                        output$median_expenditure <- renderText({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition_exp2 == "All") {
                            expenditures_data <- app_data
                          }
                          
                          if (input$condition_exp2 != "All") {
                            expenditures_data <- app_data %>% 
                              filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | 
                                       condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2)
                          }
                          
                          expenditures_data <- expenditures_data %>% 
                            
                            # Filtering for selected service(s)
                            
                            filter(service_received %in% input$service_exp) %>% 
                            
                            # Creating table with one observation per visit which includes expenditure for that visit
                            
                            count(event_id, `Total Expenditure`) %>% 
                            
                            # Calculating median expenditure for the selected data
                            
                            summarize(median_expenditure = median(`Total Expenditure`))
                          
                          paste0("Median expenditure: $", comma(expenditures_data$median_expenditure))
                        })
              
                        
                  # output$expenditure_histogram: Histogram that shows the distribution of expenditures for visits with the 
                  # selected characteristics
                        
                        output$expenditure_histogram <- renderPlot({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition_exp2 == "All") {
                            expenditures_data <- app_data
                          }
                          
                          if (input$condition_exp2 != "All") {
                            expenditures_data <- app_data %>% 
                              filter(condition == input$condition_exp2  | condition_2 == input$condition_exp2 | 
                                       condition_3 == input$condition_exp2 | condition_4 == input$condition_exp2)
                          }
                          
                          expenditures_data %>% 
                            
                            # Filtering for selected service(s)
                            
                            filter(service_received %in% input$service_exp) %>% 
                            
                            # Creating non-zero expenditures so that log10 transformation does not result in infinite values
                            
                            mutate(`Total Expenditure` = `Total Expenditure` + 1) %>% 
                            
                            # Creating histogram of expenditures and the number of times each service was provided
                            
                            ggplot(aes(x = `Total Expenditure`, fill = service_received)) +
                            geom_histogram() +
                            scale_x_continuous(name = "Total Expenditure", 
                                               breaks = c(100, 1000, 10000, 100000), 
                                               labels = c("$100", "$1,000", "$10,000", "$100,000"), 
                                               trans = "log10") +
                            ylab("Number of times service was provided") +
                            scale_fill_manual(name = "Service", 
                                              values = services_fill) +
                            labs(caption = "***Note: Many expenditures included multiple services.")
                        })
              
              
            ##### DISTRIBUTION OF PAYMENTS BETWEEN DOCTORS AND FACILITIES ##############################################  
          
                  # output$dr_facility_stats: Text showing median and maximum payments to doctors and facilities
                        
                        output$dr_facility_stats <- renderText({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition_exp3 == "All") {
                            payers_data <- expenditure_data
                          }
                          
                          if (input$condition_exp3 != "All") {
                            payers_data <- expenditure_data %>% 
                              filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | 
                                       condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3)
                          }
                          
                          # Creating table with median and maximum payments doctors received within given total expenditure range
                          
                          dr_stats <- payers_data %>%
                            filter(payment_type == "Total Doctor Payment",
                                   `Total Expenditure` > input$exp_range2[1], `Total Expenditure` <= input$exp_range2[2]) %>% 
                            group_by(payment_type) %>% 
                            summarize(median = median(amount), maximum = max(amount))
                          
                          # Creating table with median and maximum payments facilities received within given total expenditure range
                          
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
                
                 # output$dr_facility_barplot: Barplot that shows the distribution of payments between doctors and facilities
                        output$dr_facility_barplot <- renderPlot({
                          
                          # Filtering data by selected condition
                          
                          if (input$condition_exp3 == "All") {
                            payers_data <- expenditure_data
                          }
                          
                          if (input$condition_exp3 != "All") {
                            payers_data <- expenditure_data %>% 
                              filter(condition == input$condition_exp3  | condition_2 == input$condition_exp3 | 
                                       condition_3 == input$condition_exp3 | condition_4 == input$condition_exp3)
                          }
                          
                          payers_data %>%
                            
                            # Filtering for facility and doctor payments within given total expenditure range
                            
                            filter(payment_type %in% c("Total Facility Payment", "Total Doctor Payment"),
                                   `Total Expenditure` > input$exp_range2[1], `Total Expenditure` <= input$exp_range2[2]) %>% 
                            
                            # Creating a bar plot showing breakdown of payments between doctors and facilities
                            
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
                        
        
      ##################### HIGH-USE PATIENTS PAGE #################################################################################
            
            ##### COMPARING COSTS FOR HIGH-USE AND LOW-USE PATIENTS ##############################################
                             
                  # output$high_low_med_exp: Text that shows median amount paid for high-use vs low-use patients
                        
                        output$high_low_med_exp <- renderText({
                          
                          text_data <- app_data %>% 
                            
                            # Calculating various payments for each event
                            
                            group_by(event_id, condition, condition_2, condition_3, condition_4, n_visits_2016) %>% 
                            summarize(`Total Expenditure` = mean(`Total Expenditure`),
                                      `Out of Pocket` = mean(`Out of Pocket`),
                                      `Medicaid` = mean(`Medicaid`),
                                      `Medicare` = mean(`Medicare`),
                                      `Private Insurance` = mean(`Private Insurance`),
                                      `Other Insurance` = mean(`Other Insurance`)) %>% 
                            ungroup() %>% 
                            
                            # Creating column with types of payers and how much they each paid. Each observation is a unique
                            # event/payer pair.
                            
                            gather(-event_id, -condition, -condition_2, -condition_3, -condition_4, -n_visits_2016,
                                   key = payment_type, value = `Amount Paid`) %>% 
                            
                            # Filtering for selected payer type and non-zero payments
                            
                            filter(payment_type == input$payment_type, `Amount Paid` != 0) %>% 
                            
                            # Classifying visits as those related to high-use or low-use patients
                            
                            mutate(type = case_when(n_visits_2016 < input$n_definition ~ "Low-use",
                                                     n_visits_2016 >= input$n_definition ~ "High-use")) %>% 
                            
                            # Determining median payment for high-use and low-use patients given selected payer(s)
                            
                            group_by(type) %>% 
                            summarize(median = median(`Amount Paid`)) %>% 
                            ungroup()
                          
                          paste0("Median for high-use patients: $", comma(subset(text_data, type == "High-use")$median), br(),
                                 "Median for low-use patients: $", comma(subset(text_data, type == "Low-use")$median))
                        })
              
                        
                  # output$high_low_exp: Histogram that shows distribution of payments for high-use vs low-use patients
                        
                        output$high_low_exp <- renderPlot({
                          
                          plot_data <- app_data %>% 
                            
                            # Calculating various payments for each event
                            
                            group_by(event_id, condition, condition_2, condition_3, condition_4, n_visits_2016) %>% 
                            summarize(`Total Expenditure` = mean(`Total Expenditure`),
                                      `Out of Pocket` = mean(`Out of Pocket`),
                                      `Medicaid` = mean(`Medicaid`),
                                      `Medicare` = mean(`Medicare`),
                                      `Private Insurance` = mean(`Private Insurance`),
                                      `Other Insurance` = mean(`Other Insurance`)) %>% 
                            ungroup() %>% 
                            
                            # Creating column with types of payers and how much they each paid. Each observation is a unique
                            # event/payer pair.
                            
                            gather(-event_id, -condition, -condition_2, -condition_3, -condition_4, -n_visits_2016,
                                   key = payment_type, value = `Amount Paid`) %>% 
                            
                            # Filtering for selected payer type and non-zero payments
                            
                            filter(payment_type == input$payment_type, `Amount Paid` != 0) %>% 
                            
                            # Classifying visits as those related to high-use or low-use patients
                            
                            mutate(type = case_when(n_visits_2016 < input$n_definition ~ "Low-use",
                                                     n_visits_2016 >= input$n_definition ~ "High-use"))
                          
                          # Creating overlaid histograms of payments for high-use versus low-use patients  
                          
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
              
              
            ##### COMPARING CONDITIONS OF HIGH-USE AND LOW-USE PATIENTS ##############################################
        
                  # output$one_condition: Title for bar plots if only one condition selected
                        
                        output$one_condition <- renderText({
                          if (input$top_n == 1) {
                            paste("Top condition for high-use vs. low-use patients")
                          }
                        })
            
                        
                  # output$multiple_conditions: Title for bar plots if multiple conditions selected
                        
                        output$multiple_conditions <- renderText({
                          if (input$top_n != 1) {
                            paste("Top", input$top_n,"conditions for high-use vs. low-use patients")
                          }
                        })
                        
              
                  # output$high_low_conditions: Bar plots that show top conditions for high-use and low use patients
                        
                        output$high_low_conditions <- renderPlot({
                          
                          high_low_conditions <- app_data %>% 
                            
                            # Classifying visits as those related to high-use or low-use patients
                            
                            mutate(type = case_when(n_visits_2016 < input$n_definition2 ~ "Low-use",
                                                    n_visits_2016 >= input$n_definition2 ~ "High-use")) %>% 
                            
                            # Creating table with one observation per visit
                            
                            group_by(type) %>% 
                            count(event_id, condition, condition_2, condition_3, condition_4) %>% 
                            
                            # Creating column that includes all conditions associated with each visit and gettin rid of
                            # unnecessary columns
                            
                            gather(-event_id, -n, -type, key = "condition_number", value = "conditions", na.rm = TRUE) %>%
                            select(type, event_id, conditions) %>% 
                            
                            # Creating column with total number of visits associated with high-use vs low-use patients
                            
                            mutate(total_type_visits = n_distinct(event_id)) %>% 
                            
                            # Counting the number of conditions by high-use and low-use patients while preserving total visits 
                            # in order to generate percentage of visits associtated with high-use vs low-use patients and each
                            # condition
                            
                            count(conditions, total_type_visits) %>% 
                            mutate(condition_percent = n/total_type_visits) %>% 
                            
                            # Isolating most common conditions
                            
                            arrange(type, desc(condition_percent)) %>% 
                            slice(1:input$top_n)
                          
                          # Creating bar plots that show the most common conditions and the percentage of visits they make up
                          # by patient type (high-use or low-use)
                          
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
