
# Load packages ----------------------------------

#install.packages("rsconnect")
library(rsconnect)

#install.packages("shiny")
library(shiny)

#install.packages("DT")
library(DT)

#install.packages("tidyverse")
library(tidyverse)

library(rio)

# Pulling in functions
source("functions.R")

# Import data -----------------------------------

converted_data <- import("converted_data.csv") 
appendix_data <- meta_clean_shiny(converted_data)

# Connect to server -----------------------------

rsconnect::setAccountInfo(name = 'kdenning', 
                          token = 'AD70F9499EDA69122245913A14E213AF', 
                          secret = 'xW5J/GbOnICJlPk3vGgvaPQyhu3o4EzrN29joLZE')

# Begin UI --------------------------------------

ui <- fluidPage(
    navbarPage(
        title = "Meta-analysis table generator",
    tabPanel(
        "Tables",
        sidebarLayout(
            sidebarPanel(width = 3,
                         strong("Instructions:"),
                         br(),
                         "Below you can manipulate the perspective taking 
                         instruction/comparison combination and outcome category 
                         used to produce the table. Press 'Generate to produce 
                         the table. You can download each table you produce into
                         a .csv.",
                         tags$hr(style="border-color: black;"),
                         fixedRow(radioButtons(
                             inputId = "pt_instruct",
                             label = "Perspective taking/comparison combination:",
                             choices = c("Self PT vs Day control",
                                         "Self PT vs Other control",
                                         "Self PT vs Objective",
                                         "Other PT vs Day control",
                                         "Other PT vs Other control",
                                         "Other PT vs Objective"),
                             selected = "Self PT vs Day control"
                         )
                         ),
                         fixedRow(radioButtons(
                             inputId = "outcome_cat",
                             label = "Outcome category:",
                             choices = c("Interpersonal Feels", 
                                         "Overlap", 
                                         "Stereotyping"),
                             selected = "Interpersonal Feels"
                             )
                         ),
                         column(
                             12, 
                             align = "center", 
                             actionButton(
                                 inputId = "generate", 
                                 label = "Generate"
                                 )
                             ),
                         br()
                
            ),
        mainPanel(
            fixedRow(DTOutput('effect_size_tbl')),
            br(),
            "Download .csv:",
            downloadButton("download", "Download")
            )
        )
        
    ),
    tabPanel(
        "Documentation",
        br(),
        strong("This corresponds with Kathryn Denning's preliminary exam at University of Oregon. 
        An updated version will be submitted for publication."),
        br(),
        br(),
        strong("Variable Dictionary"),
        br(),
        "- 'Study per article' - corresponding study out of that paper that the effect size was extracted from.",
        br(),
        "- 'Effect # total' - The sample out of all samples collected, counted sequentially from the first alphabetical study in our entire data-set.
        Included in this table to distinguish dependent effect sizes due to multiple instruction sizes.",
        br(),
        "-'Sample # total' - Indicates if, within that study, there were multiple unique samples.",
        br(),
        "- 'g' - Hedge's g; bias corrected Cohen's d",
        br(),
        "- 'v' - variance around g",
        br(),
        "- 'n for pt' - sample size for perspective taking instruction condition",
        br(),
        "- 'n for comparison' - sample size for comparison condition",
        br(),
        br(),
        "*OSF Repo link will be added here with all other materials when available"
    )
    )

)

# Begin server -------------------------------------

server <- function(input, output){
    data_4_static_tbl <- data.frame(head(appendix_data)) 
    
    data_4_static_tbl_clean <- data_4_static_tbl %>% 
        select(-pt_comparison, -outcome_type,) %>% 
        mutate(dunb = round(dunb, digits = 2),
               var_dunb = round(var_dunb, digits = 2)) %>% 
        rename("Author(s)" = authors,
               "Effect # total" = effect_size_num,
               "Study per article" = study_num_per_article,
               "Sample # total" = sample_number_total,
               "g" = dunb,
               "v" = var_dunb,
               "n for pt" = n_pt,
               "n for comparison" = n_comparison)
    
    output$effect_size_tbl <- DT::renderDT(
        DT::datatable(data_4_static_tbl_clean,
                      options = list(initComplete = JS( # changes the banner color
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': 'rgba(60, 179, 113, .80)', 'color': '#000'});",
                                         "}"))))
    observeEvent(
        input$generate, {
            results <- filtered_appendix(
                appendix_data, 
                input$pt_instruct, 
                input$outcome_cat)
            
            results <- data.frame(results)
            
            inputted_df <- results %>% 
                mutate(dunb = round(dunb, digits = 2),
                       var_dunb = round(var_dunb, digits = 2)) %>% 
                rename("Author(s)" = authors,
                       "Effect # total" = effect_size_num,
                       "Study per article" = study_num_per_article,
                       "Sample # total" = sample_number_total,
                       "g" = dunb,
                       "v" = var_dunb,
                       "n for pt" = n_pt,
                       "n for comparison" = n_comparison) 
            
            reactive_df <- reactive({inputted_df})
            
            output$effect_size_tbl <- DT::renderDT(
                DT::datatable(reactive_df(),
                              options = list(initComplete = JS( # changes the banner color
                                                 "function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': 'rgba(60, 179, 113, .80)', 'color': '#000'});",
                                                 "}"))))
            
            # Downloadable csv of status
            output$download <- downloadHandler(
                filename = function() {
                    paste("effect_size-data-", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(inputted_df, file, row.names = FALSE)
                }
            )
                
        }
    )
}

shinyApp(ui = ui, server = server)

