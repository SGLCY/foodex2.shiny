library(shiny)
library(shinyTree)
library(data.tree)
library(tidyverse)
library(stringr)
library(DT)
library(collapsibleTree)
library(shinydashboard)
library(here)
library(shinycssloaders)

options(shiny.maxRequestSize = 30*1024^2) 

mtx_parent <- readRDS("Data/mtx_parent.rds")
fdx2_chain_name <- readRDS("Data/fdx2_chain_name.rds")
fdx2_chain_code <- readRDS("Data/fdx2_chain_code.rds")
fdx2_chain_hierararchy <- readRDS("Data/fdx2_chain_hierarchy.rds")
mtx_levels <- fdx2_chain_code[c("termCode", "termExtendedName" ,"depth")]
fdx2_list_simple <- readRDS("Data/fdx2_list_simple.rds")
fdx2_list_explicit <- readRDS("Data/fdx2_list_explicit.rds")

# facets datasets
cooking_facets <- readxl::read_xlsx("Data/COOKING FACETS.xlsx") %>%
    select(termExtendedName = PROCESS, termCode=FOODEXCODE, depth = LEVEL)

process_facets <- readRDS("Data/process_facets.rds")

scenarios <- c("LB", "MB",  "UB")

source("R/functions_2.R", local = TRUE)


sub_categories <- c("Additive", "Pesticide", "Veterinary Drug Residue", "Contaminant", "Genotoxic-Carcinogen")
sub_ref_type <- c("Acceptable Intake", "Tolerable Intake", "Provisional Maximum Tolerable Intake", "Benchmark Dose Level (BMDL)")
sub_frequency <- c("DAILY"  =  1, "WEEKLY"  = 7)

vars_needed_occurrenceFdx2 <- c("termCode", "termExtendedName", "level", "N", "LB_mean", "LB_median", "MB_mean", "MB_median", "UB_mean", "UB_median")
vars_needed_consumptionFdx2 <- c("SERIAL", "SUBJECTID", "DAY", "FOODEX1", "FOODEX1_name", "AMOUNTFOOD", "FOODNAME", "FOODEXCODE", "GENDER", "AGE", "WEIGHT", "AREA", "POP_CLASS", "WCOEFF")

occurrence_summary <- list(
    #N      = ~n(),
    #min    = ~min(., na.rm = TRUE),
    mean   = ~mean(., na.rm = TRUE),
    #sd     = ~sd(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE)
    #max    = ~max(., na.rm = TRUE),
    #p95    = ~quantile(., 0.95)

    # Statistic on the LOD
)

# RAW DATASETS.
# As exported from Lims
occurrence_raw <- readxl::read_xlsx(
    "Data/FC_IMPRORISK_LEAD_2016_2017.xlsx"
) %>%
    mutate(
        termCode = str_extract(foodex2, "^.{5}")
    ) %>%
    left_join(mtx_levels, by = "termCode") %>%
    filter(!is.na(termCode))

occurrence_raw_name <- "FC_IMPRORISK_LEAD_2016_2017.xlsx"


# As is from EU MENU
consumption_raw <-
    readxl::read_xlsx(
        "Data/sample-Consumption_EUMENU_Lot1.xlsx"
    )

consumption_raw_name <- "sample-Consumption_EUMENU_Lot1.xlsx"


# SAMPLE DATASETS. READY FOR SHINY APP
# Shiny Ready. Fewer variables and renamed.
consumption_sample <- readRDS("Data/sample-Consumption_EUMENU-FDX2-LOT1.rds")

consumption_name <- "sample-Consumption_EUMENU-FDX2-LOT1"

# As exported from this app. Aggregated
path_occur <- "Data/ex1.Lead (Pb)-2016_2017.xlsx"
occurrence_sample <- readxl::read_xlsx(path_occur)
occurrence_name <- "ex1.Lead (Pb)-2016_2017.xlsx"

sub_name <- readxl::excel_sheets(path_occur)[[1]]

# UI ----------------------------------------------------------------------

ui <- fluidPage(
    
    theme = shinythemes::shinytheme("yeti"),
    # Application title
    titlePanel("Exposure at FoodEx2"),
    shinyWidgets::useShinydashboard(),
    shinyFeedback::useShinyFeedback(),
    
    sidebarLayout(
        
        sidebarPanel(width = 2,
                     
                     helpText(
                         h3("Instructions for each Tab")
                     ),
                     hr(),
                     conditionalPanel(
                         condition = "input.tabs == 'Aggregate Occurrence'",
                         h4("Aggregate Raw Occurrence"),
                         p("Here you create an aggregated occurrence file by food items"),
                         p("Select the food items you wish to aggregate their occurrence."),
                         p(strong("Note:"), "Selecting a food item, the full chain up to level 7 is selected
                       therefore use ", code("Overall"), " to select  specific item"),
                         p("This is to enable selecting a specific food item, and also 
                       it's parent item, (or grandparent etc.) and separate aggregations
                       will performed."),
                         hr(),
                         fileInput("occurrence_raw_file", "Upload Raw Occurrence", accept = ".xlsx"),
                         uiOutput("occur_raw_progress_UI")%>% 
                             shinycssloaders::withSpinner()
                     ),
                     conditionalPanel(
                         condition = "input.tabs == 'Occurrence'",
                         h4("Occurrence"),
                         p("Visualise the hierarchy that exists in your occurrence data"),
                         p("The size of the circle corresponds to the number of the immidiate children of the hierarchy"),
                         hr()
                     ),
                     conditionalPanel(
                         condition = "input.tabs == 'Exposure'",
                         h4("Calculate exposure at FoodEx2"),
                         #p("")
                         hr(),
                         fileInput("occurrence_file", "Upload Occurrence", accept = ".xlsx"),
                         uiOutput("occur_progress_UI")%>% 
                             shinycssloaders::withSpinner(),
                         hr(),
                         fileInput("consumption_file", "Upload consumption",  accept = ".xlsx"),
                         uiOutput("cons_progress_UI")%>% 
                             shinycssloaders::withSpinner()
                     ),
                     conditionalPanel(
                         condition = "input.tabs == 'FoodEx2'",
                         h4("FoodEx2"),
                         p("Visualise the full FoodEx2 hierarchy"),
                     ),
                     conditionalPanel(
                         condition = "input.tabs == 'Facets'",
                         h4("Explore Facets"),
                         p("Explore the Ingreient and Cooking facets"),
                     )
                     
                 
        ),
        
        
        mainPanel(width = 10,
                  tabsetPanel(id= "tabs",
                              
                              # tabPanel("FoodEx2 Tree",
                              #          shinyTree("fdx2_simple", checkbox = TRUE) %>% 
                              #              shinycssloaders::withSpinner(),
                              #          verbatimTextOutput("fdx2_sel")
                              #          
                              #          #shinyTree("fdx2_explicit", checkbox = TRUE)
                              # ),
                              
                              tabPanel("Aggregate Occurrence",
                                       
                                       p(" "),
                                       #       fluidRow(
                                       #           column(4,
                                       #                  helpText(
                                       #                      HTML("<p>Select the food items you wish to aggregate their 
                                       # occurrence<p><strong>Note:</strong>Selecting a food item, the full chain up to level 7 is selected
                                       # therefore use<code>Overall</code>to select  specific item.This is to enable selecting
                                       # a specific food item, and also it's parent item, (or grandparent etc.) and separate aggregations
                                       # will performed.</p>")
                                       #                  )
                                       #                  )
                                       #       ),
                                       strong("Click to expand the tree, or type in the box below to search"),
                                       fluidRow(
                                           
                                           column(5,
                                                  shinyTree("occurrence_TreeList", checkbox = TRUE, search = TRUE)  %>% 
                                                      shinycssloaders::withSpinner(),
                                                  
                                           ),
                                           column(7,
                                                  uiOutput("selectionsUI")
                                           )
                                       )
                                       
                                       
                              ),
                              tabPanel("Occurrence", 
                                       
                                       collapsibleTreeOutput("exposure_collapsible", height = "1000px")%>% 
                                           shinycssloaders::withSpinner()
                                       
                              ),
                              tabPanel("Exposure",
                                       h3("Overall Exposure estimates"),
                                       tags$hr(style="border-color: black;"),
                                       #h4("Exposure Statistics"),
                                       fluidRow(
                                           column(3,
                                                  box(title= "Substance Info", 
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      #tableOutput("subInfo_exposure"),
                                                      uiOutput("substance_infoUI") %>% 
                                                          shinycssloaders::withSpinner(),
                                                      width = NULL
                                                  ),
                                                  box(title="Exposure Statistics (μg/Kg b.w.)", 
                                                      #uiOutput("stats_label"),
                                                      br(),
                                                      tableOutput("tbl_exposure_stats") %>% 
                                                          shinycssloaders::withSpinner(),
                                                      #mod_downloadTable_ui("tbl_exposure_stats")
                                                      width = NULL 
                                                  ),
                                                  box(width = NULL,
                                                      title = "Customise tables & graphs",
                                                      collapsed = TRUE,
                                                      collapsible = TRUE,
                                                      
                                                      fluidRow(
                                                          column(width = 5,
                                                                 h5("Graphs"),
                                                                 numericInput("n.breaks",
                                                                              value = 10,
                                                                              min = 5,
                                                                              max = 30,
                                                                              step = 1,
                                                                              label = "Number of breaks"
                                                                 ),
                                                                 sliderInput("pct.digits_exposure",
                                                                             value = 1,
                                                                             min = 0,
                                                                             max = 3,
                                                                             step = 1,
                                                                             ticks = TRUE,
                                                                             label = "% decimals"
                                                                 ),
                                                                 shinyWidgets::prettyCheckbox(
                                                                     inputId = "show_stats_exposure",
                                                                     value = TRUE,
                                                                     label ="Show reference(s) on graph?",
                                                                     icon = icon("check"),
                                                                     status = "success"
                                                                 )
                                                                 
                                                          ),
                                                          column(width = 4,
                                                                 offset = 2,
                                                                 h5("Tables/Graphs"),
                                                                 numericInput("digits_exposure", 
                                                                              "Digits",
                                                                              value = 3, 
                                                                              min = 1, max = 10, 
                                                                              step = 1
                                                                 )
                                                          )
                                                          
                                                      )
                                                  )
                                                  
                                           ),
                                           column(7,
                                                  offset = 1,
                                                  box(title = "Distribution of exposure", 
                                                      width = NULL,
                                                      shinyWidgets::radioGroupButtons(
                                                          
                                                          inputId  = "slct_scenario_exposure",
                                                          label = "Select Scenario",
                                                          choices = scenarios,
                                                          selected = scenarios[2] #MB
                                                          
                                                      ),
                                                      tabBox(id = "graphs",width = NULL, 
                                                             title= "",
                                                             tabPanel(title = "PDF",
                                                                      plotOutput("exposure_pdf") %>% 
                                                                          shinycssloaders::withSpinner()
                                                                      #mod_downloadPlot_ui("exposurePDF")
                                                             ),
                                                             tabPanel(title = "CDF",
                                                                      plotOutput("exposure_cdf") %>% 
                                                                          shinycssloaders::withSpinner()
                                                                      #mod_downloadPlot_ui("exposureCDF")
                                                                      
                                                             )
                                                      )
                                                      
                                                  )
                                           )
                                           
                                       ) #  fluidRow
                                       
                              ),
                              tabPanel("FoodEx2",
                                       
                                       collapsibleTreeOutput("foodex2_collaps", height = "900px" )%>% 
                                           shinycssloaders::withSpinner()
                                       
                              ),
                              tabPanel("Facets",
                                       box(width = 10,
                                           title = h4("Explore the Facets in each food item"),
                                           tabBox(width = NULL,
                                                  
                                                  tabPanel("Ingredients",
                                                           DTOutput("facet_ingredient")  %>% 
                                                               shinycssloaders::withSpinner(),
                                                           DTOutput("facet_ingredient_count"),
                                                           downloadButton("down_ingredients", "Download")
                                                  ),
                                                  tabPanel("Cooking",
                                                           DTOutput("facet_cooking")  %>% 
                                                               shinycssloaders::withSpinner(),
                                                           downloadButton("down_cooking", "Download")
                                                  )
                                                  
                                           )
                                       )
                                       
                                      
                                      
                                       
                              )
                  )
                  
        )
    )
    
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    
    rv <- reactiveValues(
        
        scenario = NULL,
        title = NULL,
        title_statsDemo =  NULL,
        
        # initialise with the sample_ files
        #exposure_factor    = if(sample_substance_info$values[5]=="DAILY") {1} else {7},
        #exposure_frequency = sample_substance_info$values[5],
        #ref_value          = 0.05,#as.numeric(sample_substance_info$values[3]),
        #ref_value_type     = sample_substance_info$values[4],
        #sample_size        = nrow(sample_tbl_subjects) , 
        #pop_size           = sum(sample_tbl_subjects$wcoeff),
        
        demo               = c("gender" = "Gender", "area" ="Area", "pop_class" = "Population_Class"),
        
        show_level1 = NULL, #for the contribution tables
        
        x_label = "μg/Kg body weight", # somtime I will give the option to the user to change it
        y_label = "Population", # this might be `sample`? in case of non weighted
        
        
        #substance_info = sample_substance_info,
        exp_label = "μg/Kg body weight",
        
        tbl_stats_caption = "the cption"
    )
    
    dt_name <- reactiveValues(
        
        occurrence_raw = occurrence_raw_name,
        occurrence = occurrence_name,
        
        consumption_raw = consumption_raw_name,
        consumption  = consumption_name
    )
    
    datasets <- reactiveValues(
        
        occurrence_raw = occurrence_raw,  # As exported from LIMS
        
        occurrence = occurrence_sample, # Shiny ready. Aggregated
        
        consumption_raw = consumption_raw, # Like EU MENU. All info there
        
        consumption = consumption_sample, # SHiny Ready. New col names (etc..)e.g. SUBJECTID, FOODEX_name  , etc..
        
        
        placeholder  = NULL
    )
    
    
    substance_info <- reactiveValues(
        
        name = sub_name, #"Lead (Pb)",
        category = "Contaminant",
        ref_value = 0.15,
        type = "Benchamark Dose Level (BMDL)",
        frequency= "DAILY"
        
    )
    
    
    
    output$substance_infoUI <- renderUI({
        
        tagList(
            tags$table(
                tags$tr(width = "100%",
                        tags$td(width = "60%", div(style = "font-size:14px;", "Chemical Substance")),
                        tags$td(width = "40%", textInput(inputId = "substance_name", value = substance_info$name, label = NULL))),
                tags$tr(width = "100%",
                        tags$td(width = "60%", tags$div(style = "font-size:14pX;", "Substance Category")),
                        tags$td(width = "40%", selectInput(inputId = "substance_category", 
                                                           selected = substance_info$category, 
                                                           choices = sub_categories,
                                                           label = NULL))),
                tags$tr(width = "100%",
                        tags$td(width = "60%", tags$div(style = "font-size:14pX;", "Reference value (μg/Kg bw)")),
                        tags$td(width = "40%", numericInput(inputId = "substance_refValue", 
                                                            value = substance_info$ref_value, 
                                                            min  = 0,
                                                            label = NULL))),
                tags$tr(width = "100%",
                        tags$td(width = "60%", tags$div(style = "font-size:14pX;", "Type of reference value")),
                        tags$td(width = "40%", selectInput(inputId = "substance_type", 
                                                           selected = substance_info$type, 
                                                           choices = sub_ref_type,
                                                           label = NULL))),
                tags$tr(width = "100%",
                        tags$td(width = "60%", tags$div(style = "font-size:14pX;", "Frequency")),
                        tags$td(width = "40%", selectInput(inputId = "substance_frequency", 
                                                           selected = substance_info$frequency, 
                                                           choices = sub_frequency,
                                                           label = NULL)))
            )
            
            
        )
    })
    
    
    occur_nodes <- reactive({
        
        get_occurrence_nodes(datasets$occurrence_raw)
        
        })
    
    node_counts <- reactive(occur_nodes()$node_counts)
    occurrence_tree_code <- reactive(occur_nodes()$occurrence_tree_code)
    occurrence_tree_name <- reactive(occur_nodes()$occurrence_tree_name)
    
    
    occurrence_TreeList <- reactive(create_occurrence_TreeList(occurrence_tree_name()))
    
    
    
    #Read Files ####
    
    output$occur_raw_progress_UI <- renderUI({
        
        validate(
            need(input$occurrence_raw_file, "Import the Raw occurrence in .xlsx format")
        )
        
        file_type <- tools::file_ext(input$occurrence_raw_file$name)
        
        if(!file_type == "xlsx") {
            error_notExcel()
            validate("!Please import an .xlsx file")
        }
        
        path  = input$occurrence_raw_file$datapath
        data <-  readxl::read_xlsx(path)
        #check_varsOccurrenceFdx2(data, vars_needed_occurrenceFdx2)
        
        substance_name <- readxl::excel_sheets(path)[[1]] 
        
        #ALL OK (lets say..)
        show_success_alert("Occurence data are all set")
        
        # Update the occurence relevant files
        datasets$occurrence_raw <- data %>%
            mutate(
                termCode = str_extract(foodex2, "^.{5}")
            ) %>%
            left_join(mtx_levels, by = "termCode") %>%
            filter(!is.na(termCode))
        
        dt_name$occurrence_raw <- input$occurrence_raw_file$name
        
        #  after i Update the files!!!
        # because the below trigger the calculations
        #valid_occurrence_file(TRUE)
        
        tagList(
            p(paste0("Dataset: ", dt_name$occurrence_raw)),
            h4("Your Raw Occurrence data have been checked and succesfully uploaded",style= "colour: '#3CB371'")
        )
        
        
    })
    
    output$occur_progress_UI <- renderUI({
        
        
        validate(
            need(input$occurrence_file, "Import the occurrence in .xlsx format")
        )
        
        # Is it an Excel file?
        file_type <- tools::file_ext(input$occurrence_file$name)
        
        if(!file_type == "xlsx") {
            
            error_notExcel()
            validate("!Please import an .xlsx file")
        }
        
        # Correct sheets?
        #check_sheets_occur(input$occurrence_file$datapath)
        
        # OK, read and perform checks inside the sheet
        path  = input$occurrence_file$datapath
        
        data <-  readxl::read_xlsx(path)
        check_varsOccurrenceFdx2(data, vars_needed_occurrenceFdx2)
        
        substance_name <- readxl::excel_sheets(path)[[1]] 
        
        # not really needed beacuse I use the level 3 fro mappings, but lets
        #check_fdx1_descr(Level3, "level2")
        
        #ALL OK (lets say..)
        show_success_alert("Occurence data are all set")
        
        # Update the occurence relevant files
        datasets$occurrence <- data
        substance_info$name <- substance_name
        dt_name$occurrence <- input$occurrence_file$name
        #  after i Update the files!!!
        # because the below trigger the calculations
        #valid_occurrence_file(TRUE)
        
        tagList(
            p(paste0("Dataset: ", dt_name$occurrence)),
            h4("Your Occurrence data have been checked and succesfully uploaded",style= "colour: '#3CB371'")
        )
        
        
    })
    
    
    output$cons_progress_UI <- renderUI({
        
        validate(
            need(input$consumption_file, "Import the consumption in .xlsx format")
        )
        
        file_type <- tools::file_ext(input$consumption_file$name)
        
        if(!file_type == "xlsx") {
            
            error_notExcel()
            validate("!Please import an .xlsx file")
        }
        
        path = input$consumption_file$datapath
        
        data <- readxl::read_xlsx(path)
        
        # Perform checks
        check_varsConsumptionFdx2(data, vars_needed_consumptionFdx2)
        #check_fdx1_coding(Consumption)
        check_fewRows(data)
        
        datasets$consumption <- data
        dt_name$consumption <- input$consumption_file$name
        
        # After I update the file
        #since the following triggers calculation
        #valid_consumption_file(TRUE)
        
        tagList(
            p(paste0("Dataset: ", dt_name$consumption)),
            tags$h4("Your Consumption data have been checked and succesfully uploaded",style= "colour: '#3CB371'")
        )
        
    })
    
    #  ffff####
    
    output$fdx2_simple <- renderTree({
        
        fdx2_list_simple
    })
    
    
    output$fdx2_sel <- renderPrint({
        
        # names(as.data.frame(get_selected(input$occurrence_TreeList,format = "names")))
        get_selected(input$fdx2_simple,format = "names")
    })
    
    
    
    output$foodex2_collaps <- renderCollapsibleTree({
        
        collapsibleTree(
            fdx2_chain_name,
            hierarchy  = paste0("level", 1:7),
            zoomable = TRUE,
            root  = "FoodeX2",
            nodeSize = "count",
            width = 900
        )
        
    })
    
    output$exposure_collapsible <- renderCollapsibleTree({
        
        #level = input$level
        
        hierarchy <- paste0("level", 1:7)
        
        collapsibleTree(
            occurrence_tree_name(),
            hierarchy = hierarchy,
            zoomable = TRUE,
            root  = "Exposure",
            tooltip = TRUE,
            nodeSize = "count",
            fontSize = 12,
            width = 900
        )
    })
    
    output$occurrence_TreeList <- renderTree({
        
        datasets$occurrence_raw
        occurrence_TreeList()
        
    })
    
    
    occurrence_selections <- reactive({
        
        #dependency on new file
        datasets$occurrence_raw
        
        
        occur_list <- shinyTree::get_selected(input$occurrence_TreeList,format = "names")
        
        req(length(occur_list) > 0)
        
        as_table <- get_selected_termCodes(occur_list)
        
        #final_table_counts 
        as_table %>% 
            left_join(node_counts()[c("termCode", "n")], by = c("termCode")) %>% 
            arrange(level) 
        
    })
    
    
    occurrence_template <- reactive({
        
        occurrence_selections <- occurrence_selections()
        
        
        food_levels <- paste0("level", unique(occurrence_selections$level))
        food_codes <- unique(occurrence_selections$termCode)
        food_names <- unique(occurrence_selections$termExtendedName)
        
        
        food_levels %>% 
            set_names() %>% 
            map(
                ~ aggregate_by_level(datasets$occurrence_raw, occurrence_tree_code(), .)
            ) %>% 
            map(
                ~filter(., termCode %in% food_codes)
            ) %>% 
            bind_rows(
                .id = "level"
            ) %>% 
            left_join(mtx_levels[c("termCode","termExtendedName")]) %>% 
            relocate(
                termCode, termExtendedName
            )
        
        
    })
    
    output$occurrence_template <- renderDT({
        
        occurrence_template() %>% 
            mutate(
                across(matches("LB|MB|UB"), ~ round(.,4))
            ) %>% 
            datatable(
                caption = "Aggregated Occurence Values",
                style = "bootstrap4",
                options = list(
                    #pageLength = 10,lengthChange = FALSE,
                    paging = FALSE,
                    scrollX = TRUE, scrollY = "600px"
                )
                
            )
    })
    
    output$occurrence_selections <- DT::renderDataTable({
        
        occurrence_selections() %>% 
            rename(Samples= n) %>% 
            datatable(
                caption = "Your selected food items and number of samples",
                style = "bootstrap",
                options = list(
                    pageLength = 20  
                )
                
            )
    })
    
    
    output$selectionsUI <- renderUI({
        
        req(length(occurrence_selections()) >0 )
        
        tagList(
            tabBox(id = "occur_tables",
                   width = 12,
                   tabPanel("Selections",
                            DT::DTOutput("occurrence_selections")
                   ),
                   tabPanel("Aggregated",
                            DT::DTOutput("occurrence_template"),
                            downloadButton("down_occurrence", "Download Occurence")
                   )
            )
            
            
            
        )
    })
    
    output$down_occurrence <- downloadHandler(
        
        
        filename = function(){
            
            #"occurence_selected.rds"
            # substace_name can be a reactive? see ?downloadHandler
            paste0("occurrence-", dt_name$occurrence_raw, ".xlsx")
        },
        
        content = function(file){
            
            #saveRDS(occur_sel(),file)
            writexl::write_xlsx(occurrence_template(), path = file)
        }
    )
    
    output$down_ingredients <- downloadHandler(
        
        
        filename = function(){
            
            #"occurence_selected.rds"
            # substace_name can be a reactive? see ?downloadHandler
            paste0("FACETS-INGREDIENTS-", dt_name$consumption_raw, ".xlsx")
        },
        
        content = function(file){
            
            #saveRDS(occur_sel(),file)
            writexl::write_xlsx(facet_ingredient(), path = file)
        }
    )
    
    output$down_cooking <- downloadHandler(
        
        
        filename = function(){
            
            #"occurence_selected.rds"
            # substace_name can be a reactive? see ?downloadHandler
            paste0("FACETS-COOKING-", dt_name$consumption_raw, ".xlsx")
        },
        
        content = function(file){
            
            #saveRDS(occur_sel(),file)
            writexl::write_xlsx(facet_cooking(), path = file)
        }
    )
    
    # FACETS ####
    
    
    # Ingredient
    
    facet_ingredient <- reactive({
        
        create_facet_table(datasets$consumption_raw,"F04", mtx_levels) %>% 
            select(ORSUBCODE,RECORDIDENTIFIER,
                   DAY,
                   FOODEXCODE, ORFOODCODE,
                   AMOUNTFRAW,
                   AMOUNTRECIPE,ENRECIPEDESC,
                   ORFOODNAME, fdx2_name,
            facet, facet_name
            ) %>%
            mutate(
                across(!contains("AMOUNT"), factor)
            )
        
    })
    
    
    output$facet_ingredient <- renderDT({
        
        facet_ingredient() %>% 
            relocate(FOODEXCODE, .after = last_col()) %>% 
            datatable(
                caption = "Ingredient Facets",
                style = "bootstrap",
                filter = 'top',
                rownames = FALSE,
                options = list(
                    pageLength = 30,
                    autoWidth = TRUE,
                    #paging = FALSE,
                    scrollX = TRUE, scrollY = "600px"
                )
                
            )
    })
    
    
    facet_ingredient_count <- reactive({
        
        facet_ingredient() %>% 
            count(facet, facet_name, fdx2_name,  sort = TRUE) %>% 
            janitor::get_dupes(facet) %>% 
            select(
                Facet = facet,
                Ingredient = facet_name,
                'FoodEx2 Name' = fdx2_name,
                "N occassions" = n
            )
    })
    
    output$facet_ingredient_count <- renderDT({
        facet_ingredient_count() %>% 
            datatable(
                caption = "Ingredient Facets",
                style = "bootstrap",
                filter = 'top',
                rownames = FALSE,
                options = list(
                    pageLength = 30,
                    autoWidth = TRUE,
                    #paging = FALSE,
                    scrollX = TRUE, scrollY = "600px"
                )
                
            )
        })
    
    # Cooking
    facet_cooking <- reactive({
        
        
        create_facet_table(datasets$consumption_raw,"F28", cooking_facets) %>% 
            mutate(
                across(!contains("AMOUNT"), factor)
            )
        
        
    })
    
    output$facet_cooking_count <- renderTable({
        
        facet_cooking() %>% 
            count(facet, facet_name, sort = TRUE)
    })
    
    output$facet_cooking <- renderDT({
        
        facet_cooking() %>% 
            relocate(FOODEXCODE, .after = last_col()) %>% 
            datatable(
                caption = "Cooking Facets",
                style = "bootstrap",
                filter = 'top',
                options = list(
                    pageLength = 30,
                    autoWidth = TRUE,
                    #paging = FALSE,
                    scrollX = TRUE, scrollY = "600px"
                )
                
            )
    })
    
    
    
    # EXPOSURE ASSESSMENT ####
    tbl_consumption <- reactive({
        
        
        datasets$consumption
        
        
    })
    
    tbl_occurrence <- reactive({
        
        datasets$occurrence
    })
    
    output$tbl_consumption <- renderTable(tbl_consumption())
    
    tbl_subjects <- reactive({
        
        tbl_consumption() %>% 
            rename_all(tolower) %>% 
            create_tbl_subjects_fdx2()
    })
    
    output$tbl_subject <- renderTable({
        tbl_subjects()
    })
    
    
    tbl_merged <- reactive({
        
        create_tbl_merged_fdx2(tbl_consumption(), tbl_occurrence(), fdx2_chain_hierararchy, mtx_levels)
        
    })
    
    output$tbl_merged <- renderTable({
        
        tbl_merged()
    })
    
    tbl_exposure <- reactive({
        
        req(input$substance_frequency)
        exposure_factor <- as.numeric(input$substance_frequency)
        create_tbl_exposure_fdx2(tbl_merged(), tbl_subjects(), exposure_factor)
        
    })
    
    output$tbl_exposure <- renderTable({
        
        tbl_exposure()
    })
    
    
    tbl_exposure_stats <- reactive({
        
        #req(input$digits_exposure)
        req(tbl_exposure())
        
        digits  <- input$digits_exposure
        ref_value <- input$substance_refValue  #substance_info$ref_value
        
        tbl_stats <- 
        tbl_exposure() %>% 
            tidyr::pivot_longer(
                cols = starts_with("subExp_"),
                names_to = "scenario",
                values_to = "exposure"
            ) %>% 
            dplyr::group_by(scenario) %>% 
            summarise_weighted(ref_value = ref_value) %>% 
            mutate(pctOver  = percent(pctOver)) %>% 
            mutate_at(vars(-scenario, -pctOver), ~ round(.,digits)) %>% 
            tidyr::gather(key, value, - scenario) %>%
            tidyr::pivot_wider(names_from = scenario, values_from = value) %>% 
            {.} %>% 
            dplyr::rename_with(
                ~stringr::str_remove(., "subExp_"),
                dplyr::starts_with("subExp_")
            ) 
        
        if(input$substance_type == "Benchmark Dose Level (BMDL)"){
            tbl_stats <- tbl_stats %>% filter(key != "pctOver")
        } else {
            tbl_stats <- tbl_stats %>% filter(key != "MOE")
        }
        tbl_stats
    })
    
    output$tbl_exposure_stats <- renderTable({
        
            
        # if( input$substance_type == "Benchmark Dose Level (BMDL)"){
        #     as.character(p(br(),"MOE: Margin of Exposure"))
        # } else {
        #     as.character(p(br(),"pctOver: % of population over the reference value"))
        # }
        # 
        tbl_exposure_stats()%>% 
            data.frame(row.names = "key")
        
    }
    ,rownames = TRUE
    #,digits = function() input$digits_exposure
    #,caption = caption #as.character(p(br(),"pctOver: % of population over the reference value"))
    , caption = NULL #function()rv$tbl_stats_caption %>% as.character()
    )
    
    # Graphs ####
    exposure_pdf <- reactive({
        
        req(input$slct_scenario_exposure)
        
        #ref_value  <- input$substance_refValue #substance_info$ref_value%||%NA_real_  #NULL ref_value brakes down the plot. Thanks @_ColinFay
        scenario   <- input$slct_scenario_exposure
        
        var_to_use <- paste0("subExp_",scenario)
        title      <- glue::glue("Probability distribution of exposure at the {scenario} scenario")
        x_label    <- rv$x_label
        y_label    <- rv$y_label
        
        n.breaks   <- input$n.breaks
        digits     <- input$digits_exposure
        accuracy   <- 1/(10^input$pct.digits_exposure)
        
        add_stats  <- input$show_stats_exposure
        
        validate(
            need(n.breaks>=5 && n.breaks <=  30, 
                 glue::glue(
                     "# of breaks should be:>=  {5} and <= {30}"
                 )
            )
        )
        
        exp_plot <- pdf_exposure(tbl_exposure(),
                                 var_exp = var_to_use,
                                 bins  = n.breaks +1,
                                 digits = digits,
                                 accuracy = accuracy
        )
        
        if(add_stats){
            exp_plot <- 
                exp_plot+
                geom_vline(aes(xintercept=median(.data[[var_to_use]], na.rm = TRUE),
                               color="Median exposure"), linetype="dashed",
                           size=1) +
                geom_vline(aes(xintercept=mean(.data[[var_to_use]], na.rm = TRUE),
                               color="Mean exposure"), linetype="dotted",
                           size=1) +
                # geom_vline(aes(xintercept=ref_value,  
                #                color="Reference value"), linetype="dotted",
                #            size=1) +
                scale_color_manual(name = "Statistics", values = c('Median exposure' = "blue", 
                                                                   'Mean exposure' = "red"
                                                                   #, 'Reference value' = "black"
                )
                )
        }
        
        # Add the labs
        exp_plot <- 
            exp_plot +
            labs(
                title = title,
                x     = x_label,
                y     = y_label
            )+
            NULL
        
        
        catch_plotError(exp_plot)
        
        exp_plot
        
    })
    
    output$exposure_pdf <- renderPlot({
        
        exposure_pdf()
        
    })
    
    
    exposure_cdf <- reactive({
        
        ref_value <- input$substance_refValue #substance_info$ref_value
        
        scenario <- input$slct_scenario_exposure
        var_to_use <- paste0("subExp_",scenario)
        
        title <- glue::glue("Cummulative distribution of exposure at the {scenario} scenario")
        x_label <- rv$x_label
        y_label <- rv$y_label
        
        cdf_plot <- 
            cdf_exposure(tbl_exposure(),
                         var_exp = var_to_use,
                         ref_value = ref_value
            )+
            labs(
                title = title,
                x  = x_label,
                y = y_label
            )+
            NULL
        
        catch_plotError(cdf_plot)
        
        cdf_plot
    })
    
    output$exposure_cdf <- renderPlot({
        
        exposure_cdf()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
