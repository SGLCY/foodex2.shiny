library(shiny)
library(shinyTree)
library(data.tree)
library(tidyverse)
library(DT)
library(collapsibleTree)
library(shinydashboard)
library(here)
library(shinycssloaders)


mtx_parent <- readRDS("Data/mtx_parent.rds")
fdx2_chain_name <- readRDS("Data/fdx2_chain_name.rds")
fdx2_chain_code <- readRDS("Data/fdx2_chain_code.rds")
fdx2_chain_hierararchy <- readRDS("Data/fdx2_chain_hierarchy.rds")
mtx_levels <- fdx2_chain_code[c("termCode", "termExtendedName" ,"depth")]
fdx2_list_simple <- readRDS("Data/fdx2_list_simple.rds")
fdx2_list_explicit <- readRDS("Data/fdx2_list_explicit.rds")

scenarios <- c("LB", "MB",  "UB")

source("R/functions_2.R", local = TRUE)


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
    "Data/FC_IMPRORISK_METALS_2016_2017.xlsx"
) %>%
    mutate(
        termCode = str_extract(foodex2, "^.{5}")
    ) %>%
    left_join(mtx_levels, by = "termCode") %>%
    filter(!is.na(termCode))

# As is from EU MENU
consumption_raw <- 
    readxl::read_xlsx(
        "Data/Consumption_EUMENU_mapped_fdx2_fdx1 Lot1.xlsx"
    )



substance_name <- "METALS_2016_2017"

# SAMPLE DATASETS. READY FOR SHINY APP
# Shiny Ready. Fewer variables and renamed.
consumption_sample <- readRDS("Data/Consumption_EUMENU-FDX2-LOT1.rds")
# As exported from this app. Aggregated
occurrence_sample <- readxl::read_xlsx("Data/occurrence-METALS_2016_2017.xlsx")

# UI ----------------------------------------------------------------------

ui <- fluidPage(
    
    # Application title
    titlePanel("Exposure at FoodEx2"),
    shinyWidgets::useShinydashboard(),
    
    sidebarLayout(
        
        sidebarPanel(width = 2,
                     
                     helpText(
                         h3("Instructions for each Tab")
                     ),
                     hr(),
                     h4("Aggregate Occurrence"),
                     p("Select the food items you wish to aggregate their 
                       occurrence."),
                     p(strong("Note:"), "Selecting a food item, the full chain up to level 7 is selected
                       therefore use ", code("Overall"), " to select  specific item"),
                     p("This is to enable selecting a specific food item, and also 
                       it's parent item, (or grandparent etc.) and separate aggregations
                       will performed."),
                     hr(),
                     h4("Occurrence"),
                     p("Visualise the hierarchy that exists in your occurrence data"),
                     p("You can limit the level that is shown"),
                     p("The size of the circle corresponds to the muber of the immidiate children of the hierarchy"),
                     hr(),
                     h4("FoodEx2"),
                     p("Visualise the full FoodEx2 hierarchy"),
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
                                       # selectInput("level", "Show up to Level:", 
                                       #             as.character(c(1:6)),
                                       #             selected = "3",
                                       #             width = "100px"
                                       # ),
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
                                                      #tableOutput("subInfo_exposure"),
                                                      #tableOutput("tbl_consumption"),
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
        ref_value          = 0.05,#as.numeric(sample_substance_info$values[3]),
        #ref_value_type     = sample_substance_info$values[4],
        #sample_size        = nrow(sample_tbl_subjects) , 
        #pop_size           = sum(sample_tbl_subjects$wcoeff),
        
        demo               = c("gender" = "Gender", "area" ="Area", "pop_class" = "Population_Class"),
        
        show_level1 = NULL, #for the contribution tables
        
        x_label = "μg/Kg body weight", # somtime I will give the option to the user to change it
        y_label = "Population", # this might be `sample`? in case of non weighted
        
        
        #substance_info = sample_substance_info,
        exp_label = "μg/Kg body weight"
    )
    
    
    datasets <- reactiveValues(
        
        occurrence_raw = occurrence_raw,  # As exported from LIMS
        
        occurrence = occurrence_sample,
        
        consumption_raw = consumption_raw, # Like EU MENU. All info there
        
        consumption = consumption_sample, # SHiny Ready. New col names (etc..)e.g. SUBJECTID, FOODEX_name  , etc..
        
        
        placeholder  = NULL
    )
    
    occur_nodes <- reactive({get_occurrence_nodes(datasets$occurrence_raw)})
    
    node_counts <- reactive(occur_nodes()$node_counts)
    occurrence_tree_code <- reactive(occur_nodes()$occurrence_tree_code)
    occurrence_tree_name <- reactive(occur_nodes()$occurrence_tree_name)
    
    
    occurrence_TreeList <- reactive(create_occurrence_TreeList(occurrence_tree_name()))
    
    
    
    
    output$fdx2_simple <- renderTree({
        
        fdx2_list_simple
    })
    
    
    output$fdx2_sel <- renderPrint({
        
        # names(as.data.frame(get_selected(input$occurrence_TreeList,format = "names")))
        get_selected(input$fdx2_simple,format = "names")
    })
    
    
    
    output$foodex2_collaps <- renderCollapsibleTree({
        
        collapsibleTreeSummary(
            fdx2_chain_name,
            hierarchy  = paste0("level", 1:7),
            zoomable = TRUE,
            root  = "FoodeX2",
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
        
        occurrence_TreeList()
        
    })
    
    
    occurrence_selections <- reactive({
        
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
                style = "bootstrap",
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
            paste0("occurrence-", substance_name, ".xlsx")
        },
        
        content = function(file){
            
            #saveRDS(occur_sel(),file)
            writexl::write_xlsx(occurrence_template(), path = file)
        }
    )
    
    
    
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
        
        create_tbl_exposure_fdx2(tbl_merged(), tbl_subjects(), 1)
        
    })
    
    output$tbl_exposure <- renderTable({
        
        tbl_exposure()
    })
    
    
    tbl_exposure_stats <- reactive({
        
        #req(input$digits_exposure)
        
        digits  <- input$digits_exposure
        ref_value <- rv$ref_value
        
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
    })
    
    output$tbl_exposure_stats <- renderTable({
        
        #style the pctOver row
        ind <- match("pctOver",tbl_exposure_stats()$key)
        
        tbl_exposure_stats()%>% 
            data.frame(row.names = "key")
        
    }
    ,rownames = TRUE
    #,digits = function() input$digits_exposure
    ,caption = as.character(p(br(),"pctOver: % of population over the reference value"))
    )
    
    # Graphs ####
    exposure_pdf <- reactive({
        
        req(input$slct_scenario_exposure)
        
        ref_value  <- rv$ref_value%||%NA_real_  #NULL ref_value brakes down the plot. Thanks @_ColinFay
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
            need(n.breaks>5 && n.breaks <=  30, 
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
        
        
        #catch_plotError(exp_plot)
        
        exp_plot
        
    })
    
    output$exposure_pdf <- renderPlot({
        
        exposure_pdf()
        
    })
    
    
    exposure_cdf <- reactive({
        
        ref_value <- rv$ref_value
        
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
        
        #catch_plotError(cdf_plot)
        
        cdf_plot
    })
    
    output$exposure_cdf <- renderPlot({
        
        exposure_cdf()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
