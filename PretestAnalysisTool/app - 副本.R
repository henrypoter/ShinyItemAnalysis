library(shiny)
library(ShinyItemAnalysis)
library(psych)
library(ggplot2)
library(ggdendro)
library(mirt)
library(knitr)
library(rmarkdown)

# Increase max upload size
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  titlePanel("Pre-test Analysis Tool (Based on ShinyItemAnalysis)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Input"),
      actionButton("load_demo", "Load Demo Data (GMAT)", icon = icon("table")),
      tags$br(), tags$br(),
      fileInput("file", "Upload Data (.csv)",

                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header", "Header", TRUE),
      
      fileInput("key_file", "Upload Answer Key (Optional, .csv)",
                multiple = FALSE,
                accept = c(".csv")),
      helpText("If Key is provided, Distractor Analysis will be available. Data should be raw responses. If Key is not provided, Data is assumed to be Scored (0/1)."),
      
      tags$hr(),
      h4("2. Variable Selection"),
      uiOutput("var_select_ui"),
      
      tags$hr(),
      h4("3. Method Settings"),
      selectInput("irt_type", "IRT Model:",
                  choices = c("Rasch", "2PL", "3PL", "Graded Response"),
                  selected = "2PL"),
      
      tags$hr(),
      downloadButton("downloadReport", "Generate & Download Report")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 h3("Scored Data Preview"),
                 tableOutput("contents")
        ),
        
        tabPanel("Validity",
                 h3("Predictive Validity (Score vs Criterion)"),
                 plotOutput("validity_scatter"),
                 verbatimTextOutput("validity_cor"),
                 
                 h3("Construct Validity"),
                 h4("Polychoric Correlation Heatmap"),
                 plotOutput("corr_heatmap"),
                 h4("Scree Plot (Parallel Analysis)"),
                 plotOutput("scree_plot"),
                 h4("Factor Diagram (EFA - 1 Factor)"),
                 plotOutput("fa_diagram")
        ),
        
        tabPanel("Reliability",
                 h3("Internal Consistency (Cronbach's Alpha)"),
                 verbatimTextOutput("alpha_output")
        ),
        
        tabPanel("Item Analysis (CTT)",
                 h3("Difficulty & Discrimination Plot"),
                 plotOutput("dd_plot"),
                 h3("Item Statistics Table"),
                 tableOutput("item_stat_table"),
                 conditionalPanel(
                   condition = "output.key_present == true",
                   h3("Distractor Analysis"),
                   uiOutput("distractor_select_ui"),
                   plotOutput("distractor_plot")
                 )
        ),
        
        tabPanel("IRT Analysis",
                 h3("Wright Map"),
                 plotOutput("wright_map"),
                 h3("Item Characteristic Curves (ICC)"),
                 plotOutput("icc_plot"),
                 h3("Test Information Function"),
                 plotOutput("test_info_plot"),
                 h3("Item Fit Statistics"),
                 tableOutput("fit_stats")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to hold data
  v <- reactiveValues(data = NULL, key = NULL)
  
  observeEvent(input$file, {
    tryCatch({
      v$data <- read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
      v$key <- NULL # Reset key on new data upload
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$key_file, {
    tryCatch({
      k <- read.csv(input$key_file$datapath, header = FALSE, stringsAsFactors = FALSE)
      if (nrow(k) > 1) k <- k[1, ]
      v$key <- unlist(k)
      showNotification("Key loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading key:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_demo, {
    # Load demo data logic
    # We use GMATtest (raw) and GMATkey from difNLR if available, or fallback
    if (requireNamespace("difNLR", quietly = TRUE)) {
      data(GMATtest, package = "difNLR")
      data(GMATkey, package = "difNLR")
      data(GMAT, package = "difNLR")
      
      # Use raw data + criterion
      df <- GMATtest[, 1:20]
      df$Criterion <- GMAT[, "criterion"]
      
      v$data <- df
      v$key <- as.character(GMATkey)
      
      showNotification("Demo Data (GMAT) loaded. Includes Raw Responses and Key.", type = "message")
    } else {
      # Fallback if difNLR not installed: use ShinyItemAnalysis::HCI (Already scored)
      data(HCI, package = "ShinyItemAnalysis")
      v$data <- HCI[, 1:20]
      v$key <- NULL
      showNotification("difNLR package not found. Loaded HCI data (Scored) instead.", type = "warning")
    }
  })

  # --- Data Loading & Processing ---
  
  rawData <- reactive({
    req(v$data)
    v$data
  })
  
  keyData <- reactive({
    v$key
  })
  
  output$key_present <- reactive({
    !is.null(keyData())
  })
  outputOptions(output, "key_present", suspendWhenHidden = FALSE)
  
  output$var_select_ui <- renderUI({
    req(rawData())
    df <- rawData()
    vars <- names(df)
    # Default selection: All numerical/integer columns or all columns
    tagList(
      selectInput("item_vars", "Select Items (Questions):", choices = vars, multiple = TRUE, selected = vars[1:min(20, length(vars))]),
      selectInput("criterion_var", "Select Criterion (Optional):", choices = c("None", vars), selected = "None")
    )
  })
  
  # Scored Data (The data used for analysis)
  scoredData <- reactive({
    req(rawData(), input$item_vars)
    raw <- rawData()[, input$item_vars, drop = FALSE]
    
    key <- keyData()
    if (!is.null(key)) {
      # Check dimension
      if (length(key) != ncol(raw)) {
        showNotification("Key length does not match number of selected items!", type = "error")
        return(NULL)
      }
      # Scoring
      scored <- raw
      for (i in 1:ncol(raw)) {
        scored[, i] <- as.integer(raw[, i] == key[i])
      }
      scored
    } else {
      # Assume already scored
      # Basic check: should be numeric
      if (!all(sapply(raw, is.numeric))) {
         # Try to coerce
         raw[] <- lapply(raw, function(x) as.numeric(as.character(x)))
      }
      raw
    }
  })
  
  criterionData <- reactive({
    req(rawData(), input$criterion_var)
    if (input$criterion_var == "None") return(NULL)
    rawData()[[input$criterion_var]]
  })
  
  # --- Outputs ---
  
  output$contents <- renderTable({
    req(scoredData())
    head(scoredData(), 10)
  }, caption = "Showing first 10 rows of analyzed (scored) data")
  
  # 1. Validity
  
  validity_scatter_plot <- reactive({
    req(scoredData(), criterionData())
    score <- rowSums(scoredData(), na.rm = TRUE)
    criterion <- criterionData()
    df <- data.frame(TotalScore = score, Criterion = criterion)
    
    # Heuristic for discrete vs continuous criterion
    if (length(unique(criterion)) < 10) {
      ggplot(df, aes(y = TotalScore, x = as.factor(Criterion), fill = as.factor(Criterion))) +
        geom_boxplot() +
        geom_jitter(shape = 16, position = position_jitter(0.2)) +
        scale_fill_brewer(palette = "Blues") +
        labs(x = "Criterion Group", y = "Total Score", fill = "Group") +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "none")
    } else {
      ggplot(df, aes(x = TotalScore, y = Criterion)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(x = "Total Score", y = "Criterion Variable") +
        theme_bw()
    }
  })
  
  output$validity_scatter <- renderPlot({
    validity_scatter_plot()
  })
  
  output$validity_cor <- renderPrint({
    req(scoredData(), criterionData())
    score <- rowSums(scoredData(), na.rm = TRUE)
    cor.test(criterionData(), score)
  })
  
  corr_heatmap_plot <- reactive({
    req(scoredData())
    # Drop items with 0 variance to avoid errors
    data_clean <- scoredData()[, apply(scoredData(), 2, var, na.rm=TRUE) != 0, drop=FALSE]
    if (ncol(data_clean) < 2) return(NULL)
    
    ShinyItemAnalysis::plot_corr(data_clean, cor = "polychoric", clust_method = "ward.D2", n_clust = min(3, ncol(data_clean)-1))
  })
  
  output$corr_heatmap <- renderPlot({
    corr_heatmap_plot()
  })
  
  scree_plot_obj <- reactive({
    req(scoredData())
    # psych::fa.parallel plots directly, doesn't return ggplot. 
    # We capture plot in UI, but for report we need to reproduce it.
    # We will use recordPlot or just let it plot.
    psych::fa.parallel(scoredData(), fa = "fa", plot = FALSE) 
  })
  
  output$scree_plot <- renderPlot({
    res <- scree_plot_obj()
    plot(res)
  })
  
  fa_diagram_plot <- reactive({
    req(scoredData())
    psych::fa(scoredData(), nfactors = 1)
  })
  
  output$fa_diagram <- renderPlot({
    psych::fa.diagram(fa_diagram_plot())
  })
  
  # 2. Reliability
  
  alpha_res <- reactive({
    req(scoredData())
    psych::alpha(scoredData(), check.keys = TRUE)
  })
  
  output$alpha_output <- renderPrint({
    alpha_res()
  })
  
  # 3. Item Analysis
  
  dd_plot_obj <- reactive({
    req(scoredData())
    ShinyItemAnalysis::DDplot(scoredData(), k = 3, l = 1, u = 3)
  })
  
  output$dd_plot <- renderPlot({
    dd_plot_obj()
  })
  
  item_stat_res <- reactive({
    req(scoredData())
    ShinyItemAnalysis::ItemAnalysis(scoredData())
  })
  
  output$item_stat_table <- renderTable({
    item_stat_res()
  }, rownames = TRUE)
  
  # Distractor
  output$distractor_select_ui <- renderUI({
    req(input$item_vars)
    selectInput("dist_item", "Select Item for Distractor Plot:", choices = input$item_vars, selected = input$item_vars[1])
  })
  
  distractor_plot_obj <- reactive({
    req(rawData(), input$dist_item, keyData())
    
    # We need Raw Data and Key
    item_idx <- which(names(rawData()) == input$dist_item)
    if(length(item_idx) == 0) return(NULL)
    
    # We need the INDEX of the item in the dataset passed to plotDistractorAnalysis
    # And we need the data to only include the items, or specified columns
    
    data_items <- rawData()[, input$item_vars, drop = FALSE]
    key_vec <- keyData()
    
    # Find index in the subset
    idx_in_subset <- which(input$item_vars == input$dist_item)
    
    ShinyItemAnalysis::plotDistractorAnalysis(data_items, key_vec, num.group = 3, item = idx_in_subset)
  })
  
  output$distractor_plot <- renderPlot({
    distractor_plot_obj()
  })
  
  # 4. IRT
  
  mirt_fit <- reactive({
    req(scoredData())
    data <- scoredData()
    model_type <- input$irt_type
    
    itemtype <- switch(model_type,
                       "Rasch" = "Rasch",
                       "2PL" = "2PL",
                       "3PL" = "3PL",
                       "Graded Response" = "graded")
    
    mirt::mirt(data, 1, itemtype = itemtype, verbose = FALSE, SE = TRUE)
  })
  
  wright_map_obj <- reactive({
    req(mirt_fit())
    fit <- mirt_fit()
    
    # Get params
    # Depending on model, b param location varies
    coefs <- coef(fit, IRTpars = TRUE, simplify = TRUE)
    
    if (is.list(coefs) && "items" %in% names(coefs)) {
      items <- coefs$items
      if ("b" %in% colnames(items)) {
         b <- items[, "b"]
      } else if ("d" %in% colnames(items)) {
          # For graded, etc.
          # Simplification: Use first threshold or average? 
          # WrightMap usually for dichotomous.
          b <- items[, grep("d", colnames(items))] 
      } else {
         return(NULL)
      }
    } else {
      return(NULL)
    }
    
    fs <- as.vector(fscores(fit))
    ShinyItemAnalysis::ggWrightMap(fs, b)
  })
  
  output$wright_map <- renderPlot({
    wright_map_obj()
  })
  
  output$icc_plot <- renderPlot({
    req(mirt_fit())
    plot(mirt_fit(), type = "trace", facet_items = FALSE)
  })
  
  output$test_info_plot <- renderPlot({
    req(mirt_fit())
    plot(mirt_fit(), type = "infoSE")
  })
  
  output$fit_stats <- renderTable({
    req(mirt_fit())
    mirt::itemfit(mirt_fit())
  })
  
  # --- Report ---
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Pretest_Report_", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      
      # Show notification
      showNotification("Generating report... This may take a few seconds.", type = "message")
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      # Ensure report.Rmd exists in the same dir as app.R or copy it from app dir
      if(file.exists("report.Rmd")) {
         file.copy("report.Rmd", tempReport, overwrite = TRUE)
      } else {
         stop("report.Rmd not found!")
      }

      # Prepare params
      # We pass the REACTIVE objects evaluated
      # Note: For plots that are plot objects (ggplot), we can pass them.
      # For plots that are side-effects (plot()), we cannot pass them easily unless we wrap them in a function or capture them.
      
      # Helper to save plot to file and return path? No, knitr can handle plot objects.
      # But standard plot() output (like plot(mirt_fit)) is tricky.
      # mirt plots are lattice/ggplot objects usually.
      
      # Let's verify mirt plot types.
      # plot(mirt) -> lattice object.
      
      params <- list(
        validity_scatter = validity_scatter_plot(),
        corr_heatmap = corr_heatmap_plot(),
        scree_plot_res = scree_plot_obj(), # fa.parallel result
        fa_res = fa_diagram_plot(),
        alpha_res = alpha_res(),
        dd_plot = dd_plot_obj(),
        item_stat = item_stat_res(),
        wright_map = wright_map_obj(),
        mirt_object = mirt_fit(), # Pass model to re-plot in Rmd
        item_fit = mirt::itemfit(mirt_fit())
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
