library(shiny)
library(ShinyItemAnalysis)
library(psych)
library(ggplot2)
library(ggdendro)
library(mirt)
library(knitr)
library(rmarkdown)
library(readxl)

# Increase max upload size
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  titlePanel("预试分析工具 (Pre-test Analysis Tool)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. 数据输入 (Data Input)"),
      actionButton("load_demo", "加载示例数据 (GMAT)", icon = icon("table")),
      helpText("如果没有数据，点击上方按钮加载示例数据进行体验。"),
      tags$br(), tags$br(),
      fileInput("file", "上传数据 (CSV/Excel)",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls")),
      checkboxInput("header", "包含表头 (Header)", TRUE),
      
      fileInput("key_file", "上传答案键 (可选, CSV/Excel)",
                multiple = FALSE,
                accept = c(".csv", ".xlsx", ".xls")),
      helpText("上传的数据应为原始作答数据。如果未提供答案键，默认数据已计分(0/1)。"),
      
      tags$hr(),
      h4("2. 变量选择 (Variable Selection)"),
      uiOutput("var_select_ui"),
      helpText("请选择题目变量。"),
      
      tags$br(),
      actionButton("run_analysis", "开始分析 (Run Analysis)", class = "btn-primary", width = "100%"),
      helpText("点击按钮后执行分析和图表更新。"),
      
      tags$hr(),
      h4("3. 方法设置 (Method Settings)"),
      selectInput("irt_type", "IRT 模型 (用于单独的IRT分析):",
                  choices = c("Rasch", "2PL", "3PL", "Graded Response"),
                  selected = "2PL"),
      helpText("在'预试报告'一栏中，默认使用 Graded Response Model (GRM) 进行参数估计。"),
      
      tags$hr(),
      h4("4. 导出报告"),
      radioButtons("report_format", "报告格式:", choices = c("HTML", "Markdown (.zip)"), selected = "HTML"),
      downloadButton("downloadReport", "生成并下载报告")
    ),
    
    mainPanel(
      tabsetPanel(
        # New Tab: Pre-test Report
        tabPanel("预试报告 (总览)",
                 h3("预试结果摘要 (Pre-test Summary)"),
                 p("本页面展示了预试数据的核心指标，包含描述性统计、信度、IRT拟合指数及项目参数。此报告采用 Graded Response Model (GRM) 估计，适用于多级计分及二分数据。"),
                 
                 h4("1. 诊断建议及判断标准 (Diagnostics)"),
                 uiOutput("diagnostics_text"),
                 
                 h4("2. 整体拟合指数 (Global Fit Indices)"),
                 helpText("M2统计量及其相关指数用于评估IRT模型的整体拟合度。RMSEA < 0.06, CFI/TLI > 0.95 通常表示拟合良好。"),
                 tableOutput("global_fit_table"),
                 
                 h4("3. 信度分析 (Reliability)"),
                 helpText("McDonald's Omega 和 Cronbach's Alpha 用于评估内部一致性信度。通常建议 > 0.7。"),
                 tableOutput("reliability_table"),
                 
                 h4("4. 项目参数表 (Item Parameters - GRM)"),
                 helpText("基于GRM模型估算的区分度 (a) 和 截距/阈值相关参数 (d_k)。"),
                 tableOutput("grm_params_table"),
                 
                 h4("5. 描述性统计 (Descriptive Statistics)"),
                 tableOutput("desc_stats_table"),
                 
                 h4("6. 反应频数分布 (Response Frequency)"),
                 plotOutput("response_freq_plot", height = "400px")
        ),
        
        tabPanel("数据预览 (Data)", 
                 h3("计分数据预览"),
                 tableOutput("contents")
        ),
        
        tabPanel("效度 (Validity)",
                 h3("预测效度 (总分 vs 效标)"),
                 helpText("展示总分与外在效标变量的关系。"),
                 plotOutput("validity_scatter"),
                 verbatimTextOutput("validity_cor"),
                 
                 h3("结构效度"),
                 h4("多格相关热图 (Polychoric Correlation Heatmap)"),
                 helpText("基于多格相关系数的题目间相关矩阵热图，并进行聚类。"),
                 plotOutput("corr_heatmap"),
                 h4("碎石图 (Scree Plot - Parallel Analysis)"),
                 helpText("平行分析用于确定建议保留的因子个数。"),
                 plotOutput("scree_plot"),
                 h4("因子路径图 (EFA - 1 Factor)"),
                 plotOutput("fa_diagram")
        ),
        
        tabPanel("信度 (Reliability Detail)",
                 h3("内部一致性 (Cronbach's Alpha)"),
                 verbatimTextOutput("alpha_output")
        ),
        
        tabPanel("项目分析 (CTT)",
                 h3("难度与区分度图 (Difficulty & Discrimination)"),
                 helpText("展示经典测量理论下的题目难度（通过率）和区分度（题总相关）。"),
                 plotOutput("dd_plot"),
                 h3("项目统计表"),
                 tableOutput("item_stat_table"),
                 conditionalPanel(
                   condition = "output.key_present == true",
                   h3("干扰项分析 (Distractor Analysis)"),
                   helpText("展示各选项的选择比例随总分分组的变化情况。"),
                   uiOutput("distractor_select_ui"),
                   plotOutput("distractor_plot")
                 )
        ),
        
        tabPanel("IRT 分析 (详细)",
                 h3("怀特图 (Wright Map)"),
                 helpText("同时展示受试者能力分布（左侧）和题目难度分布（右侧）。"),
                 plotOutput("wright_map"),
                 h3("项目特征曲线 (ICC)"),
                 helpText("展示作答正确（或选择某选项）的概率随能力值的变化。"),
                 plotOutput("icc_plot"),
                 h3("测验信息函数 (TIF)"),
                 helpText("展示测验在不同能力水平上提供的信息量（测量精度的指标）。"),
                 plotOutput("test_info_plot"),
                 h3("项目拟合统计量"),
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
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("xlsx", "xls")) {
        v$data <- as.data.frame(readxl::read_excel(input$file$datapath, col_names = input$header))
      } else {
        v$data <- read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
      }
      v$key <- NULL # Reset key
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$key_file, {
    tryCatch({
      ext <- tools::file_ext(input$key_file$name)
      if (ext %in% c("xlsx", "xls")) {
        k <- as.data.frame(readxl::read_excel(input$key_file$datapath, col_names = FALSE))
      } else {
        k <- read.csv(input$key_file$datapath, header = FALSE, stringsAsFactors = FALSE)
      }
      
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
  
  # Reactive for the raw data used for variable selection (updates immediately upon load)
  rawDataPre <- reactive({
    req(v$data)
    v$data
  })
  
  # Trigger analysis only when button is pressed
  # We use eventReactive to lock the downstream data until logic runs
  analysisTrigger <- eventReactive(input$run_analysis, {
    list(items = input$item_vars, criterion = input$criterion_var)
  })
  
  # Data used for analysis - depends on Trigger
  scoredData <- reactive({
    # Require trigger
    trig <- analysisTrigger()
    req(v$data)
    
    # Use variables from trigger snapshot
    items <- trig$items
    if(length(items) == 0) return(NULL)
    
    raw <- v$data[, items, drop = FALSE]
    
    # Scoring logic...
    key <- v$key
    if (!is.null(key)) {
      # Check dimension
      if (length(key) != ncol(raw)) {
        showNotification("答案键长度与题目数量不匹配!", type = "error")
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
      if (!all(sapply(raw, is.numeric))) {
         # Try to coerce
         raw[] <- lapply(raw, function(x) as.numeric(as.character(x)))
      }
      raw
    }
  })
  
  # Raw data subset for analysis (e.g. for distractors)
  rawDataAnalysis <- reactive({
    trig <- analysisTrigger()
    req(v$data)
    v$data[, trig$items, drop = FALSE]
  })
  
  criterionData <- reactive({
    trig <- analysisTrigger()
    req(v$data)
    if (trig$criterion == "None") return(NULL)
    v$data[[trig$criterion]]
  })
  
  output$key_present <- reactive({
    !is.null(v$key)
  })
  outputOptions(output, "key_present", suspendWhenHidden = FALSE)
  
  output$var_select_ui <- renderUI({
    req(rawDataPre())
    df <- rawDataPre()
    vars <- names(df)
    tagList(
      selectInput("item_vars", "选择题目变量 (Items):", choices = vars, multiple = TRUE, selected = vars[1:min(20, length(vars))]),
      selectInput("criterion_var", "选择效标变量 (Criterion, 可选):", choices = c("None", vars), selected = "None")
    )
  })
  
  # --- Outputs ---
  
  output$contents <- renderTable({
    req(scoredData())
    head(scoredData(), 10)
  }, caption = "展示前10行已计分数据")
  
  # --- New Tab Calculations ---
  
  # GRM Model for Summary
  fit_grm <- reactive({
    req(scoredData())
    data <- scoredData()
    # Remove columns with zero variance to prevent errors
    data <- data[, apply(data, 2, var, na.rm=TRUE) != 0, drop=FALSE]
    mirt::mirt(data, 1, itemtype = "graded", verbose = FALSE, SE = FALSE) # SE=FALSE for speed
  })
  
  # Global fit
  output$global_fit_table <- renderTable({
    req(fit_grm())
    tryCatch({
      fit_stats <- mirt::M2(fit_grm())
      fit_stats
    }, error = function(e) {
      data.frame(Message = "无法计算M2统计量 (可能存在缺失数据或数据太稀疏)")
    })
  })
  
  # Reliability Omega
  output$reliability_table <- renderTable({
    req(scoredData())
    data <- scoredData()
    
    # Omega
    om <- tryCatch({
      psych::omega(data, nfactors = 1, plot = FALSE)$omega.tot
    }, error = function(e) NA)
    
    # Alpha
    al <- tryCatch({
      psych::alpha(data, check.keys=TRUE)$total$raw_alpha
    }, error = function(e) NA)
    
    data.frame(
      Index = c("Cronbach's Alpha", "McDonald's Omega"),
      Value = c(al, om),
      Rule = c("> 0.7", "> 0.7")
    )
  })
  
  # GRM Parameters
  output$grm_params_table <- renderTable({
    req(fit_grm())
    coefs <- coef(fit_grm(), simplify = TRUE)
    if(is.list(coefs) && "items" %in% names(coefs)) {
      as.data.frame(coefs$items)
    } else {
      NULL
    }
  }, rownames = TRUE)
  
  # Descriptive Stats
  output$desc_stats_table <- renderTable({
    req(scoredData())
    desc <- psych::describe(scoredData())
    desc <- desc[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")]
    desc$Item <- rownames(desc)
    desc[, c("Item", "n", "mean", "sd", "min", "max", "skew", "kurtosis")]
  })
  
  # Response Frequencies
  output$response_freq_plot <- renderPlot({
    req(rawDataAnalysis())
    # Use raw responses for frequency
    data <- rawDataAnalysis()
    # Gather for ggplot
    data_long <- data.frame(
      Item = rep(names(data), each = nrow(data)),
      Response = as.vector(as.matrix(data))
    )
    # Remove NA
    data_long <- na.omit(data_long)
    
    ggplot(data_long, aes(x = as.factor(Response))) +
      geom_bar(fill = "steelblue") +
      facet_wrap(~Item, scales = "free_x") +
      labs(x = "Response Option", y = "Frequency") +
      theme_bw()
  })
  
  # Diagnostics Text
  output$diagnostics_text <- renderUI({
    req(fit_grm(), scoredData())
    
    f <- fit_grm()
    # Check simple convergence or stats (conceptual)
    m2 <- tryCatch(mirt::M2(f), error=function(e) NULL)
    
    advice <- list()
    advice[[1]] <- tags$li(strong("信度判断: "), "如果 Alpha 或 Omega < 0.7, 建议检查是否存在低区分度或负相关的题目。")
    
    if(!is.null(m2)) {
      if(!is.na(m2$RMSEA) && m2$RMSEA > 0.08) {
        advice[[length(advice)+1]] <- tags$li(strong("拟合判断: "), "RMSEA > 0.08, 模型拟合欠佳。建议移除拟合差的题目或考虑多维模型。")
      } else {
         advice[[length(advice)+1]] <- tags$li(strong("拟合判断: "), "RMSEA < 0.08, 模型整体拟合尚可.")
      }
    }
    
    tags$ul(advice)
  })
  
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
    req(analysisTrigger()) # Wait for analysis trigger/vars
    vars <- analysisTrigger()$items
    selectInput("dist_item", "选择题目 (用于干扰项分析图):", choices = vars, selected = vars[1])
  })
  
  distractor_plot_obj <- reactive({
    req(rawDataPre(), input$dist_item, v$key, analysisTrigger())
    
    # We need Raw Data (subset) and Key
    trig <- analysisTrigger()
    
    # Check if dist_item is in current analysis set
    if(!(input$dist_item %in% trig$items)) return(NULL)
    
    data_items <- v$data[, trig$items, drop = FALSE]
    key_vec <- v$key
    
    # Find index in the subset
    idx_in_subset <- which(trig$items == input$dist_item)
    
    # Use ShinyItemAnalysis plot
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
      if(input$report_format == "HTML") {
        paste("Pretest_Report_", Sys.Date(), ".html", sep="")
      } else {
        paste("Pretest_Report_", Sys.Date(), ".zip", sep="")
      }
    },
    content = function(file) {
      
      showNotification("正在生成报告，请稍候...", type = "message")
      
      # Use a robust temp directory approach
      temp_dir <- tempdir()
      tempReport <- file.path(temp_dir, "report.Rmd")
      
      # Ensure report source exists
      if(file.exists("report.Rmd")) {
         file.copy("report.Rmd", tempReport, overwrite = TRUE)
      } else {
         # Try full path if relative fails (in case working dir changed)
         app_dir_report <- file.path(getwd(), "report.Rmd")
         if(file.exists(app_dir_report)) {
            file.copy(app_dir_report, tempReport, overwrite = TRUE)
         } else {
            # Fallback for deployed scenarios or strict pathing
            stop("无法找到 report.Rmd 模板文件。")
         }
      }

      # Calculate all objects before rendering to ensure they are available
      # Especially mirt objects which might need to be passed carefully
      
      # Snapshots
      snap_validity_scatter <- tryCatch(validity_scatter_plot(), error=function(e) NULL)
      snap_corr_heatmap <- tryCatch(corr_heatmap_plot(), error=function(e) NULL)
      snap_scree <- tryCatch(scree_plot_obj(), error=function(e) NULL)
      snap_fa <- tryCatch(fa_diagram_plot(), error=function(e) NULL)
      snap_alpha <- tryCatch(alpha_res(), error=function(e) NULL)
      snap_dd <- tryCatch(dd_plot_obj(), error=function(e) NULL)
      snap_item_stat <- tryCatch(item_stat_res(), error=function(e) NULL)
      snap_wright <- tryCatch(wright_map_obj(), error=function(e) NULL)
      snap_mirt <- tryCatch(mirt_fit(), error=function(e) NULL)
      snap_item_fit <- if(!is.null(snap_mirt)) tryCatch(mirt::itemfit(snap_mirt), error=function(e) NULL) else NULL
      snap_desc <- tryCatch(psych::describe(scoredData()), error=function(e) NULL)
      snap_grm_indices <- if(!is.null(fit_grm())) tryCatch(mirt::M2(fit_grm()), error=function(e) NULL) else NULL
      snap_grm_coefs <- if(!is.null(fit_grm())) {
             cf <- coef(fit_grm(), simplify=T); 
             if(is.list(cf)) cf$items else NULL 
      } else NULL

      params <- list(
        validity_scatter = snap_validity_scatter,
        corr_heatmap = snap_corr_heatmap,
        scree_plot_res = snap_scree,
        fa_res = snap_fa,
        alpha_res = snap_alpha,
        dd_plot = snap_dd,
        item_stat = snap_item_stat,
        wright_map = snap_wright,
        mirt_object = snap_mirt, 
        item_fit = snap_item_fit,
        desc_stats = snap_desc,
        grm_indices = snap_grm_indices,
        grm_coefs = snap_grm_coefs
      )
      
      out_fmt <- if(input$report_format == "HTML") "html_document" else "md_document"
      
      # Render
      out_file <- rmarkdown::render(tempReport, output_format = out_fmt,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      if(input$report_format == "HTML") {
        file.copy(out_file, file)
      } else {
        # ZIP logic
        # We need to zip the .md file and the _files folder
        
        # Get paths relative to temp_dir
        out_file_name <- basename(out_file)
        files_dir_name <- paste0(tools::file_path_sans_ext(out_file_name), "_files")
        
        # We must change wd to temp_dir to zip correctly with relative paths
        owd <- setwd(temp_dir)
        on.exit(setwd(owd))
        
        files_to_zip <- c(out_file_name)
        if(dir.exists(files_dir_name)) {
          files_to_zip <- c(files_to_zip, files_dir_name)
        }
        
        # Use utils::zip or fallback
        # On Windows, utils::zip needs external tool. zip::zip is safer if installed.
        # But we probably don't have zip package.
        # Try utils::zip.
        utils::zip(file, files = files_to_zip)
      }
    }
  )
}

shinyApp(ui, server)
