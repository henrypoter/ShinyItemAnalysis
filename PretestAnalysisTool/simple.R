library(ShinyItemAnalysis)
library(psych)
library(ggplot2)
library(ggdendro)
library(mirt)
library(knitr)
library(rmarkdown)
library(readxl)

#' Run Pre-test Analysis and Generate Report
#'
#' @param data_path Path to the data file (.csv, .xlsx, .xls)
#' @param key_path Path to the key file (.csv, .xlsx, .xls). Optional.
#' @param output_dir Directory to save the report. Default is current directory.
#' @param report_name Name of the output report (without extension).
#' @param items Vector of column names to select as items. If NULL, uses all columns.
#' @param criterion Column name for criterion variable. Optional.
#'
#' @return Path to the generated report
#' @export
run_pretest_analysis <- function(data_path, key_path = NULL, output_dir = ".", report_name = "Pretest_Report", 
                                 items = NULL, criterion = NULL) {
  
  message("Loading data...")
  # 1. Load Data
  ext <- tools::file_ext(data_path)
  if (ext %in% c("xlsx", "xls")) {
    raw_data <- as.data.frame(readxl::read_excel(data_path))
  } else {
    raw_data <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE)
  }
  
  if (is.null(items)) {
    # If items not specified, assume all columns except criterion are items
    if (!is.null(criterion)) {
      items <- setdiff(names(raw_data), criterion)
    } else {
      items <- names(raw_data)
      # Heuristic: remove non-numeric columns if blindly selecting
      nums <- sapply(raw_data, is.numeric)
      items <- names(raw_data)[nums]
    }
  }
  
  analysis_data <- raw_data[, items, drop = FALSE]
  
  # 2. Key & Scoring
  key <- NULL
  scored_data <- analysis_data
  
  if (!is.null(key_path)) {
    k_ext <- tools::file_ext(key_path)
    if (k_ext %in% c("xlsx", "xls")) {
      k_df <- as.data.frame(readxl::read_excel(key_path, col_names = FALSE))
    } else {
      k_df <- read.csv(key_path, header = FALSE, stringsAsFactors = FALSE)
    }
    
    if (nrow(k_df) > 1) k_df <- k_df[1, ]
    key <- unlist(k_df)
    
    if (length(key) != ncol(analysis_data)) {
      warning("Key length does not match number of items. Skip scoring.")
    } else {
      for (i in 1:ncol(analysis_data)) {
        scored_data[, i] <- as.integer(analysis_data[, i] == key[i])
      }
    }
  } else {
    # Helper to clean numeric
     if (!all(sapply(scored_data, is.numeric))) {
         scored_data[] <- lapply(scored_data, function(x) as.numeric(as.character(x)))
      }
  }
  
  # Clean data (remove zero variance for IRT)
  # But for stats we keep them to show they are bad
  # For IRT we strictly filter
  scored_data_irt <- scored_data[, apply(scored_data, 2, var, na.rm=TRUE) != 0, drop=FALSE]
  
  message("Running analyses...")
  
  # --- Analyses ---
  
  # 1. Validity
  validity_scatter <- NULL
  if (!is.null(criterion) && criterion %in% names(raw_data)) {
    crit_vec <- raw_data[[criterion]]
    score <- rowSums(scored_data, na.rm = TRUE)
    df <- data.frame(TotalScore = score, Criterion = crit_vec)
    
    if (length(unique(crit_vec)) < 10) {
      validity_scatter <- ggplot(df, aes(y = TotalScore, x = as.factor(Criterion), fill = as.factor(Criterion))) +
        geom_boxplot() +
        geom_jitter(shape = 16, position = position_jitter(0.2)) +
        scale_fill_brewer(palette = "Blues") +
        labs(x = "Criterion Group", y = "Total Score", fill = "Group") +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "none")
    } else {
      validity_scatter <- ggplot(df, aes(x = TotalScore, y = Criterion)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(x = "Total Score", y = "Criterion Variable") +
        theme_bw()
    }
  }
  
  corr_heatmap <- NULL
  if (ncol(scored_data_irt) > 1) {
    corr_heatmap <- tryCatch(
      ShinyItemAnalysis::plot_corr(scored_data_irt, cor = "polychoric", clust_method = "ward.D2", n_clust = min(3, ncol(scored_data_irt)-1)),
      error = function(e) NULL
    )
  }
  
  scree_plot_res <- tryCatch(psych::fa.parallel(scored_data_irt, fa = "fa", plot = FALSE), error = function(e) NULL)
  fa_res <- tryCatch(psych::fa(scored_data_irt, nfactors = 1), error = function(e) NULL)
  
  # 2. Reliability
  alpha_res <- tryCatch(psych::alpha(scored_data, check.keys = TRUE), error = function(e) NULL)
  
  # 3. CTT
  dd_plot <- tryCatch(ShinyItemAnalysis::DDplot(scored_data, k = 3, l = 1, u = 3), error = function(e) NULL)
  item_stat <- tryCatch(ShinyItemAnalysis::ItemAnalysis(scored_data), error = function(e) NULL)
  
  # 4. IRT - GRM
  message("Fitting IRT model (This may take a moment)...")
  fit_grm <- tryCatch(
    mirt::mirt(scored_data_irt, 1, itemtype = "graded", verbose = FALSE, SE = FALSE),
    error = function(e) NULL
  )
  
  wright_map <- NULL
  mirt_object <- fit_grm
  item_fit <- NULL
  grm_indices <- NULL
  grm_coefs <- NULL
  
  if (!is.null(fit_grm)) {
    # Wright Map
    coefs <- coef(fit_grm, simplify = TRUE)
    if (is.list(coefs) && "items" %in% names(coefs)) {
       pars <- coefs$items
       a_cols <- grep("^a", colnames(pars))
       d_cols <- grep("^d", colnames(pars))
       if(length(d_cols) > 0) {
         a <- if(length(a_cols)>0) pars[, a_cols[1]] else rep(1, nrow(pars))
         d <- pars[, d_cols, drop=FALSE]
         b <- -d/a
         fs <- tryCatch(as.vector(mirt::fscores(fit_grm, method="EAP")), error=function(e) NULL)
         if(!is.null(fs)) wright_map <- ShinyItemAnalysis::ggWrightMap(fs, b)
       }
       
       # Coefs formatted
       grm_coefs <- as.data.frame(pars)
       grm_coefs <- cbind(Item = rownames(grm_coefs), grm_coefs)
    }
    
    # Fit stats
    item_fit <- tryCatch(mirt::itemfit(fit_grm), error = function(e) NULL)
    grm_indices <- tryCatch(mirt::M2(fit_grm), error = function(e) NULL)
  }
  
  # 5. Summary Stats
  desc_stats <- tryCatch(psych::describe(scored_data), error = function(e) NULL)
  
  # 6. Conclusion Table
  item_conclusion <- NULL
  if (!is.null(item_stat) && !is.null(grm_coefs)) {
     if(!"Item" %in% names(item_stat)) item_stat$Item <- rownames(item_stat)
     
     merged <- merge(item_stat, grm_coefs, by = "Item", all.x = TRUE)
     
     merged$Conclusion <- apply(merged, 1, function(row) {
      rit <- as.numeric(row["RIT"])
      a_param <- as.numeric(row["a1"]) 
      if(is.na(rit)) return("Check")
      if(rit < 0.2) return("删除 (Delete)")
      else if (rit < 0.3 || (!is.na(a_param) && a_param < 0.8)) return("修改 (Modify)")
      else return("保留 (Keep)")
    })
    
    cols <- c("Item", "Conclusion", "RIT", "Diff", "ULI", "a1")
    d_cols <- grep("^d", names(merged), value=TRUE)
    cols <- intersect(c(cols, d_cols), names(merged))
    item_conclusion <- merged[, cols, drop=FALSE]
  }

  # --- Render Report ---
  message("Generating report...")
  
  params <- list(
        validity_scatter = validity_scatter,
        corr_heatmap = corr_heatmap,
        scree_plot_res = scree_plot_res,
        fa_res = fa_res,
        alpha_res = alpha_res,
        dd_plot = dd_plot,
        item_stat = item_stat,
        wright_map = wright_map,
        mirt_object = mirt_object, 
        item_fit = item_fit,
        desc_stats = desc_stats,
        grm_indices = grm_indices,
        grm_coefs = grm_coefs,
        item_conclusion = item_conclusion
  )
  
  # Locate report.Rmd
  template_path <- "report.Rmd"
  
  # Search order: 
  # 1. Provided template_path arg (if I added it, but I won't change signature to break it)
  # 2. Current WD
  # 3. Known app directory
  if (!file.exists(template_path)) {
     candidates <- c(
        file.path(getwd(), "report.Rmd"),
        "e:/aicoding2026/ShinyItemAnalysis/PretestAnalysisTool/report.Rmd"
     )
     for(cand in candidates) {
        if(file.exists(cand)) {
           template_path <- cand
           break
        }
     }
  }
  
  if (!file.exists(template_path)) {
    stop("report.Rmd not found. Please ensure 'report.Rmd' is in the working directory or at 'e:/aicoding2026/ShinyItemAnalysis/PretestAnalysisTool/'.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  out_file_html <- file.path(output_dir, paste0(report_name, ".html"))
  out_file_md <- file.path(output_dir, paste0(report_name, ".md"))

  # Render HTML
  rmarkdown::render(template_path, output_format = "html_document", 
                    output_file = out_file_html, params = params, quiet = TRUE,
                    envir = new.env(parent = globalenv())) 
                    
  # Render Markdown
  rmarkdown::render(template_path, output_format = "md_document", 
                    output_file = out_file_md, params = params, quiet = TRUE,
                    envir = new.env(parent = globalenv()))

  message(paste("Reports generated:\n", out_file_html, "\n", out_file_md))
  return(c(out_file_html, out_file_md))
}

# Example Usage (commented out):
# run_pretest_analysis("data.csv", output_dir = "results")
