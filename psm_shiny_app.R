# =============================================================================
#  PSM Impact Assessment – Shiny App
#  Assad Bori, OER MEAL Team
#  FAO Emergency and Resilience Division – Data & Evidence Team
#  Method: Propensity Score Matching (Nearest Neighbour)
# =============================================================================

# ---- Dependencies -----------------------------------------------------------
required_packages <- c("shiny", "shinydashboard", "MatchIt", "dplyr",
                       "ggplot2", "haven", "gridExtra", "DT", "broom",
                       "shinyWidgets", "shinycssloaders")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(shiny)
library(shinydashboard)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(haven)
library(gridExtra)
library(DT)
library(broom)
library(shinyWidgets)
library(shinycssloaders)

# =============================================================================
#  UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",

  # ---- Header ---------------------------------------------------------------
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://www.fao.org/images/corporatelibraries/fao-logo/fao-logo-en.svg",
               height = "38px", style = "margin-right:8px;"),
      "DEAL IMPACT ESTIMATOR"
    ),
    titleWidth = 500
  ),

  # ---- Sidebar --------------------------------------------------------------
  dashboardSidebar(
    width = 500,
    sidebarMenu(
      id = "tabs",
      menuItem("1 · Load Data",      tabName = "tab_data",    icon = icon("upload")),
      menuItem("2 · Set Variables",  tabName = "tab_vars",    icon = icon("sliders-h")),
      menuItem("3 · Run Matching",   tabName = "tab_run",     icon = icon("play-circle")),
      menuItem("4 · Balance Plots",  tabName = "tab_balance", icon = icon("chart-bar")),
      menuItem("5 · Treatment Effect", tabName = "tab_te",   icon = icon("bullseye"))
    ),

    # Navigation buttons
    tags$div(
      style = "padding: 15px;",
      actionButton("btn_next", "Next →", class = "btn btn-primary btn-block"),
      br(),
      actionButton("btn_prev", "← Back", class = "btn btn-default btn-block")
    )
  ),

  # ---- Body -----------------------------------------------------------------
  dashboardBody(

    # Custom CSS
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .box { border-top: 3px solid #007bff; }
      .step-title { font-size: 1.3em; font-weight: bold; color: #2c3e50;
                    margin-bottom: 6px; }
      .step-subtitle { color: #7f8c8d; margin-bottom: 20px; font-size: 0.95em; }
      .result-badge { background:#1a73e8; color:#fff; border-radius:4px;
                      padding:4px 10px; font-size:0.9em; }
      .section-label { font-weight:600; color:#34495e; margin-top:10px; }
    "))),

    tabItems(

      # ================================================================
      # TAB 1 – Load Data
      # ================================================================
      tabItem(tabName = "tab_data",
        fluidRow(
          box(width = 8, title = "Upload your dataset", status = "primary", solidHeader = TRUE,
            p(class = "step-subtitle",
              "Supported formats: .csv, .rds, .dta (Stata). The file will be kept in memory for the session."),
            fileInput("file_upload", label = NULL,
                      accept = c(".csv", ".rds", ".dta"),
                      buttonLabel = "Browse…", placeholder = "No file selected"),
            conditionalPanel("input.file_upload != null",
              hr(),
              fluidRow(
                valueBoxOutput("vbox_rows",  width = 4),
                valueBoxOutput("vbox_cols",  width = 4),
                valueBoxOutput("vbox_miss",  width = 4)
              ),
              h4("Preview (first 10 rows)"),
              withSpinner(DTOutput("preview_table"), type = 4)
            )
          ),
          box(width = 4, title = "Tips", status = "info", solidHeader = TRUE,
            tags$ul(
              tags$li("Ensure your treatment variable is ", tags$b("binary (0/1).")),
              tags$li("Missing values are automatically removed before matching."),
              tags$li("Factor/character columns are supported as covariates."),
              tags$li("Large files (>10 MB) are not supported — pre-filter when possible.")
            )
          )
        )
      ),

      # ================================================================
      # TAB 2 – Set Variables
      # ================================================================
      tabItem(tabName = "tab_vars",
        fluidRow(
          box(width = 6, title = "Variable Selection", status = "primary", solidHeader = TRUE,
            p(class = "step-subtitle", "Select the outcome, treatment and covariate columns from your dataset."),

            p(class = "section-label", "Outcome variable"),
            selectInput("var_outcome", label = NULL, choices = NULL),

            p(class = "section-label", "Treatment variable (binary 0/1)"),
            selectInput("var_treatment", label = NULL, choices = NULL),

            p(class = "section-label", "Covariates"),
            pickerInput("var_covariates", label = NULL, choices = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `selected-text-format` = "count > 3",
                                       `count-selected-text` = "{0} covariates selected"))
          ),
          box(width = 6, title = "Variable Summary", status = "info", solidHeader = TRUE,
            p(class = "step-subtitle", "Mean of outcome by treatment group (raw data)."),
            withSpinner(DTOutput("desc_table"), type = 4),
            hr(),
            p(class = "step-subtitle", "Welch t-test on raw outcome:"),
            verbatimTextOutput("raw_ttest")
          )
        )
      ),

      # ================================================================
      # TAB 3 – Run Matching
      # ================================================================
      tabItem(tabName = "tab_run",
        fluidRow(
          box(width = 5, title = "Matching Configuration", status = "primary", solidHeader = TRUE,
            p(class = "step-subtitle",
              "Configure the PSM options and click Run to estimate propensity scores and match units."),

            p(class = "section-label", "Matching method"),
            selectInput("psm_method", label = NULL,
                        choices = c("Nearest Neighbour" = "nearest",
                                    "Optimal"           = "optimal",
                                    "Full"              = "full"),
                        selected = "nearest"),

            p(class = "section-label", "Ratio (controls per treated unit)"),
            numericInput("psm_ratio", label = NULL, value = 1, min = 1, max = 5, step = 1),

            p(class = "section-label", "Distance metric"),
            selectInput("psm_distance", label = NULL,
                        choices = c("Logit (logistic regression)" = "glm",
                                    "Probit"                       = "glm",
                                    "GAM"                          = "gam",
                                    "Gradient Boosting"            = "gbm"),
                        selected = "glm"),

            switchInput("psm_replace", label = "Matching with replacement",
                        value = FALSE, onLabel = "Yes", offLabel = "No"),
            br(),
            actionButton("btn_run", "▶  Run Matching",
                         class = "btn btn-success btn-lg btn-block",
                         icon = icon("cogs"))
          ),
          box(width = 7, title = "Propensity Score Distribution", status = "info", solidHeader = TRUE,
            p(class = "step-subtitle",
              "Overlap of propensity scores between treated and control before matching."),
            withSpinner(plotOutput("ps_plot", height = "340px"), type = 4)
          )
        ),
        fluidRow(
          box(width = 12, title = "Matching Summary", status = "success", solidHeader = TRUE,
            collapsible = TRUE, collapsed = FALSE,
            withSpinner(verbatimTextOutput("match_summary"), type = 4)
          )
        )
      ),

      # ================================================================
      # TAB 4 – Balance Plots
      # ================================================================
      tabItem(tabName = "tab_balance",
        fluidRow(
          box(width = 12, title = "Covariate Balance After Matching", status = "primary", solidHeader = TRUE,
            p(class = "step-subtitle",
              "Each panel shows the covariate vs. propensity score by treatment group.
               Loess curves are added for continuous variables.
               Treated and control units should overlap well after matching."),
            fluidRow(
              column(4,
                p(class = "section-label", "Covariates to display"),
                pickerInput("bal_covs", label = NULL,
                            choices = NULL, multiple = TRUE,
                            options = list(`actions-box` = TRUE,
                                           `live-search` = TRUE,
                                           `selected-text-format` = "count > 3",
                                           `count-selected-text` = "{0} selected"))
              ),
              column(4,
                p(class = "section-label", "Plots per row"),
                sliderInput("bal_ncol", label = NULL, min = 1, max = 4, value = 2, step = 1)
              ),
              column(4,
                p(class = "section-label", "Plot height (px)"),
                sliderInput("bal_height", label = NULL, min = 300, max = 1600, value = 700, step = 50)
              )
            ),
            withSpinner(plotOutput("balance_plot",
                                   height = "auto",    # controlled by renderUI below
                                   inline  = FALSE), type = 4),
            uiOutput("balance_plot_ui")
          )
        ),
        fluidRow(
          box(width = 12, title = "Balance t-tests (matched sample)", status = "warning",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            p(class = "step-subtitle",
              "p-values should be large (> 0.05) after good matching, indicating no significant
               mean differences between groups on covariates."),
            withSpinner(DTOutput("balance_ttest"), type = 4)
          )
        )
      ),

      # ================================================================
      # TAB 5 – Treatment Effect
      # ================================================================
      tabItem(tabName = "tab_te",
        fluidRow(
          box(width = 6, title = "Means by Group (Matched Sample)", status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("te_means"), type = 4)
          ),
          box(width = 6, title = "T-test on Outcome (Matched Sample)", status = "primary", solidHeader = TRUE,
            withSpinner(verbatimTextOutput("te_ttest"), type = 4)
          )
        ),
        fluidRow(
          box(width = 6, title = "OLS Without Covariates", status = "success", solidHeader = TRUE,
            withSpinner(DTOutput("te_ols_simple"), type = 4)
          ),
          box(width = 6, title = "OLS With Covariates", status = "success", solidHeader = TRUE,
            withSpinner(DTOutput("te_ols_cov"), type = 4)
          )
        ),
        fluidRow(
          box(width = 12, title = "Outcome Distribution (Matched Sample)", status = "info",
              solidHeader = TRUE,
            withSpinner(plotOutput("te_dist_plot", height = "320px"), type = 4)
          )
        )
      )
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# =============================================================================
#  SERVER
# =============================================================================
server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Reactive: dataset
  # --------------------------------------------------------------------------
  raw_data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    df <- switch(ext,
      csv = read.csv(input$file_upload$datapath, stringsAsFactors = FALSE),
      rds = readRDS(input$file_upload$datapath),
      dta = haven::read_dta(input$file_upload$datapath),
      stop("Unsupported file type.")
    )
    # Convert haven-labelled to plain R types
    df <- as.data.frame(lapply(df, function(x) {
      if (inherits(x, "haven_labelled")) as.numeric(x) else x
    }))
    df
  })

  # --------------------------------------------------------------------------
  # TAB 1 – Data preview
  # --------------------------------------------------------------------------
  output$vbox_rows <- renderValueBox({
    valueBox(nrow(raw_data()), "Observations", icon = icon("table"), color = "blue")
  })
  output$vbox_cols <- renderValueBox({
    valueBox(ncol(raw_data()), "Variables", icon = icon("columns"), color = "teal")
  })
  output$vbox_miss <- renderValueBox({
    pct <- round(100 * mean(is.na(raw_data())), 1)
    valueBox(paste0(pct, "%"), "Missing values", icon = icon("exclamation-triangle"),
             color = if (pct > 10) "red" else "green")
  })
  output$preview_table <- renderDT({
    datatable(head(raw_data(), 10), options = list(scrollX = TRUE, dom = "t"),
              rownames = FALSE)
  })

  # --------------------------------------------------------------------------
  # TAB 2 – Variable dropdowns
  # --------------------------------------------------------------------------
  observe({
    req(raw_data())
    cols <- names(raw_data())
    updateSelectInput(session,  "var_outcome",    choices = cols)
    updateSelectInput(session,  "var_treatment",  choices = cols)
    updatePickerInput(session,  "var_covariates", choices = cols)
  })

  working_data <- reactive({
    req(raw_data(), input$var_outcome, input$var_treatment, input$var_covariates)
    raw_data() %>%
      select(all_of(c(input$var_outcome, input$var_treatment, input$var_covariates)))
  })

  output$desc_table <- renderDT({
    req(working_data())
    trt <- input$var_treatment
    out <- input$var_outcome
    df <- working_data()
    df[[trt]] <- factor(df[[trt]])
    tbl <- df %>%
      group_by(.data[[trt]]) %>%
      summarise(
        N    = n(),
        Mean = round(mean(.data[[out]], na.rm = TRUE), 4),
        SD   = round(sd(.data[[out]],   na.rm = TRUE), 4),
        SE   = round(SD / sqrt(N), 4)
      )
    datatable(tbl, options = list(dom = "t"), rownames = FALSE)
  })

  output$raw_ttest <- renderPrint({
    req(working_data())
    f <- as.formula(paste(input$var_outcome, "~", input$var_treatment))
    t.test(f, data = working_data())
  })

  # --------------------------------------------------------------------------
  # TAB 3 – Matching
  # --------------------------------------------------------------------------
  match_result <- eventReactive(input$btn_run, {
    req(working_data(), input$var_outcome, input$var_treatment, input$var_covariates)

    df <- working_data() %>% na.omit()
    cov_formula <- as.formula(
      paste(input$var_treatment, "~", paste(input$var_covariates, collapse = " + "))
    )

    mod <- matchit(cov_formula,
                   method   = input$psm_method,
                   distance = input$psm_distance,
                   ratio    = input$psm_ratio,
                   replace  = input$psm_replace,
                   data     = df)
    list(mod = mod, data = match.data(mod), raw = df)
  })

  # Propensity score distribution (pre-matching)
  output$ps_plot <- renderPlot({
    req(working_data())
    df <- working_data() %>% na.omit()
    cov_formula <- as.formula(
      paste(input$var_treatment, "~", paste(input$var_covariates, collapse = " + "))
    )
    glm_fit <- glm(cov_formula, family = binomial(), data = df)
    ps_df <- data.frame(
      ps    = predict(glm_fit, type = "response"),
      group = factor(df[[input$var_treatment]],
                     levels = c(0, 1),
                     labels = c("Control (0)", "Treated (1)"))
    )
    ggplot(ps_df, aes(x = ps, fill = group)) +
      geom_histogram(bins = 30, color = "white", alpha = 0.8) +
      facet_wrap(~ group, ncol = 2) +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      labs(x = "Propensity Score", y = "Count", fill = "Group",
           title = "Propensity Score Distribution (before matching)") +
      theme_bw(base_size = 13) +
      theme(legend.position = "none")
  })

  output$match_summary <- renderPrint({
    req(match_result())
    summary(match_result()$mod)
  })

  # --------------------------------------------------------------------------
  # TAB 4 – Balance plots
  # --------------------------------------------------------------------------
  observe({
    req(match_result())
    updatePickerInput(session, "bal_covs",
                      choices  = input$var_covariates,
                      selected = input$var_covariates)
  })

  fn_bal <- function(dta, variable, treatment_var) {
    dta$variable <- dta[[variable]]
    dta[[treatment_var]] <- as.factor(dta[[treatment_var]])

    p <- ggplot(dta, aes(x = distance, y = variable,
                         color = .data[[treatment_var]])) +
      geom_point(alpha = 0.25, size = 1.4) +
      scale_color_manual(values = c("0" = "#3498db", "1" = "#e74c3c"),
                         labels = c("Control", "Treated"),
                         name   = "Group") +
      labs(x = "Propensity Score", y = variable,
           title = variable) +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "bold", size = 10))

    if (is.numeric(dta$variable) || is.integer(dta$variable)) {
      support <- range(dta$variable, na.rm = TRUE)
      p <- p +
        geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
        coord_cartesian(ylim = support)
    } else {
      p <- p + geom_jitter(height = 0.15, alpha = 0.2, size = 1.3)
    }
    p
  }

  # Dynamic height for balance plot
  output$balance_plot_ui <- renderUI({
    req(input$bal_covs, input$bal_ncol, input$bal_height)
    n_plots <- length(input$bal_covs)
    n_rows  <- ceiling(n_plots / input$bal_ncol)
    h       <- max(input$bal_height, n_rows * 250)
    plotOutput("balance_plot", height = paste0(h, "px"))
  })

  output$balance_plot <- renderPlot({
    req(match_result(), input$bal_covs)
    dta_m   <- match_result()$data
    trt_var <- input$var_treatment
    covs    <- input$bal_covs
    ncol_   <- input$bal_ncol

    plots <- lapply(seq_along(covs), function(i) {
      p <- fn_bal(dta_m, covs[i], trt_var)
      if (i %% ncol_ != 1) p <- p + theme(legend.position = "none")
      p
    })

    n_rows <- ceiling(length(plots) / ncol_)
    do.call(gridExtra::grid.arrange, c(plots, nrow = n_rows, ncol = ncol_))
  })

  # Balance t-tests
  output$balance_ttest <- renderDT({
    req(match_result())
    dta_m   <- match_result()$data
    trt_var <- input$var_treatment
    covs    <- input$var_covariates

    results <- lapply(covs, function(v) {
      tt <- tryCatch(
        t.test(dta_m[[v]] ~ dta_m[[trt_var]]),
        error = function(e) NULL
      )
      if (is.null(tt)) return(NULL)
      data.frame(
        Covariate   = v,
        Mean_Control  = round(tt$estimate[1], 4),
        Mean_Treated  = round(tt$estimate[2], 4),
        Difference    = round(diff(tt$estimate), 4),
        t_statistic   = round(tt$statistic, 3),
        p_value       = round(tt$p.value, 4),
        Balanced      = ifelse(tt$p.value > 0.05, "✓ Yes", "✗ No"),
        stringsAsFactors = FALSE
      )
    })

    tbl <- do.call(rbind, Filter(Negate(is.null), results))

    datatable(tbl, rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 20)) %>%
      formatStyle("Balanced",
                  color = styleEqual(c("✓ Yes", "✗ No"),
                                     c("#27ae60",  "#e74c3c")),
                  fontWeight = "bold")
  })

  # --------------------------------------------------------------------------
  # TAB 5 – Treatment Effect
  # --------------------------------------------------------------------------
  output$te_means <- renderDT({
    req(match_result())
    dta_m   <- match_result()$data
    trt_var <- input$var_treatment
    out_var <- input$var_outcome

    tbl <- dta_m %>%
      group_by(.data[[trt_var]]) %>%
      summarise(N    = n(),
                Mean = round(mean(.data[[out_var]], na.rm = TRUE), 4),
                SD   = round(sd(.data[[out_var]],   na.rm = TRUE), 4))
    datatable(tbl, rownames = FALSE, options = list(dom = "t"))
  })

  output$te_ttest <- renderPrint({
    req(match_result())
    dta_m <- match_result()$data
    f     <- as.formula(paste(input$var_outcome, "~", input$var_treatment))
    t.test(f, data = dta_m)
  })

  output$te_ols_simple <- renderDT({
    req(match_result())
    dta_m <- match_result()$data
    f     <- as.formula(paste(input$var_outcome, "~", input$var_treatment))
    tidy(lm(f, data = dta_m)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$te_ols_cov <- renderDT({
    req(match_result())
    dta_m <- match_result()$data
    f <- as.formula(paste(input$var_outcome, "~", input$var_treatment, "+",
                          paste(input$var_covariates, collapse = " + ")))
    tidy(lm(f, data = dta_m)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE,
                                                  pageLength = 20))
  })

  output$te_dist_plot <- renderPlot({
    req(match_result())
    dta_m   <- match_result()$data
    trt_var <- input$var_treatment
    out_var <- input$var_outcome
    dta_m[[trt_var]] <- factor(dta_m[[trt_var]],
                                levels = c(0, 1),
                                labels = c("Control (0)", "Treated (1)"))

    ggplot(dta_m, aes(x = .data[[out_var]], fill = .data[[trt_var]])) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      labs(x = out_var, y = "Density", fill = "Group",
           title = paste("Distribution of", out_var, "by treatment group (matched sample)")) +
      theme_bw(base_size = 13)
  })

  # --------------------------------------------------------------------------
  # Navigation buttons
  # --------------------------------------------------------------------------
  tab_order <- c("tab_data", "tab_vars", "tab_run", "tab_balance", "tab_te")

  observeEvent(input$btn_next, {
    cur <- input$tabs
    idx <- match(cur, tab_order)
    if (!is.na(idx) && idx < length(tab_order))
      updateTabItems(session, "tabs", tab_order[idx + 1])
  })

  observeEvent(input$btn_prev, {
    cur <- input$tabs
    idx <- match(cur, tab_order)
    if (!is.na(idx) && idx > 1)
      updateTabItems(session, "tabs", tab_order[idx - 1])
  })

} # end server

# =============================================================================
#  Launch
# =============================================================================
shinyApp(ui = ui, server = server)
