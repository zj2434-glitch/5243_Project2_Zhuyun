library(shiny)
library(bslib)
library(readr)
library(readxl)
library(jsonlite)
library(DT)
library(janitor)
library(dplyr)
library(tidyr)

ui <- page_navbar(
  title = "Interactive Data Cleaning, Feature Engineering, and EDA Studio",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .metric-card {
        min-height: 130px;
        border-radius: 16px;
        box-shadow: 0 6px 18px rgba(0,0,0,0.08);
      }
      .metric-title {
        font-size: 1rem;
        color: #4b5563;
        margin-bottom: 0.5rem;
      }
      .metric-value {
        font-size: 2rem;
        font-weight: 700;
        color: #111827;
      }
      .section-title {
        margin-top: 0.5rem;
        margin-bottom: 1rem;
      }
      .sidebar-section h4 {
        margin-bottom: 1rem;
      }
      .app-note {
        color: #4b5563;
        font-size: 0.98rem;
      }
      .team-box {
        background: #f8fafc;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 1rem 1.25rem;
      }
      .control-buttons .btn {
        margin-right: 0.5rem;
        margin-top: 0.5rem;
      }
      .feature-plot-wrap {
        height: 390px;
        overflow: hidden;
        margin-bottom: 20px;
      }
      .explain-box {
        background: #f8fafc;
        border: 1px solid #e5e7eb;
        border-radius: 14px;
        padding: 0.9rem 1.1rem;
        margin-bottom: 1rem;
        color: #374151;
      }
      .dataTables_wrapper {
        width: 100%;
      }
    ")),
    tags$script(HTML("
      let savedScrollY = 0;

      document.addEventListener('change', function(e) {
        savedScrollY = window.scrollY || document.documentElement.scrollTop;
      });

      document.addEventListener('click', function(e) {
        if (e.target && (
          e.target.id === 'create_feature' ||
          e.target.id === 'clear_feature' ||
          e.target.id === 'rename_column_btn' ||
          e.target.id === 'drop_column_btn' ||
          e.target.id === 'load_data'
        )) {
          savedScrollY = window.scrollY || document.documentElement.scrollTop;
        }
      });

      $(document).on('shiny:idle', function() {
        window.scrollTo(0, savedScrollY);
      });
    "))
  ),
  
  nav_panel(
    "Home",
    page_fluid(
      br(),
      h2("Welcome", class = "section-title"),
      p(
        "This app allows users to upload datasets, clean and preprocess data, create new features, rename or drop columns, and perform exploratory data analysis through interactive visualizations.",
        class = "app-note"
      ),
      br(),
      card(
        card_header("How to Use This App"),
        card_body(
          tags$ol(
            tags$li("Go to the Upload Data tab and load a dataset."),
            tags$li("Use the Preprocessing tab to clean, encode, scale, and transform the data."),
            tags$li("Use the Feature Engineering tab to create a new feature and manage existing columns."),
            tags$li("Use the EDA tab to filter, visualize, and summarize the data."),
            tags$li("Use the Download tab to export the final dataset.")
          )
        )
      ),
      br(),
      card(
        card_header("Supported Formats"),
        card_body(
          tags$ul(
            tags$li("CSV"),
            tags$li("Excel (.xlsx)"),
            tags$li("JSON"),
            tags$li("RDS")
          )
        )
      ),
      br(),
      card(
        card_header("Group Information"),
        card_body(
          div(
            class = "team-box",
            p(tags$b("Zhuyun Jin"), " (zj2434)"),
            p(tags$b("Megan Wang"), " (placeholder)")
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Upload Data",
    page_sidebar(
      sidebar = sidebar(
        class = "sidebar-section",
        h4("Upload or choose data"),
        fileInput(
          "file_upload",
          "Upload a file",
          accept = c(".csv", ".xlsx", ".json", ".rds")
        ),
        selectInput(
          "builtin_data",
          "Or choose a built-in dataset",
          choices = c("None", "iris", "mtcars")
        ),
        actionButton("load_data", "Load Data", class = "btn-primary"),
        br(),
        br(),
        helpText("Tip: use the sample files in your data folder to demonstrate each format.")
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          class = "metric-card",
          card_body(
            div(class = "metric-title", "Rows"),
            div(class = "metric-value", textOutput("rows_value"))
          )
        ),
        card(
          class = "metric-card",
          card_body(
            div(class = "metric-title", "Columns"),
            div(class = "metric-value", textOutput("cols_value"))
          )
        ),
        card(
          class = "metric-card",
          card_body(
            div(class = "metric-title", "Missing Values"),
            div(class = "metric-value", textOutput("missing_value"))
          )
        )
      ),
      
      br(),
      h3("Current Dataset"),
      verbatimTextOutput("dataset_label"),
      br(),
      h3("Dataset Information"),
      verbatimTextOutput("data_info"),
      br(),
      h3("Column Type Summary"),
      tableOutput("column_type_summary"),
      br(),
      h3("Dataset Preview"),
      DTOutput("data_preview")
    )
  ),
  
  nav_panel(
    "Preprocessing",
    page_sidebar(
      sidebar = sidebar(
        class = "sidebar-section",
        h4("Cleaning options"),
        checkboxInput("remove_duplicates", "Remove duplicate rows", FALSE),
        checkboxInput("clean_names", "Standardize column names", FALSE),
        helpText("Standardizing names converts columns to lowercase_with_underscores."),
        selectInput(
          "missing_method",
          "Missing value handling",
          choices = c(
            "None",
            "Drop rows with missing values",
            "Fill numeric with mean",
            "Fill numeric with median"
          ),
          selected = "None"
        ),
        helpText("Mean/median filling applies only to numeric columns."),
        checkboxInput("scale_numeric", "Scale numeric columns (z-score)", FALSE),
        helpText("Scaling applies only to numeric columns after missing value handling."),
        
        hr(),
        h4("Categorical Encoding"),
        selectInput(
          "encoding_method",
          "Encoding method",
          choices = c(
            "None",
            "Convert character columns to factors",
            "One-hot encode selected columns"
          ),
          selected = "None"
        ),
        uiOutput("encoding_cols_ui"),
        helpText("One-hot encoding expands selected categorical columns into binary indicator columns."),
        
        hr(),
        h4("Type Conversion"),
        selectInput(
          "conversion_method",
          "Convert column types",
          choices = c(
            "None",
            "Convert selected numeric columns to factors",
            "Convert selected character/factor columns to numeric codes"
          ),
          selected = "None"
        ),
        uiOutput("conversion_cols_ui"),
        helpText("Use this when you want to switch numeric columns into categories or turn categorical columns into numeric codes for modeling or plotting."),
        
        hr(),
        h4("Outlier Handling"),
        selectInput(
          "outlier_method",
          "Outlier method",
          choices = c(
            "None",
            "Remove outliers using IQR rule"
          ),
          selected = "None"
        ),
        uiOutput("outlier_cols_ui"),
        helpText("IQR rule removes rows outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR] for selected numeric columns.")
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          class = "metric-card",
          card_body(
            div(class = "metric-title", "Rows After Processing"),
            div(class = "metric-value", textOutput("proc_rows_value"))
          )
        ),
        card(
          class = "metric-card",
          card_body(
            div(class = "metric-title", "Columns After Processing"),
            div(class = "metric-value", textOutput("proc_cols_value"))
          )
        ),
        card(
          class = "metric-card",
          card_body(
            div(class = "metric-title", "Missing After Processing"),
            div(class = "metric-value", textOutput("proc_missing_value"))
          )
        )
      ),
      
      div(
        class = "explain-box",
        tags$b("What this section does:"),
        " Clean the dataset, handle missing values and duplicates, convert column types, encode categorical variables, scale numeric columns, and optionally remove outliers with instant preview below."
      ),
      br(),
      h3("Preprocessing Summary"),
      verbatimTextOutput("preprocess_summary"),
      br(),
      h3("Processed Data Preview"),
      DTOutput("processed_preview")
    )
  ),
  
  nav_panel(
    "Feature Engineering",
    page_sidebar(
      sidebar = sidebar(
        class = "sidebar-section",
        
        h4("Create new feature"),
        textInput("new_feature_name", "New feature name", ""),
        helpText("Choose a unique column name."),
        selectInput(
          "feature_method",
          "Method",
          choices = c(
            "None",
            "Sum of two columns",
            "Difference of two columns",
            "Product of two columns",
            "Ratio of two columns"
          ),
          selected = "None"
        ),
        uiOutput("feature_col1_ui"),
        uiOutput("feature_col2_ui"),
        helpText("Choose two numeric columns, then click Create Feature."),
        div(
          class = "control-buttons",
          actionButton("create_feature", "Create Feature", class = "btn-primary"),
          actionButton("clear_feature", "Clear Feature", class = "btn-secondary")
        ),
        
        hr(),
        h4("Rename existing column"),
        uiOutput("rename_old_col_ui"),
        textInput("rename_new_col", "New column name", ""),
        actionButton("rename_column_btn", "Rename Column", class = "btn-outline-primary"),
        
        hr(),
        h4("Drop existing column"),
        uiOutput("drop_col_ui"),
        actionButton("drop_column_btn", "Drop Column", class = "btn-outline-danger")
      ),
      
      div(
        class = "explain-box",
        tags$b("What this section does:"),
        " Create a new feature from two numeric columns, then rename or remove columns and immediately inspect how the final dataset changes in the preview table below."
      ),
      h3("Feature Engineering Summary"),
      verbatimTextOutput("feature_summary"),
      br(),
      h3("Column Management Summary"),
      verbatimTextOutput("column_ops_summary"),
      br(),
      h3("Created Feature Feedback"),
      verbatimTextOutput("feature_feedback"),
      div(
        class = "feature-plot-wrap",
        plotOutput("feature_plot", height = "360px")
      ),
      br(),
      h3("Engineered Data Preview"),
      verbatimTextOutput("engineered_debug"),
      DTOutput("engineered_preview")
    )
  ),
  
  nav_panel(
    "EDA",
    page_sidebar(
      sidebar = sidebar(
        class = "sidebar-section",
        h4("Plot controls"),
        selectInput(
          "plot_type",
          "Choose plot type",
          choices = c("Histogram", "Boxplot", "Scatterplot", "Correlation Heatmap"),
          selected = "Histogram"
        ),
        sliderInput("hist_bins", "Histogram bins", min = 5, max = 60, value = 30),
        uiOutput("eda_x_ui"),
        uiOutput("eda_y_ui"),
        uiOutput("eda_group_ui"),
        
        hr(),
        h4("Filter data"),
        uiOutput("filter_var_ui"),
        uiOutput("filter_control_ui"),
        helpText("Plots and EDA summaries below are based on the filtered dataset.")
      ),
      h3("EDA Overview"),
      verbatimTextOutput("eda_filter_summary"),
      br(),
      h3("Exploratory Data Analysis"),
      plotOutput("eda_plot", height = "560px"),
      br(),
      h3("EDA Summary"),
      verbatimTextOutput("eda_summary")
    )
  ),
  
  nav_panel(
    "Download",
    page_fluid(
      br(),
      h3("Download Final Dataset"),
      p("Download the latest version of the dataset after preprocessing and feature engineering."),
      downloadButton("download_data", "Download CSV", class = "btn-success")
    )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactiveVal(NULL)
  feature_state <- reactiveVal(NULL)
  dataset_source <- reactiveVal("No dataset loaded")
  column_ops <- reactiveVal(list())
  
  current_feature_name <- reactive({
    fs <- feature_state()
    ops <- column_ops()
    
    if (is.null(fs)) return(NULL)
    
    nm <- fs$new_name
    
    if (length(ops) == 0) return(nm)
    
    for (op in ops) {
      if (op$type == "rename" && identical(op$old, nm)) {
        nm <- op$new
      }
      if (op$type == "drop" && identical(op$col, nm)) {
        return(NULL)
      }
    }
    
    nm
  })
  
  observeEvent(input$load_data, {
    
    df <- NULL
    source_name <- "No dataset loaded"
    
    if (!is.null(input$file_upload)) {
      file_path <- input$file_upload$datapath
      file_name <- input$file_upload$name
      ext <- tolower(tools::file_ext(file_name))
      
      if (ext == "csv") {
        df <- tryCatch(read_csv(file_path, show_col_types = FALSE), error = function(e) NULL)
      } else if (ext == "xlsx") {
        df <- tryCatch(read_excel(file_path), error = function(e) NULL)
      } else if (ext == "json") {
        df <- tryCatch({
          json_data <- fromJSON(file_path)
          as.data.frame(json_data)
        }, error = function(e) NULL)
      } else if (ext == "rds") {
        df <- tryCatch({
          obj <- readRDS(file_path)
          as.data.frame(obj)
        }, error = function(e) NULL)
      }
      
      source_name <- paste0("Uploaded file: ", file_name)
    } else if (input$builtin_data != "None") {
      if (input$builtin_data == "iris") {
        df <- iris
      } else if (input$builtin_data == "mtcars") {
        df <- mtcars
      }
      source_name <- paste0("Built-in dataset: ", input$builtin_data)
    }
    
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
      return()
    }
    
    raw_data(as.data.frame(df))
    dataset_source(source_name)
    feature_state(NULL)
    column_ops(list())
    
    updateCheckboxInput(session, "remove_duplicates", value = FALSE)
    updateCheckboxInput(session, "clean_names", value = FALSE)
    updateCheckboxInput(session, "scale_numeric", value = FALSE)
    
    updateSelectInput(session, "missing_method", selected = "None")
    updateSelectInput(session, "encoding_method", selected = "None")
    updateSelectInput(session, "conversion_method", selected = "None")
    updateSelectInput(session, "outlier_method", selected = "None")
    
    updateTextInput(session, "new_feature_name", value = "")
    updateSelectInput(session, "feature_method", selected = "None")
    updateTextInput(session, "rename_new_col", value = "")
    
    updateSelectInput(session, "plot_type", selected = "Histogram")
    updateSliderInput(session, "hist_bins", value = 30)
  })
  
  output$encoding_cols_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    if (input$encoding_method == "One-hot encode selected columns" && length(cat_cols) > 0) {
      selectInput(
        "encoding_cols",
        "Categorical columns to encode",
        choices = cat_cols,
        selected = cat_cols[1],
        multiple = TRUE
      )
    } else {
      NULL
    }
  })
  
  output$conversion_cols_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    
    if (input$conversion_method == "Convert selected numeric columns to factors") {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      if (length(numeric_cols) == 0) return(NULL)
      selectInput(
        "conversion_cols",
        "Columns to convert",
        choices = numeric_cols,
        selected = numeric_cols[1],
        multiple = TRUE
      )
    } else if (input$conversion_method == "Convert selected character/factor columns to numeric codes") {
      cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      if (length(cat_cols) == 0) return(NULL)
      selectInput(
        "conversion_cols",
        "Columns to convert",
        choices = cat_cols,
        selected = cat_cols[1],
        multiple = TRUE
      )
    } else {
      NULL
    }
  })
  
  output$outlier_cols_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (input$outlier_method == "Remove outliers using IQR rule" && length(numeric_cols) > 0) {
      selectInput(
        "outlier_cols",
        "Numeric columns for outlier removal",
        choices = numeric_cols,
        selected = numeric_cols,
        multiple = TRUE
      )
    } else {
      NULL
    }
  })
  
  processed_data <- reactive({
    req(raw_data())
    
    df <- raw_data()
    
    if (input$clean_names) {
      df <- janitor::clean_names(df)
    }
    
    if (input$remove_duplicates) {
      df <- distinct(df)
    }
    
    if (input$missing_method == "Drop rows with missing values") {
      df <- tidyr::drop_na(df)
    }
    
    if (input$missing_method == "Fill numeric with mean") {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      for (col in numeric_cols) {
        if (any(is.na(df[[col]]))) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        }
      }
    }
    
    if (input$missing_method == "Fill numeric with median") {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      for (col in numeric_cols) {
        if (any(is.na(df[[col]]))) {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        }
      }
    }
    
    if (input$conversion_method == "Convert selected numeric columns to factors") {
      selected_cols <- input$conversion_cols
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        valid_cols <- selected_cols[selected_cols %in% names(df)]
        for (col in valid_cols) {
          if (is.numeric(df[[col]])) {
            df[[col]] <- as.factor(df[[col]])
          }
        }
      }
    }
    
    if (input$conversion_method == "Convert selected character/factor columns to numeric codes") {
      selected_cols <- input$conversion_cols
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        valid_cols <- selected_cols[selected_cols %in% names(df)]
        for (col in valid_cols) {
          if (is.character(df[[col]]) || is.factor(df[[col]])) {
            df[[col]] <- as.numeric(as.factor(df[[col]]))
          }
        }
      }
    }
    
    if (input$encoding_method == "Convert character columns to factors") {
      char_cols <- names(df)[sapply(df, is.character)]
      for (col in char_cols) {
        df[[col]] <- as.factor(df[[col]])
      }
    }
    
    if (input$encoding_method == "One-hot encode selected columns") {
      selected_cols <- input$encoding_cols
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        valid_cols <- selected_cols[selected_cols %in% names(df)]
        if (length(valid_cols) > 0) {
          for (col in valid_cols) {
            df[[col]] <- as.factor(df[[col]])
          }
          mm <- model.matrix(~ . - 1, data = df[, valid_cols, drop = FALSE])
          mm_df <- as.data.frame(mm)
          keep_df <- df[, setdiff(names(df), valid_cols), drop = FALSE]
          df <- bind_cols(keep_df, mm_df)
        }
      }
    }
    
    if (input$outlier_method == "Remove outliers using IQR rule") {
      selected_outlier_cols <- input$outlier_cols
      if (!is.null(selected_outlier_cols) && length(selected_outlier_cols) > 0) {
        valid_cols <- selected_outlier_cols[selected_outlier_cols %in% names(df)]
        if (length(valid_cols) > 0) {
          keep_rows <- rep(TRUE, nrow(df))
          for (col in valid_cols) {
            if (is.numeric(df[[col]])) {
              q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
              q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
              iqr_val <- q3 - q1
              lower <- q1 - 1.5 * iqr_val
              upper <- q3 + 1.5 * iqr_val
              keep_rows <- keep_rows & (is.na(df[[col]]) | (df[[col]] >= lower & df[[col]] <= upper))
            }
          }
          df <- df[keep_rows, , drop = FALSE]
        }
      }
    }
    
    if (input$scale_numeric) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      if (length(numeric_cols) > 0) {
        df[numeric_cols] <- scale(df[numeric_cols])
        df[numeric_cols] <- as.data.frame(df[numeric_cols])
      }
    }
    
    as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  output$feature_col1_ui <- renderUI({
    req(processed_data())
    numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    if (length(numeric_cols) == 0) return(NULL)
    
    current_val <- if (!is.null(input$feature_col1) && input$feature_col1 %in% numeric_cols) input$feature_col1 else ""
    selectInput("feature_col1", "First numeric column", choices = c("", numeric_cols), selected = current_val)
  })
  
  output$feature_col2_ui <- renderUI({
    req(processed_data())
    numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    if (length(numeric_cols) == 0) return(NULL)
    
    current_val <- if (!is.null(input$feature_col2) && input$feature_col2 %in% numeric_cols) input$feature_col2 else ""
    selectInput("feature_col2", "Second numeric column", choices = c("", numeric_cols), selected = current_val)
  })
  
  observeEvent(input$create_feature, {
    req(processed_data())
    
    df <- processed_data()
    
    if (input$feature_method == "None") return()
    if (!nzchar(input$new_feature_name)) return()
    if (input$new_feature_name %in% names(df)) return()
    if (is.null(input$feature_col1) || is.null(input$feature_col2)) return()
    if (input$feature_col1 == "" || input$feature_col2 == "") return()
    
    feature_state(list(
      new_name = input$new_feature_name,
      method = input$feature_method,
      col1 = input$feature_col1,
      col2 = input$feature_col2
    ))
  })
  
  observeEvent(input$clear_feature, {
    feature_state(NULL)
    updateTextInput(session, "new_feature_name", value = "")
    updateSelectInput(session, "feature_method", selected = "None")
  })
  
  feature_applied_data <- reactive({
    req(processed_data())
    
    df <- processed_data()
    fs <- feature_state()
    
    if (is.null(fs)) return(df)
    if (!(fs$col1 %in% names(df) && fs$col2 %in% names(df))) return(df)
    
    if (fs$method == "Sum of two columns") {
      df[[fs$new_name]] <- df[[fs$col1]] + df[[fs$col2]]
    }
    
    if (fs$method == "Difference of two columns") {
      df[[fs$new_name]] <- df[[fs$col1]] - df[[fs$col2]]
    }
    
    if (fs$method == "Product of two columns") {
      df[[fs$new_name]] <- df[[fs$col1]] * df[[fs$col2]]
    }
    
    if (fs$method == "Ratio of two columns") {
      df[[fs$new_name]] <- ifelse(df[[fs$col2]] == 0, NA, df[[fs$col1]] / df[[fs$col2]])
    }
    
    as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  final_data <- reactive({
    req(feature_applied_data())
    
    df <- feature_applied_data()
    ops <- column_ops()
    
    if (length(ops) == 0) {
      return(as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE))
    }
    
    for (op in ops) {
      if (op$type == "rename") {
        if (op$old %in% names(df) && nzchar(op$new) && !(op$new %in% names(df))) {
          names(df)[names(df) == op$old] <- op$new
        }
      }
      
      if (op$type == "drop") {
        if (op$col %in% names(df)) {
          df <- df[, setdiff(names(df), op$col), drop = FALSE]
        }
      }
    }
    
    as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  safe_final_data <- reactive({
    req(final_data())
    
    df <- final_data()
    df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
    rownames(df) <- NULL
    names(df) <- make.unique(as.character(names(df)))
    
    for (nm in names(df)) {
      if (is.matrix(df[[nm]])) {
        df[[nm]] <- as.vector(df[[nm]])
      }
      
      if (is.list(df[[nm]]) && !is.data.frame(df[[nm]])) {
        df[[nm]] <- vapply(
          df[[nm]],
          function(x) {
            if (length(x) == 0) "" else paste(x, collapse = ", ")
          },
          character(1)
        )
      }
    }
    
    df
  })
  
  output$rename_old_col_ui <- renderUI({
    req(safe_final_data())
    cols <- names(safe_final_data())
    if (length(cols) == 0) return(NULL)
    
    current_val <- if (!is.null(input$rename_old_col) && input$rename_old_col %in% cols) input$rename_old_col else cols[1]
    selectInput("rename_old_col", "Column to rename", choices = cols, selected = current_val)
  })
  
  output$drop_col_ui <- renderUI({
    req(safe_final_data())
    cols <- names(safe_final_data())
    if (length(cols) == 0) return(NULL)
    
    current_val <- if (!is.null(input$drop_col) && input$drop_col %in% cols) input$drop_col else cols[1]
    selectInput("drop_col", "Column to drop", choices = cols, selected = current_val)
  })
  
  observeEvent(input$rename_column_btn, {
    req(safe_final_data())
    
    old_name <- input$rename_old_col
    new_name <- input$rename_new_col
    df <- safe_final_data()
    
    if (is.null(old_name) || !nzchar(old_name)) return()
    if (!nzchar(new_name)) return()
    if (!(old_name %in% names(df))) return()
    if (new_name %in% names(df)) return()
    
    ops <- column_ops()
    ops[[length(ops) + 1]] <- list(type = "rename", old = old_name, new = new_name)
    column_ops(ops)
    
    updateTextInput(session, "rename_new_col", value = "")
  })
  
  observeEvent(input$drop_column_btn, {
    req(safe_final_data())
    
    col_name <- input$drop_col
    df <- safe_final_data()
    
    if (is.null(col_name) || !nzchar(col_name)) return()
    if (!(col_name %in% names(df))) return()
    
    ops <- column_ops()
    ops[[length(ops) + 1]] <- list(type = "drop", col = col_name)
    column_ops(ops)
  })
  
  output$rows_value <- renderText({
    req(raw_data())
    nrow(raw_data())
  })
  
  output$cols_value <- renderText({
    req(raw_data())
    ncol(raw_data())
  })
  
  output$missing_value <- renderText({
    req(raw_data())
    sum(is.na(raw_data()))
  })
  
  output$proc_rows_value <- renderText({
    req(processed_data())
    nrow(processed_data())
  })
  
  output$proc_cols_value <- renderText({
    req(processed_data())
    ncol(processed_data())
  })
  
  output$proc_missing_value <- renderText({
    req(processed_data())
    sum(is.na(processed_data()))
  })
  
  output$dataset_label <- renderText({
    dataset_source()
  })
  
  output$data_info <- renderText({
    req(raw_data())
    df <- raw_data()
    
    paste0(
      "Rows: ", nrow(df), "\n",
      "Columns: ", ncol(df), "\n",
      "Missing values: ", sum(is.na(df)), "\n\n",
      "Column names:\n",
      paste(names(df), collapse = ", ")
    )
  })
  
  output$column_type_summary <- renderTable({
    req(raw_data())
    df <- raw_data()
    
    data.frame(
      Column = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      stringsAsFactors = FALSE
    )
  })
  
  output$data_preview <- renderDT({
    req(raw_data())
    datatable(
      raw_data(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 8,
        lengthMenu = list(c(8, 10, 25, 50, 100, -1), c("8", "10", "25", "50", "100", "All")),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  }, server = FALSE)
  
  output$preprocess_summary <- renderText({
    req(raw_data(), processed_data())
    
    raw_df <- raw_data()
    proc_df <- processed_data()
    
    encoded_cols_text <- if (!is.null(input$encoding_cols) && length(input$encoding_cols) > 0) {
      paste(input$encoding_cols, collapse = ", ")
    } else {
      "None selected"
    }
    
    conversion_cols_text <- if (!is.null(input$conversion_cols) && length(input$conversion_cols) > 0) {
      paste(input$conversion_cols, collapse = ", ")
    } else {
      "None selected"
    }
    
    outlier_cols_text <- if (!is.null(input$outlier_cols) && length(input$outlier_cols) > 0) {
      paste(input$outlier_cols, collapse = ", ")
    } else {
      "None selected"
    }
    
    paste0(
      "Before preprocessing:\n",
      "Rows: ", nrow(raw_df), "\n",
      "Columns: ", ncol(raw_df), "\n",
      "Missing values: ", sum(is.na(raw_df)), "\n\n",
      "After preprocessing:\n",
      "Rows: ", nrow(proc_df), "\n",
      "Columns: ", ncol(proc_df), "\n",
      "Missing values: ", sum(is.na(proc_df)), "\n\n",
      "Options applied:\n",
      "- Remove duplicates: ", ifelse(input$remove_duplicates, "Yes", "No"), "\n",
      "- Standardize column names: ", ifelse(input$clean_names, "Yes", "No"), "\n",
      "- Missing value handling: ", input$missing_method, "\n",
      "- Scale numeric columns: ", ifelse(input$scale_numeric, "Yes", "No"), "\n",
      "- Categorical encoding: ", input$encoding_method, "\n",
      "- Encoding columns: ", encoded_cols_text, "\n",
      "- Outlier handling: ", input$outlier_method, "\n",
      "- Outlier columns: ", outlier_cols_text
    )
  })
  
  output$processed_preview <- renderDT({
    req(processed_data())
    
    datatable(
      processed_data(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 8,
        lengthMenu = list(c(8, 10, 25, 50, 100, -1), c("8", "10", "25", "50", "100", "All")),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  }, server = FALSE)
  
  output$feature_summary <- renderText({
    req(processed_data())
    
    fs <- feature_state()
    current_name <- current_feature_name()
    
    if (is.null(fs)) {
      return("No engineered feature has been created yet.")
    }
    
    if (!(fs$col1 %in% names(processed_data()) && fs$col2 %in% names(processed_data()))) {
      return("The saved feature could not be applied under the current preprocessing settings.")
    }
    
    final_cols <- names(safe_final_data())
    exists_now <- !is.null(current_name) && current_name %in% final_cols
    
    paste0(
      "Created feature: ", fs$new_name, "\n",
      "Method: ", fs$method, "\n",
      "Using columns: ", fs$col1, " and ", fs$col2, "\n",
      "Current feature name in final dataset: ", ifelse(is.null(current_name), "Dropped", current_name), "\n",
      "Feature currently available: ", ifelse(exists_now, "Yes", "No"), "\n",
      "Total columns in final dataset: ", ncol(safe_final_data())
    )
  })
  
  output$column_ops_summary <- renderText({
    ops <- column_ops()
    
    if (length(ops) == 0) {
      return("No column rename or drop operations have been applied yet.")
    }
    
    lines <- c("Applied column operations:")
    for (i in seq_along(ops)) {
      op <- ops[[i]]
      if (op$type == "rename") {
        lines <- c(lines, paste0(i, ". Rename: ", op$old, " -> ", op$new))
      }
      if (op$type == "drop") {
        lines <- c(lines, paste0(i, ". Drop: ", op$col))
      }
    }
    
    paste(lines, collapse = "\n")
  })
  
  output$feature_feedback <- renderText({
    req(safe_final_data())
    current_name <- current_feature_name()
    
    if (is.null(current_name) || !(current_name %in% names(safe_final_data()))) {
      return("Create a feature to see its summary statistics.")
    }
    
    x <- safe_final_data()[[current_name]]
    
    if (!is.numeric(x)) {
      return("The created feature is not numeric.")
    }
    
    x <- as.numeric(x)
    x_non_missing <- x[!is.na(x)]
    
    if (length(x_non_missing) == 0) {
      return("The created feature has no non-missing values.")
    }
    
    paste0(
      "Feature: ", current_name, "\n",
      "Non-missing values: ", length(x_non_missing), "\n",
      "Mean: ", round(mean(x_non_missing), 3), "\n",
      "Median: ", round(median(x_non_missing), 3), "\n",
      "Min: ", round(min(x_non_missing), 3), "\n",
      "Max: ", round(max(x_non_missing), 3)
    )
  })
  
  output$feature_plot <- renderPlot({
    req(safe_final_data())
    
    current_name <- current_feature_name()
    if (is.null(current_name)) return(invisible(NULL))
    if (!(current_name %in% names(safe_final_data()))) return(invisible(NULL))
    
    x <- safe_final_data()[[current_name]]
    if (!is.numeric(x)) return(invisible(NULL))
    
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(invisible(NULL))
    
    tryCatch({
      hist(
        x,
        breaks = 30,
        main = paste("Distribution of", current_name),
        xlab = current_name,
        col = "#8b5cf6",
        border = "white"
      )
    }, error = function(e) {
      invisible(NULL)
    })
  }, res = 96)
  
  output$engineered_debug <- renderText({
    req(safe_final_data())
    paste0(
      "Rows: ", nrow(safe_final_data()),
      " | Columns: ", ncol(safe_final_data()),
      "\nNames: ", paste(names(safe_final_data()), collapse = ", ")
    )
  })
  
  output$engineered_preview <- DT::renderDataTable({
    req(safe_final_data())
    
    df <- safe_final_data()
    df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
    rownames(df) <- NULL
    
    for (nm in names(df)) {
      if (inherits(df[[nm]], c("Date", "POSIXct", "POSIXt"))) {
        df[[nm]] <- as.character(df[[nm]])
      } else if (is.factor(df[[nm]])) {
        df[[nm]] <- as.character(df[[nm]])
      } else if (is.logical(df[[nm]])) {
        df[[nm]] <- ifelse(is.na(df[[nm]]), NA_character_, ifelse(df[[nm]], "TRUE", "FALSE"))
      } else if (is.matrix(df[[nm]])) {
        df[[nm]] <- as.vector(df[[nm]])
      } else if (is.list(df[[nm]])) {
        df[[nm]] <- vapply(df[[nm]], function(x) paste(x, collapse = ", "), character(1))
      }
    }
    
    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      class = "cell-border stripe hover compact",
      options = list(
        pageLength = 8,
        lengthMenu = list(c(8, 10, 25, 50, 100, -1), c("8", "10", "25", "50", "100", "All")),
        scrollX = TRUE,
        autoWidth = TRUE,
        deferRender = TRUE
      )
    )
  }, server = TRUE)
  
  output$eda_x_ui <- renderUI({
    req(safe_final_data())
    df <- safe_final_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (input$plot_type %in% c("Histogram", "Boxplot", "Scatterplot") && length(numeric_cols) > 0) {
      current_val <- if (!is.null(input$eda_x) && input$eda_x %in% numeric_cols) input$eda_x else numeric_cols[1]
      selectInput("eda_x", "X variable", choices = numeric_cols, selected = current_val)
    } else {
      NULL
    }
  })
  
  output$eda_y_ui <- renderUI({
    req(safe_final_data())
    df <- safe_final_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (input$plot_type == "Scatterplot" && length(numeric_cols) >= 2) {
      current_val <- if (!is.null(input$eda_y) && input$eda_y %in% numeric_cols) input$eda_y else numeric_cols[2]
      selectInput("eda_y", "Y variable", choices = numeric_cols, selected = current_val)
    } else {
      NULL
    }
  })
  
  output$eda_group_ui <- renderUI({
    req(safe_final_data())
    df <- safe_final_data()
    categorical_cols <- names(df)[!sapply(df, is.numeric)]
    
    if (input$plot_type %in% c("Boxplot", "Scatterplot") && length(categorical_cols) > 0) {
      current_val <- if (!is.null(input$eda_group) && input$eda_group %in% c("No grouping", categorical_cols)) input$eda_group else "No grouping"
      selectInput(
        "eda_group",
        "Group / color variable (optional)",
        choices = c("No grouping", categorical_cols),
        selected = current_val
      )
    } else {
      NULL
    }
  })
  
  output$filter_var_ui <- renderUI({
    req(safe_final_data())
    df <- safe_final_data()
    if (ncol(df) == 0) return(NULL)
    
    current_val <- if (!is.null(input$filter_var) && input$filter_var %in% c("No filter", names(df))) input$filter_var else "No filter"
    
    selectInput(
      "filter_var",
      "Filter variable",
      choices = c("No filter", names(df)),
      selected = current_val
    )
  })
  
  output$filter_control_ui <- renderUI({
    req(safe_final_data())
    
    df <- safe_final_data()
    
    if (is.null(input$filter_var) || input$filter_var == "No filter" || !(input$filter_var %in% names(df))) {
      return(NULL)
    }
    
    col_data <- df[[input$filter_var]]
    
    if (is.numeric(col_data)) {
      rng <- range(col_data, na.rm = TRUE)
      if (!all(is.finite(rng))) return(NULL)
      
      current_val <- if (!is.null(input$filter_range)) input$filter_range else c(floor(rng[1]), ceiling(rng[2]))
      
      sliderInput(
        "filter_range",
        "Numeric range",
        min = floor(rng[1]),
        max = ceiling(rng[2]),
        value = current_val
      )
    } else {
      vals <- unique(as.character(col_data))
      vals <- vals[!is.na(vals)]
      current_val <- if (!is.null(input$filter_values)) input$filter_values else vals
      
      selectInput(
        "filter_values",
        "Values to keep",
        choices = vals,
        selected = current_val,
        multiple = TRUE
      )
    }
  })
  
  filtered_data <- reactive({
    req(safe_final_data())
    
    df <- safe_final_data()
    
    if (is.null(input$filter_var) || input$filter_var == "No filter" || !(input$filter_var %in% names(df))) {
      return(df)
    }
    
    col_data <- df[[input$filter_var]]
    
    if (is.numeric(col_data)) {
      if (is.null(input$filter_range)) return(df)
      keep <- !is.na(col_data) & col_data >= input$filter_range[1] & col_data <= input$filter_range[2]
      df <- df[keep, , drop = FALSE]
    } else {
      if (is.null(input$filter_values)) return(df)
      df <- df[as.character(col_data) %in% input$filter_values, , drop = FALSE]
    }
    
    df
  })
  
  output$eda_filter_summary <- renderText({
    req(safe_final_data(), filtered_data())
    
    paste0(
      "Rows before filter: ", nrow(safe_final_data()), "\n",
      "Rows after filter: ", nrow(filtered_data()), "\n",
      "Active filter: ", ifelse(is.null(input$filter_var) || input$filter_var == "No filter", "None", input$filter_var)
    )
  })
  
  output$eda_plot <- renderPlot({
    req(filtered_data())
    
    df <- filtered_data()
    if (nrow(df) == 0) return(invisible(NULL))
    
    tryCatch({
      
      if (input$plot_type == "Histogram") {
        if (is.null(input$eda_x) || !(input$eda_x %in% names(df))) return(invisible(NULL))
        x <- df[[input$eda_x]]
        if (!is.numeric(x)) return(invisible(NULL))
        
        hist(
          x,
          breaks = input$hist_bins,
          main = paste("Histogram of", input$eda_x),
          xlab = input$eda_x,
          col = "#60a5fa",
          border = "white"
        )
      }
      
      if (input$plot_type == "Boxplot") {
        if (is.null(input$eda_x) || !(input$eda_x %in% names(df))) return(invisible(NULL))
        x <- df[[input$eda_x]]
        if (!is.numeric(x)) return(invisible(NULL))
        
        if (!is.null(input$eda_group) && input$eda_group != "No grouping" && input$eda_group %in% names(df)) {
          boxplot(
            x ~ df[[input$eda_group]],
            main = paste("Boxplot of", input$eda_x, "by", input$eda_group),
            xlab = input$eda_group,
            ylab = input$eda_x,
            col = "lightgreen"
          )
        } else {
          boxplot(
            x,
            main = paste("Boxplot of", input$eda_x),
            ylab = input$eda_x,
            col = "lightgreen"
          )
        }
      }
      
      if (input$plot_type == "Scatterplot") {
        if (is.null(input$eda_x) || is.null(input$eda_y)) return(invisible(NULL))
        if (!(input$eda_x %in% names(df)) || !(input$eda_y %in% names(df))) return(invisible(NULL))
        
        x <- df[[input$eda_x]]
        y <- df[[input$eda_y]]
        if (!is.numeric(x) || !is.numeric(y)) return(invisible(NULL))
        
        if (!is.null(input$eda_group) && input$eda_group != "No grouping" && input$eda_group %in% names(df)) {
          groups <- as.factor(df[[input$eda_group]])
          cols <- as.integer(groups)
          
          plot(
            x, y,
            col = cols,
            pch = 19,
            xlab = input$eda_x,
            ylab = input$eda_y,
            main = paste("Scatterplot of", input$eda_y, "vs", input$eda_x)
          )
          legend("topright", legend = levels(groups), col = seq_along(levels(groups)), pch = 19, cex = 0.9)
        } else {
          plot(
            x, y,
            col = "#2563eb",
            pch = 19,
            xlab = input$eda_x,
            ylab = input$eda_y,
            main = paste("Scatterplot of", input$eda_y, "vs", input$eda_x)
          )
        }
      }
      
      if (input$plot_type == "Correlation Heatmap") {
        numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
        if (ncol(numeric_df) < 2) return(invisible(NULL))
        
        cor_mat <- cor(numeric_df, use = "complete.obs")
        image(
          1:ncol(cor_mat),
          1:nrow(cor_mat),
          t(cor_mat[nrow(cor_mat):1, ]),
          axes = FALSE,
          xlab = "",
          ylab = "",
          main = "Correlation Heatmap",
          col = colorRampPalette(c("navy", "white", "firebrick"))(100)
        )
        axis(1, at = 1:ncol(cor_mat), labels = colnames(cor_mat), las = 2)
        axis(2, at = 1:nrow(cor_mat), labels = rev(rownames(cor_mat)), las = 2)
      }
      
    }, error = function(e) {
      invisible(NULL)
    })
  }, res = 96)
  
  output$eda_summary <- renderText({
    req(filtered_data())
    
    df <- filtered_data()
    if (nrow(df) == 0) return("")
    
    if (input$plot_type == "Histogram") {
      if (is.null(input$eda_x) || !(input$eda_x %in% names(df))) return("")
      x <- df[[input$eda_x]]
      if (!is.numeric(x)) return("")
      
      paste0(
        "Plot type: Histogram\n",
        "Variable: ", input$eda_x, "\n",
        "Rows used: ", nrow(df), "\n",
        "Mean: ", round(mean(x, na.rm = TRUE), 3), "\n",
        "Median: ", round(median(x, na.rm = TRUE), 3), "\n",
        "SD: ", round(sd(x, na.rm = TRUE), 3)
      )
    } else if (input$plot_type == "Boxplot") {
      if (is.null(input$eda_x) || !(input$eda_x %in% names(df))) return("")
      x <- df[[input$eda_x]]
      if (!is.numeric(x)) return("")
      
      paste0(
        "Plot type: Boxplot\n",
        "Variable: ", input$eda_x, "\n",
        "Rows used: ", nrow(df), "\n",
        "Min: ", round(min(x, na.rm = TRUE), 3), "\n",
        "Q1: ", round(quantile(x, 0.25, na.rm = TRUE), 3), "\n",
        "Median: ", round(median(x, na.rm = TRUE), 3), "\n",
        "Q3: ", round(quantile(x, 0.75, na.rm = TRUE), 3), "\n",
        "Max: ", round(max(x, na.rm = TRUE), 3)
      )
    } else if (input$plot_type == "Scatterplot") {
      if (is.null(input$eda_x) || is.null(input$eda_y)) return("")
      if (!(input$eda_x %in% names(df)) || !(input$eda_y %in% names(df))) return("")
      
      x <- df[[input$eda_x]]
      y <- df[[input$eda_y]]
      if (!is.numeric(x) || !is.numeric(y)) return("")
      
      paste0(
        "Plot type: Scatterplot\n",
        "X variable: ", input$eda_x, "\n",
        "Y variable: ", input$eda_y, "\n",
        "Rows used: ", nrow(df), "\n",
        "Correlation: ", round(cor(x, y, use = "complete.obs"), 3)
      )
    } else {
      numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
      if (ncol(numeric_df) < 2) return("")
      
      paste0(
        "Plot type: Correlation Heatmap\n",
        "Rows used: ", nrow(df), "\n",
        "Numeric variables included: ", paste(colnames(numeric_df), collapse = ", "), "\n",
        "Total numeric variables: ", ncol(numeric_df)
      )
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "final_dataset.csv"
    },
    content = function(file) {
      req(safe_final_data())
      write.csv(safe_final_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)