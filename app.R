library(shiny)
library(bslib)
library(readr)
library(readxl)
library(jsonlite)
library(DT)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

safe_dt_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df) == 0) return(df)

  for (nm in names(df)) {
    x <- df[[nm]]

    if (is.factor(x)) {
      df[[nm]] <- as.character(x)
    } else if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
      df[[nm]] <- as.character(x)
    } else if (is.matrix(x) || is.data.frame(x)) {
      df[[nm]] <- apply(as.data.frame(x), 1, function(row) paste(row, collapse = ", "))
    } else if (is.list(x)) {
      df[[nm]] <- vapply(
        x,
        function(val) {
          if (length(val) == 0 || is.null(val)) return("")
          paste(as.character(unlist(val)), collapse = ", ")
        },
        character(1)
      )
    }
  }

  names(df) <- as.character(names(df))
  df
}

make_dt <- function(df, page_len = 8) {
  DT::datatable(
    safe_dt_df(df),
    filter = "top",
    rownames = FALSE,
    options = list(
      pageLength = page_len,
      lengthMenu = list(c(8, 10, 25, 50, 100, -1), c("8", "10", "25", "50", "100", "All")),
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = "lftip"
    ),
    class = "compact stripe hover"
  )
}

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
        width: 100%;
      }
      .feature-plot-wrap {
        height: 420px;
        overflow: hidden;
        margin-bottom: 1rem;
      }
      .feature-plot-wrap .shiny-plot-output {
        height: 100% !important;
      }
      .info-box {
        background: #f8fafc;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        padding: 0.9rem 1rem;
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
        "This app allows users to upload datasets, clean and preprocess data, create new features, rename or remove columns, and perform exploratory data analysis through interactive visualizations.",
        class = "app-note"
      ),
      br(),
      card(
        full_screen = FALSE,
        card_header("How to Use This App"),
        card_body(
          tags$ol(
            tags$li("Go to the Upload Data tab and load a dataset."),
            tags$li("Use the Preprocessing tab to clean and transform the data."),
            tags$li("Use the Feature Engineering tab to create new variables and manage columns."),
            tags$li("Use the EDA tab to visualize and summarize the data."),
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
        actionButton("load_data", "Load Data", class = "btn-primary")
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
        checkboxInput("scale_numeric", "Scale numeric columns (z-score)", FALSE)
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
        h2("Create new feature"),
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
        h4("Remove existing column"),
        uiOutput("drop_col_ui"),
        actionButton("drop_column_btn", "Remove Column", class = "btn-outline-danger")
      ),
      h3("Feature Engineering Summary"),
      verbatimTextOutput("feature_summary"),
      br(),
      uiOutput("feature_plot_ui"),
      br(),
      h3("Engineered Data Preview"),
      div(class = "info-box", verbatimTextOutput("engineered_debug")),
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
        uiOutput("eda_x_ui"),
        uiOutput("eda_y_ui"),
        uiOutput("eda_group_ui")
      ),
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
  rename_state <- reactiveVal(list())
  dropped_cols <- reactiveVal(character(0))

  load_df_from_input <- function() {
    df <- NULL

    if (!is.null(input$file_upload)) {
      file_path <- input$file_upload$datapath
      file_name <- input$file_upload$name
      ext <- tolower(tools::file_ext(file_name))

      if (ext == "csv") {
        df <- read_csv(file_path, show_col_types = FALSE)
      } else if (ext == "xlsx") {
        df <- read_excel(file_path)
      } else if (ext == "json") {
        json_data <- fromJSON(file_path)
        df <- as.data.frame(json_data)
      } else if (ext == "rds") {
        df <- readRDS(file_path)
        df <- as.data.frame(df)
      }
    } else if (input$builtin_data != "None") {
      if (input$builtin_data == "iris") {
        df <- iris
      } else if (input$builtin_data == "mtcars") {
        df <- mtcars
      }
    }

    if (!is.null(df)) {
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    }

    df
  }

  observeEvent(input$load_data, {
    df <- load_df_from_input()

    if (!is.null(df)) {
      raw_data(df)
      feature_state(NULL)
      rename_state(list())
      dropped_cols(character(0))

      updateCheckboxInput(session, "remove_duplicates", value = FALSE)
      updateCheckboxInput(session, "clean_names", value = FALSE)
      updateCheckboxInput(session, "scale_numeric", value = FALSE)
      updateSelectInput(session, "missing_method", selected = "None")

      updateTextInput(session, "new_feature_name", value = "")
      updateSelectInput(session, "feature_method", selected = "None")
      updateTextInput(session, "rename_new_col", value = "")

      updateSelectInput(session, "plot_type", selected = "Histogram")
    }
  }, ignoreInit = TRUE)

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

    if (input$scale_numeric) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      if (length(numeric_cols) > 0) {
        scaled_part <- scale(df[numeric_cols])
        df[numeric_cols] <- as.data.frame(scaled_part)
      }
    }

    as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  })

  final_data <- reactive({
    req(processed_data())

    df <- processed_data()
    fs <- feature_state()

    if (!is.null(fs) && fs$method != "None" && nzchar(fs$new_name) &&
        fs$col1 %in% names(df) && fs$col2 %in% names(df) &&
        fs$new_name != fs$col1 && fs$new_name != fs$col2) {

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
    }

    rn <- rename_state()
    if (length(rn) > 0) {
      current_names <- names(df)
      for (old_nm in names(rn)) {
        new_nm <- rn[[old_nm]]
        if (old_nm %in% current_names && nzchar(new_nm)) {
          names(df)[names(df) == old_nm] <- new_nm
          current_names <- names(df)
        }
      }
    }

    drops <- dropped_cols()
    if (length(drops) > 0) {
      df <- df[, setdiff(names(df), drops), drop = FALSE]
    }

    as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  })

  observe({
    req(processed_data())
    df <- final_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    categorical_cols <- names(df)[!sapply(df, is.numeric)]

    current_feature_col1 <- if (!is.null(input$feature_col1) && input$feature_col1 %in% names(processed_data())[sapply(processed_data(), is.numeric)]) input$feature_col1 else ""
    current_feature_col2 <- if (!is.null(input$feature_col2) && input$feature_col2 %in% names(processed_data())[sapply(processed_data(), is.numeric)]) input$feature_col2 else ""

    current_eda_x <- if (!is.null(input$eda_x) && input$eda_x %in% numeric_cols) input$eda_x else if (length(numeric_cols) > 0) numeric_cols[1] else NULL
    current_eda_y <- if (!is.null(input$eda_y) && input$eda_y %in% numeric_cols) input$eda_y else if (length(numeric_cols) >= 2) numeric_cols[2] else NULL
    current_eda_group <- if (!is.null(input$eda_group) && input$eda_group %in% c("No grouping", categorical_cols)) input$eda_group else "No grouping"

    updateSelectInput(session, "feature_col1", choices = c("", names(processed_data())[sapply(processed_data(), is.numeric)]), selected = current_feature_col1)
    updateSelectInput(session, "feature_col2", choices = c("", names(processed_data())[sapply(processed_data(), is.numeric)]), selected = current_feature_col2)

    if (!is.null(current_eda_x)) {
      updateSelectInput(session, "eda_x", choices = numeric_cols, selected = current_eda_x)
    }
    if (!is.null(current_eda_y) && length(numeric_cols) >= 2) {
      updateSelectInput(session, "eda_y", choices = numeric_cols, selected = current_eda_y)
    }
    if (length(categorical_cols) > 0) {
      updateSelectInput(session, "eda_group", choices = c("No grouping", categorical_cols), selected = current_eda_group)
    }
  })

  output$feature_col1_ui <- renderUI({
    req(processed_data())
    numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    selected_val <- if (!is.null(input$feature_col1) && input$feature_col1 %in% numeric_cols) input$feature_col1 else ""

    selectInput(
      "feature_col1",
      "First numeric column",
      choices = c("", numeric_cols),
      selected = selected_val
    )
  })

  output$feature_col2_ui <- renderUI({
    req(processed_data())
    numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    selected_val <- if (!is.null(input$feature_col2) && input$feature_col2 %in% numeric_cols) input$feature_col2 else ""

    selectInput(
      "feature_col2",
      "Second numeric column",
      choices = c("", numeric_cols),
      selected = selected_val
    )
  })

  output$rename_old_col_ui <- renderUI({
    req(final_data())
    selectInput("rename_old_col", "Column to rename", choices = c("", names(final_data())), selected = "")
  })

  output$drop_col_ui <- renderUI({
    req(final_data())
    selectInput("drop_col", "Column to remove", choices = c("", names(final_data())), selected = "")
  })

  observeEvent(input$create_feature, {
    req(processed_data())
    numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]

    valid_request <- (
      input$feature_method != "None" &&
      nzchar(trimws(input$new_feature_name)) &&
      !is.null(input$feature_col1) &&
      !is.null(input$feature_col2) &&
      input$feature_col1 %in% numeric_cols &&
      input$feature_col2 %in% numeric_cols &&
      !(trimws(input$new_feature_name) %in% names(final_data()))
    )

    if (valid_request) {
      feature_state(list(
        new_name = trimws(input$new_feature_name),
        method = input$feature_method,
        col1 = input$feature_col1,
        col2 = input$feature_col2
      ))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$clear_feature, {
    feature_state(NULL)
    updateTextInput(session, "new_feature_name", value = "")
    updateSelectInput(session, "feature_method", selected = "None")
  }, ignoreInit = TRUE)

  observeEvent(input$rename_column_btn, {
    req(final_data())
    old_nm <- input$rename_old_col
    new_nm <- trimws(input$rename_new_col)

    if (!nzchar(old_nm) || !nzchar(new_nm)) return()
    if (!(old_nm %in% names(final_data()))) return()
    if (new_nm %in% names(final_data())) return()

    rn <- rename_state()
    rn[[old_nm]] <- new_nm
    rename_state(rn)
    updateTextInput(session, "rename_new_col", value = "")
  }, ignoreInit = TRUE)

  observeEvent(input$drop_column_btn, {
    req(final_data())
    col_to_drop <- input$drop_col
    if (!nzchar(col_to_drop)) return()
    if (!(col_to_drop %in% names(final_data()))) return()

    dropped_cols(unique(c(dropped_cols(), col_to_drop)))
  }, ignoreInit = TRUE)

  output$feature_plot_ui <- renderUI({
    req(final_data())
    fs <- feature_state()
    if (is.null(fs) || !(fs$new_name %in% names(final_data()))) return(NULL)

    if (!is.numeric(final_data()[[fs$new_name]])) return(NULL)

    tagList(
      h3(paste("Distribution of", fs$new_name)),
      div(class = "feature-plot-wrap", plotOutput("feature_plot", height = "100%"))
    )
  })

  output$feature_plot <- renderPlot({
    req(final_data())
    fs <- feature_state()
    req(!is.null(fs))
    req(fs$new_name %in% names(final_data()))
    req(is.numeric(final_data()[[fs$new_name]]))

    ggplot(final_data(), aes(x = .data[[fs$new_name]])) +
      geom_histogram(fill = "#7c4dff", color = "white", bins = 30, alpha = 0.9) +
      labs(
        title = paste("Distribution of", fs$new_name),
        x = fs$new_name,
        y = "Frequency"
      ) +
      theme_minimal(base_size = 15)
  })

  output$eda_x_ui <- renderUI({
    req(final_data())
    df <- final_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]

    if (input$plot_type %in% c("Histogram", "Boxplot", "Scatterplot") && length(numeric_cols) > 0) {
      selected_val <- if (!is.null(input$eda_x) && input$eda_x %in% numeric_cols) input$eda_x else numeric_cols[1]
      selectInput("eda_x", "X variable", choices = numeric_cols, selected = selected_val)
    }
  })

  output$eda_y_ui <- renderUI({
    req(final_data())
    df <- final_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]

    if (input$plot_type == "Scatterplot" && length(numeric_cols) >= 2) {
      selected_val <- if (!is.null(input$eda_y) && input$eda_y %in% numeric_cols) input$eda_y else numeric_cols[2]
      selectInput("eda_y", "Y variable", choices = numeric_cols, selected = selected_val)
    }
  })

  output$eda_group_ui <- renderUI({
    req(final_data())
    df <- final_data()
    categorical_cols <- names(df)[!sapply(df, is.numeric)]

    if (input$plot_type %in% c("Boxplot", "Scatterplot") && length(categorical_cols) > 0) {
      selected_val <- if (!is.null(input$eda_group) && input$eda_group %in% c("No grouping", categorical_cols)) input$eda_group else "No grouping"
      selectInput(
        "eda_group",
        "Group / color variable (optional)",
        choices = c("No grouping", categorical_cols),
        selected = selected_val
      )
    }
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
    make_dt(raw_data())
  })

  output$preprocess_summary <- renderText({
    req(raw_data(), processed_data())

    raw_df <- raw_data()
    proc_df <- processed_data()

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
      "- Scale numeric columns: ", ifelse(input$scale_numeric, "Yes", "No")
    )
  })

  output$processed_preview <- renderDT({
    req(processed_data())
    make_dt(processed_data())
  })

  output$feature_summary <- renderText({
    req(final_data())

    lines <- c()
    fs <- feature_state()

    if (is.null(fs)) {
      lines <- c(lines, "No engineered feature has been created yet.")
    } else {
      if (fs$new_name %in% names(final_data())) {
        lines <- c(
          lines,
          paste0("Created feature: ", fs$new_name),
          paste0("Method: ", fs$method),
          paste0("Using columns: ", fs$col1, " and ", fs$col2)
        )
      }
    }

    rn <- rename_state()
    if (length(rn) > 0) {
      lines <- c(lines, "", "Renamed columns:")
      for (old_nm in names(rn)) {
        lines <- c(lines, paste0("- ", old_nm, " -> ", rn[[old_nm]]))
      }
    }

    drops <- dropped_cols()
    if (length(drops) > 0) {
      lines <- c(lines, "", "Removed columns:", paste0("- ", drops))
    }

    lines <- c(lines, "", paste0("Total columns after feature engineering: ", ncol(final_data())))
    paste(lines, collapse = "\n")
  })

  output$engineered_debug <- renderText({
    req(final_data())
    df <- final_data()
    paste0(
      "Rows: ", nrow(df), " | Columns: ", ncol(df), "\n",
      "Names: ", paste(names(df), collapse = ", ")
    )
  })

  output$engineered_preview <- DT::renderDT({
    req(final_data())
    make_dt(final_data())
  }, server = FALSE)

  output$eda_plot <- renderPlot({
    req(final_data())
    df <- final_data()

    if (input$plot_type == "Histogram") {
      req(input$eda_x)

      ggplot(df, aes(x = .data[[input$eda_x]])) +
        geom_histogram(fill = "#60a5fa", color = "white", bins = 30) +
        labs(
          title = paste("Histogram of", input$eda_x),
          x = input$eda_x,
          y = "Count"
        ) +
        theme_minimal(base_size = 14)
    } else if (input$plot_type == "Boxplot") {
      req(input$eda_x)

      if (!is.null(input$eda_group) && input$eda_group != "No grouping") {
        ggplot(df, aes(x = .data[[input$eda_group]], y = .data[[input$eda_x]], fill = .data[[input$eda_group]])) +
          geom_boxplot(alpha = 0.8) +
          labs(
            title = paste("Boxplot of", input$eda_x, "by", input$eda_group),
            x = input$eda_group,
            y = input$eda_x
          ) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none")
      } else {
        ggplot(df, aes(y = .data[[input$eda_x]])) +
          geom_boxplot(fill = "#86efac", alpha = 0.9) +
          labs(
            title = paste("Boxplot of", input$eda_x),
            x = "",
            y = input$eda_x
          ) +
          theme_minimal(base_size = 14)
      }
    } else if (input$plot_type == "Scatterplot") {
      req(input$eda_x, input$eda_y)

      if (!is.null(input$eda_group) && input$eda_group != "No grouping") {
        ggplot(df, aes(x = .data[[input$eda_x]], y = .data[[input$eda_y]], color = .data[[input$eda_group]])) +
          geom_point(size = 2.8, alpha = 0.85) +
          labs(
            title = paste("Scatterplot of", input$eda_y, "vs", input$eda_x),
            x = input$eda_x,
            y = input$eda_y,
            color = input$eda_group
          ) +
          theme_minimal(base_size = 14)
      } else {
        ggplot(df, aes(x = .data[[input$eda_x]], y = .data[[input$eda_y]])) +
          geom_point(size = 2.8, alpha = 0.85, color = "#2563eb") +
          labs(
            title = paste("Scatterplot of", input$eda_y, "vs", input$eda_x),
            x = input$eda_x,
            y = input$eda_y
          ) +
          theme_minimal(base_size = 14)
      }
    } else if (input$plot_type == "Correlation Heatmap") {
      numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
      req(ncol(numeric_df) >= 2)

      cor_mat <- cor(numeric_df, use = "complete.obs")
      cor_df <- as.data.frame(as.table(cor_mat))

      ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#1d4ed8", mid = "white", high = "#dc2626", midpoint = 0) +
        labs(
          title = "Correlation Heatmap",
          x = "",
          y = "",
          fill = "Correlation"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()
        )
    }
  })

  output$eda_summary <- renderText({
    req(final_data())
    df <- final_data()

    if (input$plot_type == "Histogram") {
      req(input$eda_x)
      paste0(
        "Plot type: Histogram\n",
        "Variable: ", input$eda_x, "\n",
        "Mean: ", round(mean(df[[input$eda_x]], na.rm = TRUE), 3), "\n",
        "Median: ", round(median(df[[input$eda_x]], na.rm = TRUE), 3), "\n",
        "SD: ", round(sd(df[[input$eda_x]], na.rm = TRUE), 3)
      )
    } else if (input$plot_type == "Boxplot") {
      req(input$eda_x)
      paste0(
        "Plot type: Boxplot\n",
        "Variable: ", input$eda_x, "\n",
        "Min: ", round(min(df[[input$eda_x]], na.rm = TRUE), 3), "\n",
        "Q1: ", round(quantile(df[[input$eda_x]], 0.25, na.rm = TRUE), 3), "\n",
        "Median: ", round(median(df[[input$eda_x]], na.rm = TRUE), 3), "\n",
        "Q3: ", round(quantile(df[[input$eda_x]], 0.75, na.rm = TRUE), 3), "\n",
        "Max: ", round(max(df[[input$eda_x]], na.rm = TRUE), 3)
      )
    } else if (input$plot_type == "Scatterplot") {
      req(input$eda_x, input$eda_y)
      paste0(
        "Plot type: Scatterplot\n",
        "X variable: ", input$eda_x, "\n",
        "Y variable: ", input$eda_y, "\n",
        "Correlation: ", round(cor(df[[input$eda_x]], df[[input$eda_y]], use = "complete.obs"), 3)
      )
    } else {
      numeric_df <- df[, sapply(df, is.numeric), drop = FALSE]
      req(ncol(numeric_df) >= 2)
      paste0(
        "Plot type: Correlation Heatmap\n",
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
      req(final_data())
      write.csv(final_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
