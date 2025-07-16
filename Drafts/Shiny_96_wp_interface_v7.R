library(shiny)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)

# Define categories and colors with better color palette
categories <- c("Blank", "Material Control (NP Only)", "Broth Only", 
                "Experiment", "Positive Control (Bacteria Only)")

category_colors <- c(
  "Experiment" = "#8a4db0",
  "Broth Only" = "#DAA520",
  "Positive Control (Bacteria Only)" = "#c46464EE",
  "Material Control (NP Only)" = "#4373a3EE",
  "Blank" = "#BEBEBE"
)


# Categories that should receive concentration assignments
concentration_categories <- c("Material Control (NP Only)", "Experiment")

# Generate well positions
rows <- LETTERS[1:8]
cols <- 1:12
plate_data <- expand.grid(Row = rows, Col = cols)
plate_data$Well <- paste0(plate_data$Row, plate_data$Col)

ui <- fluidPage(
  # Remove default Bootstrap container constraints
  tags$style(HTML("
    .container-fluid {
      max-width: none !important;
      width: 100% !important;
      padding-left: 0 !important;
      padding-right: 0 !important;
    }
  ")),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"),
    tags$style(HTML("
      * {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      }
      
      body {
        background: linear-gradient(135deg, #C0C0C0 0%, #764ba2 100%);
        min-height: 100vh;
        margin: 0;
      }
      
      .main-container {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(10px);
        border-radius: 20px;
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
        margin: 20px;
        overflow: hidden;
      }
      
      .navbar {
        background: linear-gradient(135deg, #8a8787 0%, #764ba2 100%);
        padding: 6px 10px;
        color: white;
        margin: 0;
        border-radius: 0;
      }
      
      .navbar h1 {
        margin: 0;
        font-size: 1.5em;
        font-weight: 600;
        text-shadow: 0 1px 2px rgba(0, 0, 0, 0.2);
        line-height: 1.2;
      }
      
      .navbar .subtitle {
        margin: 2px 0 0 0;
        font-size: 0.9em;
        font-weight: 400;
        opacity: 0.9;
        line-height: 1.1;
      }
      
      .content-wrapper {
        display: flex;
        min-height: calc(100vh - 80px);
      }
      
      .sidebar {
        width: 350px;
        background: #f8fafc;
        padding: 10px;
        border-right: 1px solid #e2e8f0;
        overflow-y: auto;
        flex-shrink: 0;
      }
      
      .main-content {
        flex: 1;
        padding: 30px;
        background: white;
        min-width: 0;
      }
      
      .card {
        background: white;
        border-radius: 6px;
        padding: 6px;
        margin-bottom: 6px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
        border: 1px solid #e2e8f0;
      }
      
      .card h5 {
        color: #2d3748;
        font-weight: 600;
        margin: 0 0 6px 0;
        font-size: 0.85em;
        display: flex;
        align-items: center;
        gap: 2px;
        text-transform: uppercase; 
        letter-spacing: 0.5px;
      }
      
      .card-icon {
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 8px;
        font-weight: bold;
      }
      
      .form-group {
        margin-bottom: 6px;
      }
      
      .form-control, .selectize-input {
        border: 2px solid #e2e8f0;
        border-radius: 6px;
        padding: 6px;
        font-size: 12px;
        transition: all 0.3s ease;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #667eea;
        box-shadow: 0 0 0 2px rgba(102, 126, 234, 0.1);
        outline: none;
      }
      
      .btn-group {
        display: flex;
        gap: 10px;
        margin-bottom: 20px;
      }
      
      .btn {
        border: none;
        padding: 12px 15px;
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.3s ease;
        cursor: pointer;
        text-decoration: none;
        display: inline-flex;
        align-items: center;
        gap: 8px;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.3);
      }
      
      .btn-secondary {
        background: #f1f5f9;
        color: #475569;
      }
      
      .btn-secondary:hover {
        background: #e2e8f0;
        transform: translateY(-1px);
      }
      
      .btn-danger {
        background: #c46464;
        color: white;
      }
      
      .btn-danger:hover {
        background: #dc2626;
        transform: translateY(-1px);
      }
      
      .btn-success {
        background: #04c4c7;
        color: white;
      }
      
      .btn-success:hover {
        background: #059496;
        transform: translateY(-1px);
      }
      
     .selection-mode-container {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 6px;
      }

    .selection-mode-label {
      font-size: 12px;
      font-weight: 500;
      color: #374151;
      margin-right: 4px;
    }

      .radio-buttons {
        display: flex;
        gap: 4px;
      }
      
      .radio-buttons input[type='radio'] {
        display: none;
      }
      
      .radio-buttons label {
        background: #f1f5f9;
        padding: 4px 8px;
        border-radius: 4px;
        cursor: pointer;
        transition: all 0.3s ease;
        border: 1px solid #e2e8f0;
        font-size: 11px;
        font-weight: 500;
        color: #64748b;
        min-width: 40px;
        text-align: center;
      }
      
      .radio-buttons label:hover {
        background: #e2e8f0;
      }
      
      .radio-buttons input[type='radio']:checked + label {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-color: #667eea;
        box-shadow: 0 2px 4px rgba(102, 126, 234, 0.2);
      }

      
      .summary-table {
        background: white;
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        border: 1px solid #e2e8f0;
        margin-top: 20px;
      }
      
      .summary-table table {
        width: 100%;
        border-collapse: collapse;
      }
      
      .summary-table th {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 15px;
        text-align: left;
        font-weight: 600;
      }
      
      .summary-table td {
        padding: 12px 15px;
        border-bottom: 1px solid #e2e8f0;
      }
      
      .summary-table tr:hover {
        background: #f8fafc;
      }
      
      .context-menu {
        position: absolute;
        z-index: 10000;
        background: white;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        box-shadow: 0 10px 40px rgba(0, 0, 0, 0.15);
        display: none;
        min-width: 200px;
        overflow: hidden;
      }
      
      .context-item {
        padding: 12px 16px;
        cursor: pointer;
        transition: all 0.2s ease;
        border-bottom: 1px solid #f1f5f9;
      }
      
      .context-item:hover {
        background: #f8fafc;
        color: #667eea;
      }
      
      .context-item:last-child {
        border-bottom: none;
      }
      
      .plate-container {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        border: 1px solid #e2e8f0;
        width: 100%;
        min-width: 900px;
        overflow-x: auto;
      }
      
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 20px;
        margin-bottom: 20px;
      }
      
      .stat-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 12px;
        text-align: center;
      }
      
      .stat-number {
        font-size: 2em;
        font-weight: 700;
        margin-bottom: 5px;
      }
      
      .stat-label {
        font-size: 0.9em;
        opacity: 0.9;
      }
      
      .legend {
        display: flex;
        flex-wrap: wrap;
        gap: 15px;
        margin-bottom: 20px;
        padding: 20px;
        background: #f8fafc;
        border-radius: 12px;
        border: 1px solid #e2e8f0;
      }
      
      .legend-item {
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      .legend-color {
        width: 20px;
        height: 20px;
        border-radius: 50%;
        border: 2px solid rgba(0, 0, 0, 0.1);
      }
      
      .legend-label {
        font-size: 0.9em;
        color: #475569;
      }
      
      @media (max-width: 1200px) {
        .sidebar {
          width: 250px;
        }
      }
      
      @media (max-width: 768px) {
        .content-wrapper {
          flex-direction: column;
        }
        
        .sidebar {
          width: 100%;
        }
        
        .main-container {
          margin: 10px;
        }
        
        .plate-container {
          min-width: 600px;
        }
      }
    "))
  ),
  
  div(class = "main-container",
      div(class = "navbar",
          h1("🧪 96-Well Plate Designer"),
          p(class = "subtitle", "Plate layout editor for experimental design")
      ),
      
      div(class = "content-wrapper",
          div(class = "sidebar",
              div(class = "card",
                  h5(span(class = "card-icon", "⚙"), "Plate Configuration"),
                  selectInput("plate_format", "Plate Format:", 
                              choices = c("96-well (8×12)" = "96", 
                                          "48-well (6×8)" = "48", 
                                          "24-well (4×6)" = "24"), 
                              selected = "96")
              ),
              
              div(class = "card",
                  h5(span(class = "card-icon", "🎯"), "Well Assignment"),
                  selectInput("category", "Category:", choices = categories, 
                              selected = "Experiment"),
                  div(class = "selection-mode-container",
                      span(class = "selection-mode-label", "Selection Mode:"),
                      div(class = "radio-buttons",
                          tags$input(type = "radio", name = "mode", id = "mode-click", value = "Click"),
                          tags$label(`for` = "mode-click", "Click"),
                          tags$input(type = "radio", name = "mode", id = "mode-drag", value = "Drag", checked = "checked"),
                          tags$label(`for` = "mode-drag", "Drag")
                      )
                  )
              ),
              
              div(class = "card",
                  h5(span(class = "card-icon", "🔬"), "Experimental Design"),
                  numericInput("num_replicates", "Number of Replicates:", 
                               value = 5, min = 1, max = 7),
                  textAreaInput("concentration_values", "Serial Dilution Concentrations:",
                                value = paste(round(250 / 2^(0:9), 1), collapse = ", "),
                                placeholder = "e.g., 250, 125, 62.5, 31.25, ...",
                                rows = 3),
                  textInput("concentration_units", "Units:", value = "µg/mL"),
                  textAreaInput("replicate_labels", "Replicate Labels:",
                                value = "Rep 1, Rep 2, Rep 3, Rep 4, Rep 5",
                                placeholder = "e.g., Rep 1, Rep 2, Rep 3, ...",
                                rows = 2)
              ),
              
              div(class = "card",
                  h5(span(class = "card-icon", "📤"), "Export Options"),
                  textInput("filename", "Filename (without .csv):", 
                            value = paste0("plate_layout_96well_", Sys.Date()),
                            placeholder = "Enter filename without extension"),
                  checkboxInput("include_metadata", "Include Metadata", value = TRUE),
                  checkboxInput("include_empty_wells", "Include Empty Wells", value = TRUE)
              ),
              
              div(class = "btn-group",
                  actionButton("undo", "↶ Undo", class = "btn btn-secondary"),
                  actionButton("reset", "🔄 Reset", class = "btn btn-danger")
              ),
              
              downloadButton("downloadCSV", "📥 Download CSV", class = "btn btn-success")
          ),
          
          div(class = "main-content",
              div(class = "legend",
                  lapply(names(category_colors), function(cat) {
                    div(class = "legend-item",
                        span(class = "legend-color", style = paste0("background-color: ", category_colors[cat])),
                        span(class = "legend-label", cat)
                    )
                  })
              ),
              
              div(class = "plate-container",
                  plotlyOutput("platePlot", height = "600px", width = "100%")
              ),
              
              div(class = "summary-table",
                  h5("📊 Assignment Summary"),
                  tableOutput("assignmentSummary")
              )
          )
      )
  ),
  
  # Enhanced context menu
  tags$div(
    id = "contextMenu",
    class = "context-menu",
    lapply(categories, function(cat) {
      tags$div(class = "context-item", cat)
    })
  ),
  
  tags$script(HTML("
    let selectedWell = null;
    
    // Context menu functionality
    document.addEventListener('contextmenu', function(e) {
      if (e.target && e.target.textContent.match(/^[A-H][0-9]{1,2}$/)) {
        e.preventDefault();
        selectedWell = e.target.textContent;
        const menu = document.getElementById('contextMenu');
        menu.style.left = e.pageX + 'px';
        menu.style.top = e.pageY + 'px';
        menu.style.display = 'block';
      } else {
        document.getElementById('contextMenu').style.display = 'none';
      }
    });
    
    document.addEventListener('click', function(e) {
      const menu = document.getElementById('contextMenu');
      if (e.target.classList.contains('context-item')) {
        Shiny.setInputValue('right_click_assign', {
          well: selectedWell,
          category: e.target.textContent,
          nonce: Math.random()
        });
      }
      menu.style.display = 'none';
    });
    
    // Handle custom radio buttons
    document.addEventListener('DOMContentLoaded', function() {
      const radioInputs = document.querySelectorAll('input[name=\"mode\"]');
      
      radioInputs.forEach(input => {
        input.addEventListener('change', function() {
          if (this.checked) {
            Shiny.setInputValue('mode', this.value);
          }
        });
      });
      
      // Set initial value
      const checkedInput = document.querySelector('input[name=\"mode\"]:checked');
      if (checkedInput) {
        Shiny.setInputValue('mode', checkedInput.value);
      }
    });
  "))
)

server <- function(input, output, session) {
  # Reactive values
  well_assignments <- reactiveVal(data.frame(Well = plate_data$Well, Category = "Blank"))
  history <- reactiveVal(list())
  
  # Add this reactive value for mode
  mode_value <- reactiveVal("Drag")  # Default value
  
  # Add this observer to handle mode changes
  observeEvent(input$mode, {
    if (!is.null(input$mode) && input$mode != "") {
      mode_value(input$mode)
    }
  })
  
  # Dynamic plate dimensions based on format
  plate_dims <- reactive({
    switch(input$plate_format,
           "96" = list(rows = 8, cols = 12, row_letters = LETTERS[1:8]),
           "48" = list(rows = 6, cols = 8, row_letters = LETTERS[1:6]),
           "24" = list(rows = 4, cols = 6, row_letters = LETTERS[1:4]))
  })
  
  # Generate plate data based on format
  current_plate_data <- reactive({
    dims <- plate_dims()
    rows <- dims$row_letters
    cols <- 1:dims$cols
    pd <- expand.grid(Row = rows, Col = cols)
    pd$Well <- paste0(pd$Row, pd$Col)
    pd
  })
  
  # Parse replicate labels
  replicate_labels <- reactive({
    labels <- unlist(strsplit(input$replicate_labels, ","))
    trimws(labels)
  })
  
  # Parse concentration values
  concentration_values <- reactive({
    vals <- unlist(strsplit(input$concentration_values, ","))
    vals <- suppressWarnings(as.numeric(trimws(vals)))
    vals[!is.na(vals)]
  })
  
  # Create dilution labels with units
  dilution_labels <- reactive({
    vals <- concentration_values()
    paste0(round(vals, 2), " ", input$concentration_units)
  })
  
  # Reset well assignments when plate format changes
  observeEvent(input$plate_format, {
    pd <- current_plate_data()
    well_assignments(data.frame(Well = pd$Well, Category = "Blank"))
    history(list())
  })
  
  # Render plate data with assignments
  render_plate <- reactive({
    pd <- current_plate_data()
    current <- well_assignments()
    
    # Match assignments to current plate wells
    pd$Category <- current$Category[match(pd$Well, current$Well)]
    pd$Category[is.na(pd$Category)] <- "Blank"
    pd$Color <- category_colors[pd$Category]
    pd$Color[is.na(pd$Color)] <- "lightgray"
    pd
  })
  
  # Create enhanced CSV export with metadata
  enhanced_export_data <- reactive({
    current <- well_assignments()
    pd <- current_plate_data()
    dims <- plate_dims()
    
    # Filter based on export options
    if (!input$include_empty_wells) {
      current <- current[current$Category != "Blank", ]
    }
    
    # Create base export data
    export_data <- current
    
    # Add row and column information
    export_data$Row <- substr(export_data$Well, 1, 1)
    export_data$Column <- as.numeric(substr(export_data$Well, 2, nchar(export_data$Well)))
    
    # Add replicate information
    export_data$Replicate_Number <- NA
    export_data$Replicate_Label <- NA
    
    # Add concentration information
    export_data$Concentration_Value <- NA
    export_data$Concentration_Units <- NA
    
    # Process each well
    for (i in seq_len(nrow(export_data))) {
      well <- export_data$Well[i]
      category <- export_data$Category[i]
      row_letter <- export_data$Row[i]
      col_num <- export_data$Column[i]
      
      # Assign replicate information ONLY to "Experiment" category
      if (category == "Experiment") {
        row_index <- which(dims$row_letters == row_letter)
        if (row_index <= length(replicate_labels()) && row_index <= input$num_replicates) {
          export_data$Replicate_Number[i] <- row_index
          export_data$Replicate_Label[i] <- replicate_labels()[row_index]
        }
      }
      
      # Assign concentration only to appropriate categories
      if (category %in% concentration_categories) {
        # Assuming columns 2-11 contain the dilution series
        conc_vals <- concentration_values()
        if (col_num >= 2 && col_num <= 11 && (col_num - 1) <= length(conc_vals)) {
          export_data$Concentration_Value[i] <- conc_vals[col_num - 1]
          export_data$Concentration_Units[i] <- input$concentration_units
        }
      }
    }
    
    # Add metadata if requested
    if (input$include_metadata) {
      export_data$Plate_Format <- input$plate_format
      export_data$Export_Date <- Sys.Date()
      export_data$Export_Time <- Sys.time()
    }
    
    # Reorder columns for better readability
    col_order <- c("Well", "Row", "Column", "Category", "Replicate_Number", 
                   "Replicate_Label", "Concentration_Value", "Concentration_Units")
    
    if (input$include_metadata) {
      col_order <- c(col_order, "Plate_Format", "Export_Date", "Export_Time")
    }
    
    export_data[, col_order[col_order %in% names(export_data)]]
  })
  
  # Render the enhanced plate plot
  output$platePlot <- renderPlotly({
    pd <- render_plate()
    dims <- plate_dims()
    
    # Create column labels (first and last columns empty, middle columns with concentrations)
    col_labels <- character(dims$cols)
    if (dims$cols >= 11) {
      dilution_lbls <- dilution_labels()
      col_labels[2:11] <- dilution_lbls[1:min(10, length(dilution_lbls))]
    }
    
    # Create annotations for row and column labels
    annotations <- list()
    
    # Row labels (replicates)
    for (i in seq_along(dims$row_letters)) {
      row_label <- ""
      if (i <= input$num_replicates && i <= length(replicate_labels())) {
        row_label <- replicate_labels()[i]
      }
      annotations[[length(annotations) + 1]] <- list(
        x = 0.6, y = dims$row_letters[i],
        text = row_label,
        xref = "x", yref = "y",
        showarrow = FALSE,
        xanchor = "right",
        font = list(size = 12, color = "#475569", family = "Inter")
      )
    }
    
    # Column labels (concentrations)
    for (j in 1:dims$cols) {
      annotations[[length(annotations) + 1]] <- list(
        x = j, y = -0.5,
        text = col_labels[j],
        xref = "x", yref = "y",
        showarrow = FALSE,
        yanchor = "bottom",
        textangle = -90,
        font = list(size = 10, color = "#64748b", family = "Inter")
      )
    }
    
    plot_ly(
      data = pd,
      x = ~Col,
      y = ~Row,
      type = "scatter",
      mode = "markers+text",
      text = ~Well,
      textposition = "middle center",
      marker = list(
        size = 40,
        color = pd$Color,
        line = list(width = 2, color = "#ffffff")
      ),
      textfont = list(size = 11, color = "#1e293b", family = "Inter", weight = "bold"),
      source = "plate",
      hovertemplate = paste(
        "<b>Well:</b> %{text}<br>",
        "<b>Category:</b>", pd$Category,
        "<extra></extra>"
      ),
      width = NULL,
      height = 600
    ) %>%
      layout(
        yaxis = list(
          autorange = "reversed", 
          tickvals = dims$row_letters,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        xaxis = list(
          tickvals = 1:dims$cols,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        # Use mode_value() instead of input$mode
        dragmode = if (mode_value() == "Drag") "select" else FALSE,
        annotations = annotations,
        title = list(
          text = paste0("<b>", input$plate_format, "-Well Plate Layout</b>"),
          font = list(size = 18, color = "#1e293b", family = "Inter")
        ),
        plot_bgcolor = "#f8fafc",
        paper_bgcolor = "white",
        font = list(family = "Inter"),
        margin = list(l = 80, r = 50, t = 80, b = 100),
        autosize = TRUE
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })
  
  # Enhanced assignment summary table
  output$assignmentSummary <- renderTable({
    current <- well_assignments()
    summary_data <- current %>%
      group_by(Category) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      filter(Count > 0) %>%
      arrange(desc(Count))
    
    # Add percentage
    summary_data$Percentage <- round(summary_data$Count / sum(summary_data$Count) * 100, 1)
    names(summary_data) <- c("Category", "Wells", "Percentage (%)")
    
    summary_data
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Event handlers - Modified to use mode_value()
  observeEvent(event_data("plotly_click", source = "plate"), {
    req(mode_value() == "Click")  # Changed from input$mode
    click <- event_data("plotly_click", source = "plate")
    pd <- current_plate_data()
    well_clicked <- pd$Well[click$pointNumber + 1]
    current <- well_assignments()
    previous <- current$Category[current$Well == well_clicked]
    current$Category[current$Well == well_clicked] <- input$category
    well_assignments(current)
    history(c(history(), list(data.frame(Well = well_clicked, Category = previous))))
  })
  
  observeEvent(event_data("plotly_selected", source = "plate"), {
    req(mode_value() == "Drag")  # Changed from input$mode
    selected <- event_data("plotly_selected", source = "plate")
    pd <- current_plate_data()
    wells <- pd$Well[selected$pointNumber + 1]
    current <- well_assignments()
    previous <- current[current$Well %in% wells, ]
    current$Category[current$Well %in% wells] <- input$category
    well_assignments(current)
    history(c(history(), list(previous)))
  })
  
  observeEvent(input$right_click_assign, {
    info <- input$right_click_assign
    req(info$well, info$category)
    current <- well_assignments()
    previous <- current$Category[current$Well == info$well]
    current$Category[current$Well == info$well] <- info$category
    well_assignments(current)
    history(c(history(), list(data.frame(Well = info$well, Category = previous))))
  })
  
  observeEvent(input$undo, {
    h <- history()
    if (length(h) > 0) {
      last <- tail(h, 1)[[1]]
      current <- well_assignments()
      for (i in seq_len(nrow(last))) {
        current$Category[current$Well == last$Well[i]] <- last$Category[i]
      }
      well_assignments(current)
      history(h[-length(h)])
    }
  })
  
  observeEvent(input$reset, {
    pd <- current_plate_data()
    well_assignments(data.frame(Well = pd$Well, Category = "Blank"))
    history(list())
  })
  
  # Update filename when plate format changes
  observeEvent(input$plate_format, {
    current_filename <- input$filename
    # Only update if it's still the default format or empty
    if (is.null(current_filename) || current_filename == "" || 
        grepl("^plate_layout_\\d+well_", current_filename)) {
      new_filename <- paste0("my_plate_layout_", input$plate_format, "well_", Sys.Date())
      updateTextInput(session, "filename", value = new_filename)
    }
  })
  
  # Enhanced CSV download
  output$downloadCSV <- downloadHandler(
    filename = function() {
      # Get user input, fallback to default if empty
      user_filename <- input$filename
      if (is.null(user_filename) || user_filename == "") {
        user_filename <- paste0("my_plate_layout_", input$plate_format, "well_", Sys.Date())
      }
      
      # Clean filename (remove any .csv extension if user added it)
      user_filename <- gsub("\\.csv$", "", user_filename)
      
      # Add .csv extension
      paste0(user_filename, ".csv")
    },
    content = function(file) {
      write.csv(enhanced_export_data(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
