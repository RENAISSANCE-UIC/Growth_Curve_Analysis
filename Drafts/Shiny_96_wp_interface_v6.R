library(shiny)
library(plotly)
library(dplyr)

# Define categories and colors
categories <- c("Blank", "Material Control (NP Only)", "Broth Only", 
                "Experiment", "Positive Control (Bacteria Only)")

category_colors <- c(
  "Experiment" = "#8B7B8BEE",
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
  titlePanel("96-Well Plate Editor"),
  sidebarLayout(
    sidebarPanel(
      h5("Plate Setup"),
      selectInput("plate_format", "Plate Format:", 
                  choices = c("96-well (8x12)" = "96", 
                              "48-well (6x8)" = "48", 
                              "24-well (4x6)" = "24"), 
                  selected = "96"),
      
      h5("Well Assignment"),
      selectInput("category", "Select Category:", choices = categories, 
                  selected = "Experiment"),
      radioButtons("mode", "Selection Mode:", choices = c("Click", "Drag"), 
                   selected = "Drag", inline = TRUE),
      
      h5("Experimental Design"),
      numericInput("num_replicates", "Number of Replicates (Rows):", 
                   value = 5, min = 1, max = 7),
      textAreaInput("concentration_values", "Concentrations for Serial Dilution (comma-separated):",
                    value = paste(round(250 / 2^(0:9), 1), collapse = ", "),
                    placeholder = "e.g., 250, 125, 62.5, 31.25, ..."),
      textInput("concentration_units", "Concentration Units:", value = "µg/mL"),
      
      h5("Replicate Labels"),
      textAreaInput("replicate_labels", "Replicate Labels (comma-separated):",
                    value = "Rep 1, Rep 2, Rep 3, Rep 4, Rep 5",
                    placeholder = "e.g., Rep 1, Rep 2, Rep 3, ..."),
      
      h5("Actions"),
      actionButton("undo", "Undo Last"),
      actionButton("reset", "Reset All"),
      downloadButton("downloadCSV", "Download CSV"),
      
      h5("Export Options"),
      checkboxInput("include_metadata", "Include Experimental Metadata", value = TRUE),
      checkboxInput("include_empty_wells", "Include Empty Wells in Export", value = TRUE)
    ),
    mainPanel(
      plotlyOutput("platePlot"),
      br(),
      h5("Well Assignments Summary"),
      tableOutput("assignmentSummary")
    )
  ),
  
  # Custom right-click context menu
  tags$div(
    id = "contextMenu",
    style = "position: absolute; z-index: 10000; background: white; border: 1px solid #ccc; display: none;",
    lapply(categories, function(cat) {
      tags$div(class = "context-item", cat, style = "padding: 5px; cursor: pointer;")
    })
  ),
  
  tags$script(HTML("
    let selectedWell = null;
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
  "))
)

server <- function(input, output, session) {
  # Reactive values
  well_assignments <- reactiveVal(data.frame(Well = plate_data$Well, Category = "Blank"))
  history <- reactiveVal(list())
  
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
  
  # Render the plate plot
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
        font = list(size = 12)
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
        font = list(size = 10)
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
      marker = list(size = 30, color = pd$Color),
      source = "plate"
    ) %>%
      layout(
        yaxis = list(autorange = "reversed", tickvals = dims$row_letters),
        xaxis = list(tickvals = 1:dims$cols),
        dragmode = if (input$mode == "Drag") "select" else FALSE,
        annotations = annotations,
        title = paste("Plate Layout -", input$plate_format, "well format")
      )
  })
  
  # Assignment summary table
  output$assignmentSummary <- renderTable({
    current <- well_assignments()
    summary_data <- current %>%
      group_by(Category) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      filter(Count > 0)
    summary_data
  })
  
  # Event handlers for well assignment (unchanged)
  observeEvent(event_data("plotly_click", source = "plate"), {
    req(input$mode == "Click")
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
    req(input$mode == "Drag")
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
  
  # Enhanced CSV download
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0("plate_layout_", input$plate_format, "well_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(enhanced_export_data(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)

if (!exists("SOURCED_FROM_FUNCTION")) {
  shinyApp(ui, server)
}