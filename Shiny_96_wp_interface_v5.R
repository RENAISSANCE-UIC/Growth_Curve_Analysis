
library(shiny)
library(plotly)
library(dplyr)

# Define categories and colors
categories <- c("Blank", "Material Control (NP Only)", "Broth Only", 
                "Experiment", "Positive Control (Bacteria Only)")

category_colors <- c(
  "Blank" = "#BEBEBE",
  "Material Control (NP Only)" = "#104E8B",
  "Broth Only" = "#DAA520",
  "Experiment" = "#8B1A1A",
  "Positive Control (Bacteria Only)" = "#8B7B8B"
)

# Generate well positions
rows <- LETTERS[1:8]
cols <- 1:12
plate_data <- expand.grid(Row = rows, Col = cols)
plate_data$Well <- paste0(plate_data$Row, plate_data$Col)

ui <- fluidPage(
  titlePanel("96-Well Plate Editor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category:", choices = categories, 
                  selected = "Experiment"),
      radioButtons("mode", "Selection Mode:", choices = c("Click", "Drag"), 
                   selected = "Drag", inline = TRUE),
      numericInput("num_replicates", "Number of Replicates (Rows):", 
                   value = 5, min = 1, max = 8),
      textAreaInput("concentration_values", "Concentrations for Columns 2–11 (comma-separated):",
                    value = paste(round(250 / 2^(0:9), 2), collapse = ", ")),
      actionButton("undo", "Undo Last"),
      actionButton("reset", "Reset All"),
      downloadButton("downloadCSV", "Download CSV")
    ),
    mainPanel(
      plotlyOutput("platePlot"),
      #verbatimTextOutput("wellAssignments")
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
  well_assignments <- reactiveVal(data.frame(Well = plate_data$Well, Category = "Blank"))
  history <- reactiveVal(list())
  
  row_labels <- reactive({
    paste("Rep", seq_len(input$num_replicates))
  })
  
  dilution_labels <- reactive({
    vals <- unlist(strsplit(input$concentration_values, ","))
    vals <- suppressWarnings(as.numeric(trimws(vals)))
    vals <- vals[!is.na(vals)]
    paste0(round(vals, 2), " µg/mL")
  })
  
  render_plate <- reactive({
    current <- well_assignments()
    plate_data$Category <- current$Category[match(plate_data$Well, current$Well)]
    plate_data$Color <- category_colors[plate_data$Category]
    plate_data$Color[is.na(plate_data$Color)] <- "lightgray"
    plate_data
  })
  
  output$platePlot <- renderPlotly({
    pd <- render_plate()
    col_labels <- c("", dilution_labels(), "")
    
    annotations <- list()
    for (i in seq_along(rows)) {
      annotations[[length(annotations) + 1]] <- list(
        x = 0.6, y = rows[i],
        text = if (i <= input$num_replicates) row_labels()[i] else "",
        xref = "x", yref = "y",
        showarrow = FALSE,
        xanchor = "right",
        font = list(size = 12)
      )
    }
    for (j in seq_along(cols)) {
      annotations[[length(annotations) + 1]] <- list(
        x = cols[j], y = -0.5,
        text = col_labels[j],
        xref = "x", yref = "y",
        showarrow = FALSE,
        yanchor = "bottom",
        textangle = -90,
        font = list(size = 12)
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
        yaxis = list(autorange = "reversed", tickvals = rows),
        xaxis = list(tickvals = cols),
        dragmode = if (input$mode == "Drag") "select" else FALSE,
        annotations = annotations
      )
  })
  
  observeEvent(event_data("plotly_click", source = "plate"), {
    req(input$mode == "Click")
    click <- event_data("plotly_click", source = "plate")
    well_clicked <- plate_data$Well[click$pointNumber + 1]
    current <- well_assignments()
    previous <- current$Category[current$Well == well_clicked]
    current$Category[current$Well == well_clicked] <- input$category
    well_assignments(current)
    history(c(history(), list(data.frame(Well = well_clicked, Category = previous))))
  })
  
  observeEvent(event_data("plotly_selected", source = "plate"), {
    req(input$mode == "Drag")
    selected <- event_data("plotly_selected", source = "plate")
    wells <- plate_data$Well[selected$pointNumber + 1]
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
    well_assignments(data.frame(Well = plate_data$Well, Category = "Blank"))
    history(list())
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0("well_assignments_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(na.omit(well_assignments()), file, row.names = FALSE)
    }
  )
  
  output$wellAssignments <- renderPrint({
    na.omit(well_assignments())
  })
}

shinyApp(ui, server)
