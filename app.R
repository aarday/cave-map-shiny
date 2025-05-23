library(shiny)
library(dplyr)
library(ggplot2)

deg2rad <- function(deg) {
  return(deg * pi / 180)
}

extract_shots_df <- function(sql_lines) {
  # Simple example: parse shots data from SQL file lines
  shots_lines <- grep("^INSERT into shots values", sql_lines, value = TRUE)
  
  extract_all_records <- function(sql_line) {
    record_matches <- stringr::str_match_all(sql_line, "\\((.*?)\\)")[[1]][,2]
    lapply(record_matches, function(record) {
      fields <- stringr::str_split(record, ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")[[1]]
      fields <- trimws(gsub('^"|"$', '', fields))
      return(fields)
    })
  }
  
  shots_data_list <- unlist(lapply(shots_lines, extract_all_records), recursive = FALSE)
  
  target_col_count <- 21
  shots_data_list_fixed <- lapply(shots_data_list, function(row) {
    len <- length(row)
    if (len < target_col_count) {
      c(row, rep(NA, target_col_count - len))
    } else if (len > target_col_count) {
      row[1:target_col_count]
    } else {
      row
    }
  })
  
  shots_df <- as.data.frame(do.call(rbind, shots_data_list_fixed), stringsAsFactors = FALSE)
  
  colnames(shots_df) <- c(
    "survey_id", "shot_id", "from", "to", "length", "azimuth", "inclination", 
    "compass", "x", "y", "z", "flags", "deleted", "clino", "tape", "comment",
    "direction", "timestamp", "quality", "uncertainty"
  )
  
  numeric_cols <- c("survey_id", "shot_id", "length", "azimuth", "inclination", 
                    "compass", "x", "y", "z", "flags", "deleted", "clino", "tape", 
                    "direction", "timestamp", "quality", "uncertainty")
  
  shots_df[numeric_cols] <- lapply(shots_df[numeric_cols], as.numeric)
  
  shots_df
}

ui <- fluidPage(
  titlePanel("📐 Cave Map - Shot Vectors"),
  sidebarLayout(
    sidebarPanel(
      fileInput("sqlfile", "Upload SQL File (.sql)", accept = ".sql"),
      actionButton("rotate_right", "Rotate 90° Right"),
      actionButton("rotate_left", "Rotate 90° Left")
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

server <- function(input, output, session) {
  # Rotation angle reactive value
  rotation <- reactiveVal(0)
  
  observeEvent(input$rotate_right, {
    rotation((rotation() + 90) %% 360)
  })
  
  observeEvent(input$rotate_left, {
    rotation((rotation() - 90) %% 360)
  })
  
  data_shots <- reactive({
    req(input$sqlfile)
    sql_lines <- readLines(input$sqlfile$datapath, encoding = "UTF-8")
    shots_df <- extract_shots_df(sql_lines)
    
    shots_df %>%
      select(shot_id, from, to, length, azimuth, inclination, compass) %>%
      mutate(
        from = as.numeric(trimws(gsub('"', '', from))),
        to   = as.numeric(trimws(gsub('"', '', to))),
        azimuth = (azimuth + rotation()) %% 360
      ) %>%
      filter(!is.na(length), length > 0, !is.na(azimuth))
  })
  
  output$mapPlot <- renderPlot({
    shots <- data_shots()
    
    # Starting station (id=0) coordinates
    stations <- data.frame(id = 0, x = 0, y = 0, stringsAsFactors = FALSE)
    
    # Calculate shot vector components
    shots <- shots %>%
      mutate(
        azimuth_rad = deg2rad(azimuth),
        dx = -length * sin(azimuth_rad),
        dy = length * cos(azimuth_rad)
      )
    
    # Calculate station coordinates
    for (i in 1:nrow(shots)) {
      from_id <- shots$from[i]
      to_id <- shots$to[i]
      
      if (!(from_id %in% stations$id)) next
      if (is.na(to_id)) next
      
      from_row <- stations %>% filter(id == from_id)
      
      if (!(to_id %in% stations$id)) {
        new_x <- from_row$x + shots$dx[i]
        new_y <- from_row$y + shots$dy[i]
        
        stations <- bind_rows(stations, data.frame(id = to_id, x = new_x, y = new_y))
      }
    }
    
    # Prepare segments for plotting
    segments <- shots %>%
      left_join(stations, by = c("from" = "id")) %>%
      rename(x = x, y = y) %>%
      left_join(stations, by = c("to" = "id"), suffix = c("_from", "_to")) %>%
      rename(
        x = x_from,
        y = y_from,
        xend = x_to,
        yend = y_to
      )
    
    ggplot(segments) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                   color = "darkblue", size = 0.7) +
      geom_text(aes(x = x, y = y, label = from), color = "red", size = 3, vjust = -1) +
      geom_text(aes(x = xend, y = yend, label = to), color = "green", size = 3, vjust = -1) +
      coord_fixed(ratio = 1) +
      theme_minimal() +
      labs(title = "Cave Map (2D)",
           x = "X Coordinate",
           y = "Y Coordinate")
  })
}

shinyApp(ui, server)
