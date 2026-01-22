suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(shinyWidgets))
print("VVV All error messages from the app are below this line VVV")

# Determine the month ranges available.
current_date <- str_split(Sys.Date(),"-")
current_year <- as.numeric(current_date[[1]][[1]])
current_month <- as.numeric(current_date[[1]][[2]])
current_day <- as.numeric(current_date[[1]][[3]])
if (current_day >= 10) {
  if (current_month == 1) {
    min_month <- paste0(current_year-1,"-",10)
    max_month <- paste0(current_year-1,"-",12)
  } else if (current_month == 2 | current_month == 3) {
    min_month <- paste0(current_year-1,"-",9+current_month)
    max_month <- paste0(current_year,"-",current_month-1)
  } else {
    min_month <- paste0(current_year,"-",current_month-3)
    max_month <- paste0(current_year,"-",current_month-1)
  }
} else {
  if (current_month == 1 | current_month == 2) {
    min_month <- paste0(current_year-1,"-",8+current_month)
    max_month <- paste0(current_year-1,"-",10+current_month)
  } else if (current_month == 3 | current_month == 4) {
    min_month <- paste0(current_year-1,"-",8+current_month)
    max_month <- paste0(current_year,"-",current_month-2)
  } else {
    min_month <- paste0(current_year,"-",current_month-4)
    max_month <- paste0(current_year,"-",current_month-2)
  }
}
repo_min_month <- "2025-03"
if (current_day >= 2) {
  busrouter_max_month <- paste0(current_year,"-",current_month)
} else {
  busrouter_max_month <- paste0(current_year,"-",current_month-1)
}

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #3F3F3F;
      }
      .import_shift {
        margin-top: 25px;
      }
      .red_text {
        color: #BB0000;
      }
      .green_text {
        color: #00DD00;
      }
      .blue_text {
        color: #2050C0;
      }
      input[type='checkbox'],
      input[type='radio'] {
        accent-color: #007BFF;
      }
      ")),
    tags$script(src = "../www/data_importing.js"),
    tags$script(src = "../www/discord_data_transfer.js")
  ),
  
  titlePanel(tags$p(style = "color: white; text-align: center", "Bus Route Demand Visualiser 1.3.1")),
  sidebarLayout(
    sidebarPanel(
      width = 6,
      style = "background-color: #7F7F7F;",
      tags$div(tags$h4(strong(tags$i(icon("file-import")), "Please import your data from BusRouter and Datamall."))),
      tags$div(tags$h5(strong("Import from Datamall", class = "blue_text"))),
      radioButtons("import_select","Import data from", choices = c("Datamall" = "datamall_import", "Repository" = "repository_import", "File upload" = "file_upload"), inline = T),
      conditionalPanel(
        condition = "input.import_select == 'datamall_import' || input.import_select == 'repository_import'",
        radioButtons("datamall_data_type", "Bus or train data?", choices = c("Bus" = "bus", "Train" = "train"), inline = T),
      ),
      conditionalPanel(
        condition = "input.import_select == 'datamall_import'",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "If the default account key is rate limited, use your own.", class = "red_text"))),
        checkboxInput("use_own_key", "Use your own account key", F),
        conditionalPanel(
          condition = "input.use_own_key == true",
          textInput("own_key", HTML(paste(icon("key"), "Your account key")),value = NA, width = "500px")
        ),
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "Please wait until you receive '... data import from Datamall successful!'.", class = "red_text"))),
        fluidRow(
          splitLayout(
            paste(" "),
            airDatepickerInput("datamall_date", HTML(paste(icon("calendar"), "Select Date")), value = NULL, minDate = min_month, maxDate = max_month, dateFormat = "yyyy-MM", view = "months", minView = "months", width = "100px", addon = "none", readonly = TRUE, autoClose = TRUE),
            div(class = "import_shift", actionButton("import_datamall", "Import from Datamall", width = "180px", icon = icon("file-import"))),
            cellWidths = c("10px","100px","180px")
          )
        )
      ),
      conditionalPanel(
        condition = "input.import_select == 'repository_import'",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "Please wait until you receive 'Datamall ... data import from repository successful!'.", class = "red_text"))),
        fluidRow(
          splitLayout(
            paste(" "),
            airDatepickerInput("od_matrix_date", HTML(paste(icon("calendar"), "Select Date")), value = NULL, minDate = repo_min_month, maxDate = max_month, dateFormat = "yyyy-MM", view = "months", minView = "months", width = "100px", addon = "none", readonly = TRUE, autoClose = TRUE),
            div(class = "import_shift", actionButton("import_repository", "Import from repository", icon = icon("file-import"), width = "180px")),
            cellWidths = c("10px","100px","180px")
          )
        )
      ),
      conditionalPanel(
        condition = "input.import_select == 'file_upload'",
        tags$div(tags$h5(strong(tags$i(icon("circle-info")), "Supports both bus and train origin-destination CSVs.", class = "blue_text"))),
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "Please wait until you receive 'File upload from local storage successful!'.", class = "red_text"))),
        fileInput("data1_in", "Upload LTA Origin-Destination CSV", width = "500px",
                  accept = c(".csv", ".docx", ".doc"),
        )),
      htmlOutput("upload_conf"),
      tags$div(tags$h5(strong("Import from BusRouter", class = "blue_text"))),
      tags$div(tags$h5(strong(tags$i(icon("circle-info")), "You can import MRT/LRT lines and station names data from the repository, just that the date does nothing.", class = "blue_text"))),
      tags$div(tags$h5(strong(tags$i(icon("circle-info")), "File uploading supports both data types, but file names matter. Select 'File upload' to see more info.", class = "blue_text"))),
      radioButtons("import_select2", "Import data from", choices = c("BusRouter" = "busrouter_import", "Repository" = "repository_import", "File upload" = "file_upload"), inline = T),
      conditionalPanel(
        condition = "input.import_select2 == 'busrouter_import'",
        actionButton("import_busrouter", "Import from BusRouter", width = "190px", icon = icon("file-import")),
      ),
      conditionalPanel(
        condition = "input.import_select2 == 'repository_import'",
        radioButtons("json_data_type", "Bus or train data?", choices = c("Bus" = "bus", "Train" = "train"), inline = T),
        fluidRow(
          splitLayout(
            paste(""),
            airDatepickerInput("busrouter_date", HTML(paste(icon("calendar"), "Select Date")), value = NULL, minDate = repo_min_month, maxDate = busrouter_max_month, dateFormat = "yyyy-MM", view = "months", minView = "months", width = "100px", addon = "none", readonly = TRUE, autoClose = TRUE),
            div(class = "import_shift", actionButton("import_repository2", "Import from repository", icon = icon("file-import"), width = "180px")),
            cellWidths = c("10px","100px","180px")
          )
        )
      ),
      conditionalPanel(
        condition = "input.import_select2 == 'file_upload'",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "BusRouter data must follow 'services_YYYYMM.json' and 'stops_YYYYMM.json'!", class = "red_text"))),
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "MRT/LRT lines and station names must be exactly 'stations.json' and 'station_names.json' respectively!", class = "red_text"))),
        fileInput("data2_in", "Upload BusRouter services or MRT/LRT stations JSON", width = "500px",
                  accept = c(".json")),
        fileInput("data3_in", "Upload BusRouter stops or MRT/LRT stop names JSON", width = "500px",
                  accept = c(".json"))
      ),
      htmlOutput("upload_conf2"),
      htmlOutput("upload_conf3"),
      tags$div(tags$h4(strong(tags$i(icon("table")), "Select what type of data to view."))),
      radioButtons("heatmap_type", "Select heatmap type", choices = c("By bus service" = "by_bus_svc", "By specific bus stops" = "by_specific_stops", "By MRT/LRT line" = "by_mrt_line", "By specific MRT/LRT stations" = "by_specific_stns"), inline = T),
      conditionalPanel(
        condition = "input.heatmap_type == 'by_bus_svc'",
        textInput("svc_in", HTML(paste(icon("bus"), "Which bus service would you like to see?")), width = "500px"),
        radioButtons("svc_half_in", HTML(paste(icon("scissors"), "Do you want to split route in half?")), width = "500px", choices = c("Full", "1st half", "2nd half"), inline = T),
        radioButtons("dir1_in", HTML(paste(icon("right-left"), "Which direction? For loop", icon("rotate"), ", put as 1.")), width = "500px", choices = c(1,2), inline = T)
      ),
      conditionalPanel(
        condition = "input.heatmap_type == 'by_specific_stops'",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "The bus stops you listed in the origin box must be paired with a corresponding bus stop in order in the destination box.", class = "red_text"))),
        tags$div(tags$h5(strong(tags$i(icon("circle-info")), "For example, if you put 10009,10011 as origin, 10017,10018 as destination, 10009 pairs with 10017, 10011 pairs with 10018.", class = "blue_text"))),
        textInput("ori_stops", "Which specific origin stops? Put a comma between bus stops.", width = "500px"),
        textInput("dst_stops", "Which specific destination stops? Put a comma between bus stops.", width = "500px")
      ),
      conditionalPanel(
        condition = "input.heatmap_type == 'by_mrt_line'",
        tags$div(tags$h5(strong(tags$i(icon("circle-info")), "West loop (SKPG) and Service A (BP) is dir 1. Direction 1 always counts up (except BPLRT).", class = "blue_text"))),
        textInput("mrt_line_in_1", HTML(paste(icon("train-subway"), "Which origin train line?")), width = "500px"),
        radioButtons("dir2a_in", HTML(paste(icon("right-left"), "Which direction?")), width = "500px", choices = c(1,2), inline = T),
        textInput("mrt_line_in_2", HTML(paste(icon("train-subway"), "Which destination train line?")), width = "500px"),
        radioButtons("dir2b_in", HTML(paste(icon("right-left"), "Which direction?")), width = "500px", choices = c(1,2), inline = T),
      ),
      conditionalPanel(
        condition = "input.heatmap_type == 'by_specific_stns'",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "The stations you listed in the origin box must be paired with a corresponding station in order in the destination box.", class = "red_text"))),
        tags$div(tags$h5(strong(tags$i(icon("circle-info")), "For example, if you put EW1,EW2 as origin, EW3,EW4 as destination, EW1 pairs with EW3, EW2 pairs with EW4.", class = "blue_text"))),
        textInput("ori_stns", "Which specific origin stations? Put a comma between stations.", width = "500px"),
        textInput("dst_stns", "Which specific destination stations? Put a comma between stations.", width = "500px")
      ),
      tags$div(tags$h4(strong(tags$i(icon("filter")),"Please select your filters. Filters available include time and day type filters."))),
      radioButtons("day_filter", HTML(paste(icon("calendar"), "Select Day Type filter")), choices = c("Combined" = "combined","Weekday" = "weekday","Weekend/PH" = "weekend_ph"), selected = c("combined"), inline = T),
      tags$div(tags$h5(strong(tags$i(icon("clock")), "Select Time Period filter"))),
      checkboxInput("time_filter","Filter by Time Period", F),
      conditionalPanel(
        condition = "input.time_filter == true",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "For time filters, give in 24h format. For continuation to next day, let time since be greater than time until. Both equal are treated as full (Why would you do that?).", class = "red_text"))),
        radioButtons("more_time_filters", "Include how many time periods?", choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4"), selected = c("1"), inline = T),
        tags$div(tags$h5(strong("Select Time Period filter 1", class = "blue_text"))),
        sliderInput("time_since1","TP1: Since what hour?", 0, 23, 0, step = 1, animate = F, width = "200px"),
        sliderInput("time_until1","TP2: Until what hour?", 0, 23, 0, step = 1, animate = F, width = "200px"),
        conditionalPanel(
          condition = "input.more_time_filters == '2' || input.more_time_filters == '3' || input.more_time_filters == '4'",
          tags$div(tags$h5(strong("Select Time Period filter 2", class = "blue_text"))),
          sliderInput("time_since2", "TP2: Since what hour?", 0, 23, 0, step = 1, animate = F, width = "200px"),
          sliderInput("time_until2", "TP2: Until what hour?", 0, 23, 0, step = 1, animate = F, width = "200px")
        ),
        conditionalPanel(
          condition = "input.more_time_filters == '3' || input.more_time_filters == '4'",
          tags$div(tags$h5(strong("Select Time Period filter 3", class = "blue_text"))),
          sliderInput("time_since3", "TP3: Since what hour?", 0, 23, 0, step = 1, animate = F, width = "200px"),
          sliderInput("time_until3", "TP3: Until what hour?", 0, 23, 0, step = 1, animate = F, width = "200px")
        ),
        conditionalPanel(
          condition = "input.more_time_filters == '4'",
          tags$div(tags$h5(strong("Select Time Period filter 4", class = "blue_text"))),
          sliderInput("time_since4", "TP4: Since what hour?", 0, 23, 0, step = 1, animate = F, width = "200px"),
          sliderInput("time_until4", "TP4: Until what hour?", 0, 23, 0, step = 1, animate = F, width = "200px")
        ),
      ),
      conditionalPanel(
        condition = "input.heatmap_type == 'by_bus_svc' || input.heatmap_type == 'by_mrt_line'",
        checkboxGroupInput("stop_names", "Display bus stop/station names in", choices = c("Rows" = "row_names", "Columns" = "column_names"), selected = c("row_names", "column_names"), inline = T),
      ),
      conditionalPanel(
        condition = "input.heatmap_type == 'by_specific_stops' || input.heatmap_type == 'by_specific_stns'",
        checkboxInput("stop_names2", "Display bus stop/station names in cells", F),
      ),
      actionButton("generate", "Generate Table", width = "140px", icon = icon("table")),
      htmlOutput("result_conf")
    ),
    mainPanel(
      imageOutput("result_out", inline = T)
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=900*1024^2)
  
  discord_data <- reactive({input$discord_data})
  route1 <- reactive({if (!identical(discord_data()$svc, NULL)) {discord_data()$svc}
    else {if ("by_bus_svc" %in% heatmap_type()) {input$svc_in} else if ("by_mrt_line" %in% heatmap_type()) {input$mrt_line_in_1}}})
  route2 <- reactive({input$mrt_line_in_2})
  pre_dir1 <- reactive({if (!identical(discord_data()$dir, NULL)) {discord_data()$dir}
    else {if ("by_bus_svc" %in% heatmap_type()) {input$dir1_in} else if ("by_mrt_line" %in% heatmap_type()) {input$dir2a_in}}})
  pre_dir2 <- reactive({input$dir2b_in})
  svc_half <- reactive({if (!identical(discord_data()$svc_half, NULL)) {discord_data()$svc_half}
    else {input$svc_half_in}})
  sp_ori <- reactive({if (!identical(discord_data()$sp_ori, NULL)) {discord_data()$sp_ori}
    else {if ("by_specific_stops" %in% heatmap_type()) {input$ori_stops} else if ("by_specific_stns" %in% heatmap_type()) {input$ori_stns}}})
  sp_dst <- reactive({if (!identical(discord_data()$sp_dst, NULL)) {discord_data()$sp_dst}
    else {if ("by_specific_stops" %in% heatmap_type()) {input$dst_stops} else if ("by_specific_stns" %in% heatmap_type()) {input$dst_stns}}})
  heatmap_type <- reactive({if (!identical(discord_data()$type_bool, NULL)) {discord_data()$type_bool}
    else {input$heatmap_type}})
  day_filter <- reactive({if (!identical(discord_data()$day_type, NULL)) {discord_data()$day_type}
    else {input$day_filter}})
  time_filter <- reactive({if (!identical(discord_data()$time_periods, NULL)) {discord_data()$time_period}
    else {input$time_filter}})
  time_periods <- reactive({
    time_since_list <- vector("list", 4)
    time_until_list <- vector("list", 4)
    if (!identical(discord_data()$time_periods, NULL)) {
      for (k in 1:4) {
        time_since_list[[k]] <- input$discord_data[[paste0("period", k)]][["time_since"]]
        time_until_list[[k]]  <- input$discord_data[[paste0("period", k)]][["time_until"]]
      }
    } else {
      for (k in 1:4) {
        time_since_list[[k]] <- input[[paste0("time_since", k)]]
        time_until_list[[k]]  <- input[[paste0("time_until", k)]]
      }
    }
    list(time_since_list = time_since_list, time_until_list = time_until_list)
  })
  display_stop_names <- reactive({list(discord_data()$rows, discord_data()$columns, discord_data()$cells)})
  pre_data1 <- reactiveVal(NULL)
  pre_data2 <- reactiveVal(NULL)
  pre_data3 <- reactiveVal(NULL)
  data_type1 <- reactiveVal(NULL)
  data_type2 <- reactiveVal(NULL)
  data_type3 <- reactiveVal(NULL)
  conf_msg <- reactiveVal("")
  conf_msg2 <- reactiveVal("")
  conf_msg3 <- reactiveVal("")
  result_msg <- reactiveVal("")
  line_cols <- list("EWL" = "#009E52", "NSL" = "#EF1C2A", "NEL" = "#6B3394", "CCL" = "#FCB02A", "DTL" = "#00509F", "TEL" = "#9D5B25", "BPLRT" = "#748477", "SKLRT" = "#748477", "PGLRT" = "#748477")
  cols = colorRamp2(c(0, 1, 30, 300, 1500, 6000, 30000, 99000), c("gray60","white","white", "yellow", "orange", "red", "darkred","black"))
  
  interval2hours <- function(start, end) {
    start <- as.numeric(start)
    end <- as.numeric(end)
    if (start < end) {
      return(start:(end - 1))
    } else if (start > end) {
      # wrap-around: from start to 23 and from 0 up to (end - 1)
      return(c(start:23, if (end > 0) 0:(end - 1) else integer(0)))
    } else {
      # When both are equal, treat as full day.
      return(0:23)
    }
  }
  
  unlist_routes <- function(service) {
    service$routes <- lapply(service$routes, function(route) {
      # Check if all elements in 'route' are atomic.
      if (all(sapply(route, is.atomic))) {
        # Unlist the route, but only one level deep.
        unlist(route, recursive = FALSE, use.names = FALSE)
      }
    })
    service
  }
  
  compound_route <- function(data, source) {
    compound_org <- unique(source$ORIGIN_PT_CODE[grepl("/", source$ORIGIN_PT_CODE)])
    compound_dst <- unique(source$DESTINATION_PT_CODE[grepl("/", source$DESTINATION_PT_CODE)])
    compound_entries <- unique(c(compound_org, compound_dst))
    compound_mapping <- list()
    for (entry in compound_entries) {
      parts <- strsplit(entry, "/", fixed = TRUE)[[1]]
      # For each part, store that the compound version is what we want.
      for (part in parts) {
        # In case the same simple code appears in several compounds,
        # you might choose the first occurrence.
        if (!part %in% names(compound_mapping)) {
          compound_mapping[[part]] <- entry
        }
      }
    }
    updated_data <- sapply(data, function(x) {
      if (x %in% names(compound_mapping)) {compound_mapping[[x]]
      } else {x}
    })
    updated_data
  }
  
  get_line_colour <- function(code) {
    # Each code on the left maps to the line colour on the right.
    codes <- c(
      "^(EW|CG)" = line_cols[["EWL"]],
      "^(NS)"    = line_cols[["NSL"]],
      "^(NE)"    = line_cols[["NEL"]],
      "^(CC|CE)" = line_cols[["CCL"]],
      "^(DT)"    = line_cols[["DTL"]],
      "^(TE)"    = line_cols[["TEL"]],
      "^(BP|SW|SE|PW|PE|STC|PTC)" = line_cols[["BPLRT"]]
    )
    for (pattern in names(codes)) {
      if (grepl(pattern, code)) {
        return(codes[[pattern]])
      }
    }
    return("#000000")  # Default colour if no code matches.
  }
  
  get_labels <- function(show_both, label_type, route1, route2, stop_names, stop_names2) {
    # Determine which stop table to use
    if (label_type == "column") {
      if ("by_bus_svc" %in% heatmap_type()) {
        stops <- stop_names[1:(nrow(stop_names)-1), ]
      } else {
        stops <- stop_names[1:nrow(stop_names), ]
      }
    } else if (label_type == "row") {
      if ("by_bus_svc" %in% heatmap_type()) {
        stops <- stop_names[2:nrow(stop_names), ]
      } else if (route1 == route2) {
        stops <- stop_names[1:nrow(stop_names), ]
      } else {
        stops <- stop_names2[1:nrow(stop_names2), ]
      }
    }
    # Format the label based on whether one or two names are desired.
    # Column 1 are the codes and column 2 are the names.
    if (show_both) {
      if (label_type == "column") {
        return(paste(stops[, 1], "     ", stops[, 2], sep = ""))
      } else {
        return(paste(stops[, 2], "     ", stops[, 1], sep = "")) # Swapped for rows.
      }
    } else {
      # Just return column 1 if not showing stop names.
      return(as.character(stops[, 1]))
    }
  }
  
  decimal_to_base <- function(num, base) {
    digits <- c(0:9, LETTERS)
    result <- ""
    while (num > 0) {
      remainder <- num %% base
      result <- paste0(digits[remainder + 1], result)
      num <- num %/% base
    }
    if (result == "") result <- "0"
    return(result)
  }
  
  base_to_decimal <- function(num_str, base) {
    return(as.numeric(strtoi(num_str, base = base)))
  }

  observeEvent(input$data1_in, {
    conf_msg("")
    req(input$data1_in, "file_upload" %in% input$import_select)
    if (grepl("^origin_destination_bus_", input$data1_in$name)) {
      data_type1("bus")
    } else if (grepl("^origin_destination_train_", input$data1_in$name)) {
      data_type1("train")
    } else {
      data_type1(NULL)
    }
    pre_data1(read.csv(input$data1_in$datapath, colClasses = c("ORIGIN_PT_CODE" = "character", "DESTINATION_PT_CODE" = "character")))
    if (!is.null(pre_data1()) && !is.null(data_type1())) {
      conf_msg(paste("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> Datamall ", if (grepl("^origin_destination_bus_", input$data1_in$name)) "O-D Bus" else "O-D Train"," CSV upload from local storage successful!</span>"))
    } else {
      conf_msg("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Datamall CSV upload failed. Please check for data corruption or correct file format.</span>")
    }
  })
  
  observeEvent(input$data2_in, {
    conf_msg2("")
    req(input$data2_in, "file_upload" %in% input$import_select2)
    if (grepl("^services_", input$data2_in$name)) {
      data_type2("bus")
    } else if (grepl("stations.json", input$data2_in$name)) {
      data_type2("train")
    } else {
      data_type2(NULL)
    }
    uploaded_data2 <- jsonlite::fromJSON(input$data2_in$datapath, simplifyVector = FALSE)
    for (k in seq_along(uploaded_data2)) {
      uploaded_data2[[k]] <- unlist_routes(uploaded_data2[[k]])
    }
    if (!is.null(uploaded_data2[[1]]$routes) && !is.null(data_type2())) {
      pre_data2(uploaded_data2)
      conf_msg2(paste("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i>", if (input$data2_in$name == "stations.json") "MRT/LRT lines JSON" else "BusRouter services JSON", " upload from local storage successful!</span>"))
    } else {
      conf_msg2("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> BusRouter Services JSON upload failed. Please check for data corruption or correct file format.</span>")
    }
  })
  
  observeEvent(input$data3_in, {
    conf_msg3("")
    req(input$data3_in, "file_upload" %in% input$import_select2)
    if (grepl("^stops_", input$data3_in$name)) {
      data_type3("bus")
    } else if (grepl("station_names.json", input$data3_in$name)) {
      data_type3("train")
    } else {
      data_type3(NULL)
    }
    uploaded_data3 <- jsonlite::fromJSON(input$data3_in$datapath, simplifyVector = FALSE)
    if (("Bt Merah Int" %in% unlist(uploaded_data3) || "Pasir Ris" %in% unlist(uploaded_data3)) && !is.null(data_type3())) {
      pre_data3(uploaded_data3)
      conf_msg3(paste("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i>", if (input$data3_in$name == "station_names.json") "MRT/LRT station names JSON" else "BusRouter stops JSON", " upload from local storage successful!</span>"))
    } else {
      conf_msg3("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> BusRouter Stops JSON upload failed. Please check for data corruption or correct file format.</span>")
    }
  })

  datamall_params <- eventReactive(input$import_datamall, {
    if (input$use_own_key) {
      account_key <- input$own_key
    } else {
      account_key <- "default_key_1"
    }
    list(
      account_key = account_key
    )
  })
  
  observeEvent(datamall_params(), {
    if ("bus" %in% input$datamall_data_type) {
      data_type1("bus")
    } else {
      data_type1("train")
    }
    session$sendCustomMessage("fetch_datamall", datamall_params())
  })
  
  observeEvent(input$import_busrouter, {
    conf_msg3("")
    data_type2("bus")
    data_type3("bus")
    session$sendCustomMessage("fetch_busrouter", "")
  })
  
  observeEvent(input$import_repository, {
    conf_msg3("")
    # Add a selection for either bus or MRT data later.
    if ("bus" %in% input$datamall_data_type) {
      data_type1("bus")
    } else {
      data_type1("train")
    }
    session$sendCustomMessage("fetch_drive_datamall", "")
  })
  
  observeEvent(input$import_repository2, {
    conf_msg3("")
    if (input$json_data_type == "bus") {
      data_type2("bus")
      data_type3("bus")
    } else {
      data_type2("train")
      data_type3("train")
    }
    session$sendCustomMessage("fetch_drive_busrouter", "")
  })
  
  observeEvent(input$csv_data_in$data1, {
    pre_data1(read.csv(text = input$csv_data_in$data1, colClasses = c("ORIGIN_PT_CODE" = "character", "DESTINATION_PT_CODE" = "character")))
  })
  
  observeEvent(input$discord_data$data1, {
    pre_data1(read.csv(text = input$discord_data$data1, colClasses = c("ORIGIN_PT_CODE" = "character", "DESTINATION_PT_CODE" = "character")))
  })
  
  observeEvent(input$json_data_in, {
    busrouter_data <- fromJSON(input$json_data_in)
    pre_data2(busrouter_data$data2)
    pre_data3(busrouter_data$data3)
  })
  
  result <- eventReactive(list(input$generate, discord_data()), {
    if (identical(discord_data(), NULL)) {
      req(input$generate)
    } else {
      req(!is.null(discord_data()))
    }
    if (is.null(data_type1()) || is.null(data_type2()) || is.null(data_type3())) {
      tryCatch({
        result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> You have not uploaded some or all required data. What do you wanna see?!</span>"))
        stop("Required data not defined.")
      }, error = function(e) {
        print(e$message)
        stop()
      })
    }
    if (data_type1() != data_type2() || data_type2() != data_type3()) {
      tryCatch({
        result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Your uploaded CSV is of type '", data_type1(), "', uploaded JSON are of types '", data_type2(), "', '", data_type3(), "', make up your mind!</span>"))
        stop("Data type mismatch.")
      }, error = function(e) {
        print(e$message)
        stop()
      })
    }
    if (data_type1() == "bus") {data_type <- "bus"} else {data_type <- "train"}
    if (heatmap_type() == "by_bus_svc" || heatmap_type() == "by_specific_stops") {
      heatmap_data_type = "bus"
    } else {
      heatmap_data_type = "train"
    }
    if (data_type != heatmap_data_type) {
      tryCatch({
        result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> You uploaded some ", data_type, " data, but you selected a heatmap type that needs ", heatmap_data_type, " data. Make up your mind!</span>"))
        stop("Selection of heatmap type not equal to type of uploaded data.")
      }, error = function(e) {
        print(e$message)
        stop()
      })
    }
    data1 <- pre_data1()
    data2 <- pre_data2()
    data3 <- pre_data3()
    route1 <- as.character(route1())
    route2 <- as.character(route2())
    dir1 <- as.numeric(pre_dir1())
    dir2 <- as.numeric(pre_dir2())
    if (identical(day_filter(), "combined")) {
      day_type <- "Combined"
    } else if (identical(day_filter(), "weekday")) {
      day_type <- "Weekday"
    } else if (identical(day_filter(), "weekend_ph")) {
      day_type <- "Weekend/PH"
    }
    filter_day_type <- if ("weekday" %in% day_filter()) {
      quo(DAY_TYPE == "WEEKDAY")
    } else if ("weekend_ph" %in% day_filter()) {
      quo(DAY_TYPE == "WEEKENDS/HOLIDAY")
    } else {
      quo(TRUE)
    }
    if (identical(time_filter(), TRUE)) {
      time_period <- NULL
      valid_hours <- NULL
      if (!identical(discord_data()$time_periods, NULL)) {
        defined_periods <- discord_data()$time_periods
        for (name in names(defined_periods)) {
          period_start <- defined_periods[[name]][["time_since"]]
          period_end   <- defined_periods[[name]][["time_until"]]
          period_hours <- interval2hours(period_start, period_end)
          valid_hours <- sort(unique(c(valid_hours, period_hours)))
          if (is.null(time_period)) {
            time_period <- paste0("From ", if (nchar(period_start) == 2) period_start else paste0("0", period_start), ":00 to ", if (nchar(period_end) == 2) period_end else paste0("0", period_end), ":00")
          } else {
            extra_period <- paste0(if (nchar(period_start) == 2) period_start else paste0("0", period_start), ":00 to ", if (nchar(period_end) == 2) period_end else paste0("0", period_end), ":00")
            time_period <- paste0(time_period, ", ", extra_period)
          }
        }
      } else {
        total_periods <- as.numeric(input$more_time_filters)
        for (i in 1:total_periods) {
          period_start <- time_periods()$time_since_list[[i]]
          period_end <- time_periods()$time_until_list[[i]]
          period_hours <- interval2hours(period_start, period_end)
          valid_hours <- sort(unique(c(valid_hours, period_hours)))
          if (i == 1) {
            time_period <- paste0("From ", if (nchar(period_start) == 2) period_start else paste0("0", period_start), ":00 to ", if (nchar(period_end) == 2) period_end else paste0("0", period_end), ":00")
          } else {
            extra_period <- paste0(if (nchar(period_start) == 2) period_start else paste0("0", period_start), ":00 to ", if (nchar(period_end) == 2) period_end else paste0("0", period_end), ":00")
            time_period <- paste0(time_period, ", ", extra_period)
          }
        }
      }
      if (length(valid_hours) == 24) {time_period <- "Full Day"}
      filter_time_period <- quo(TIME_PER_HOUR %in% valid_hours)
    } else {
      filter_time_period <- quo(TRUE)
      time_period <- "Full Day"
    }
    if ("by_bus_svc" %in% heatmap_type() || "by_mrt_line" %in% heatmap_type()) {
      stop_cur <- data2[[route1]]$routes[[dir1]]
      if (is.null(stop_cur)) {
        tryCatch({
          if (heatmap_data_type == "bus") {
            result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Invalid bus service. Is your bus service withdrawn?</span>"))
            stop("Invalid bus service.")
          } else {
            result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Invalid MRT/LRT line. Did you crayon your own?</span>"))
            stop("Invalid MRT/LRT line.")
          }
        }, error = function(e) {
          print(e$message)
          stop()
        })
      }
      if ("by_mrt_line" %in% heatmap_type()) {
        stop_cur2 <- data2[[route2]]$routes[[dir2]]
        if (is.null(stop_cur2)) {
          tryCatch({
            result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Invalid MRT/LRT line. Did you crayon your own?</span>"))
            stop("Invalid MRT/LRT line.")
          }, error = function(e) {
            print(e$message)
            stop()
          })
        }
      }
      if ("by_bus_svc" %in% heatmap_type()) {
        origin <- data3[[(stop_cur[1])]][[3]]
        terminus <- data3[[(stop_cur[length(stop_cur)])]][[3]]
      } else {
        terminus <- data3[[(stop_cur[length(stop_cur)])]][[1]]
        terminus2 <- data3[[(stop_cur2[length(stop_cur2)])]][[1]]
      }
      is_2way <- length(data2[[route1]]$routes)
      stop_half <- round(length(stop_cur)/2)
      if ("by_bus_svc" %in% heatmap_type()) {
        stop_half_opt = svc_half()
        if (is_2way == 2){
          if (identical(stop_half_opt, "1st half")){
            stop_cur1a <- stop_cur[1:(stop_half+3)]
            dir_graph <- paste0("Direction ",dir1,"\n(",stop_half_opt,")")
          } else if (identical(stop_half_opt, "2nd half")) {
            stop_cur1a <- stop_cur[(stop_half-3):length(stop_cur)]
            dir_graph <- paste0("Direction ",dir1,"\n(",stop_half_opt,")")
          } else {
            stop_cur1a <- stop_cur
            dir_graph <- paste("Direction ",dir1)
          } 
        } else {
          if (identical(stop_half_opt, "1st half")){
            stop_cur1a <- stop_cur[1:(3+stop_half)]
            dir_graph <- paste0("Direction 1\n(",stop_half_opt,")")
          } else if (identical(stop_half_opt, "2nd half")) {
            stop_cur1a <- stop_cur[(stop_half-2):length(stop_cur)]
            dir_graph <- paste0("Direction 1\n(",stop_half_opt,")")
          } else {
            stop_cur1a <- stop_cur
            if (origin == terminus) {dir_graph <- "Loop Svc"} else {dir_graph <- "Direction 1"}
          }}
      } else {
          stop_cur1a <- stop_cur
          dir_grah <- paste("dir",dir1)
          dir_graph2 <- paste("dir",dir2)
      }
      V <- length(stop_cur1a)
      stop_names <- data.frame(as.character(c(stop_cur1a)),c(1:V))
      if ("by_bus_svc" %in% heatmap_type()) {
        for (j in 1:V){
          stop_names[j,2] <- data3[[(stop_cur1a[j])]][[3]]
        }
      } else {
        for (j in 1:V){
          stop_names[j,2] <- data3[[(stop_cur1a[j])]][[1]]
        }
      }
      if ("by_mrt_line" %in% heatmap_type() && route1 != route2) {
        W <- length(stop_cur2)
        stop_names2 <- data.frame(as.character(c(stop_cur2)),c(1:W))
        for (j in 1:W){
          stop_names2[j,2] <- data3[[(stop_cur2[j])]][[1]]
        }
      } else {
        stop_names2 <- NULL
      }
      stop_cur1a <- compound_route(stop_cur1a, data1)
      if ("by_mrt_line" %in% heatmap_type()) {
        stop_cur2 <- compound_route(stop_cur2, data1)
      }
      stop_cur0a <- data.frame(org = 1:V, ORIGIN_PT_CODE = stop_cur1a)
      if ("by_bus_svc" %in% heatmap_type() || route1 == route2) {
        stop_cur0b <- data.frame(dst = 1:V, DESTINATION_PT_CODE = stop_cur1a)
      } else {
        stop_cur0b <- data.frame(dst = 1:W, DESTINATION_PT_CODE = stop_cur2)
      }
      dataod1 <- data1 %>% {if ("by_bus_svc" %in% heatmap_type() || route1 == route2) {
        filter(., ORIGIN_PT_CODE %in% stop_cur1a | is.na(ORIGIN_PT_CODE),
          DESTINATION_PT_CODE %in% stop_cur1a | is.na(DESTINATION_PT_CODE),
          !!filter_day_type, !!filter_time_period) 
        } else {
        filter(., ORIGIN_PT_CODE %in% stop_cur1a | is.na(ORIGIN_PT_CODE),
          DESTINATION_PT_CODE %in% stop_cur2 | is.na(DESTINATION_PT_CODE),
          !!filter_day_type, !!filter_time_period) 
        }} %>%  
        group_by(ORIGIN_PT_CODE, DESTINATION_PT_CODE) %>%
        summarise(total = sum(TOTAL_TRIPS, na.rm = TRUE), .groups = "drop")
      missing_org <- setdiff(stop_cur1a, unique(dataod1$ORIGIN_PT_CODE)) # Check for missing origin stops
      if (length(missing_org) > 0) {
        extra_rows <- tibble(ORIGIN_PT_CODE = unique(missing_org), DESTINATION_PT_CODE = NA, total = 0)  
        dataod1 <- bind_rows(dataod1, extra_rows) %>%
          distinct(ORIGIN_PT_CODE, DESTINATION_PT_CODE, .keep_all = TRUE)  # Ensure uniqueness
      }
      if ("by_bus_svc" %in% heatmap_type() || route1 == route2) {
        missing_dst <- setdiff(stop_cur1a, unique(dataod1$DESTINATION_PT_CODE)) # Check for missing destination stops
        if (length(missing_dst) > 0) {
          extra_rows <- tibble(ORIGIN_PT_CODE = missing_dst, DESTINATION_PT_CODE = missing_dst, total = 0)  # Ensure uniqueness
          dataod1 <- bind_rows(dataod1, extra_rows) %>%
            filter(DESTINATION_PT_CODE %in% stop_cur1a)  # Ensure proper alignment
        }
      } else {
        missing_dst <- setdiff(stop_cur2, unique(dataod1$DESTINATION_PT_CODE)) # Check for missing destination stops
        if (length(missing_dst) > 0) {
          extra_rows <- tibble(ORIGIN_PT_CODE = missing_dst, DESTINATION_PT_CODE = missing_dst, total = 0)  # Ensure uniqueness
          dataod1 <- bind_rows(dataod1, extra_rows) %>%
            filter(DESTINATION_PT_CODE %in% stop_cur2)  # Ensure proper alignment
        }
      }
      # Creates a table to note down the order of bus stops along a bus service or MRT line for each O-D pair
      dataod1a <- full_join(dataod1, stop_cur0a, by="ORIGIN_PT_CODE")
      dataod1a <- full_join(dataod1a, stop_cur0b, by="DESTINATION_PT_CODE")
      dataod1a$total <- as.numeric(dataod1a$total) # Convert demand to numeric form
      # Groups into a table denoting order of bus stops/MRT stations and the demand
      dataod1b <- dataod1a[,c(5,4,3)]
      # Converts to O-D matrix form.
      dataod1c <- dataod1b %>%
        spread(key = org, value = total, fill = NA)
      dataod1c[is.na(dataod1c)] <- 0 # Converts N.A. to 0 demand
      dataod1c <- dataod1c %>%
        filter(dst != 0)  # Keeps all valid rows, removes the incorrect one
      # For column names, only take away 1st destination and final origin stop if of type
      # "by_bus_svc" or if both MRT lines are the same
      if ("by_bus_svc" %in% heatmap_type()) {
        stop_cur1b <- stop_cur1a[1:V-1]
        stop_cur1c <- stop_cur1a[2:V]
      } else {
        if (route1 == route2) {
          stop_cur1b <- stop_cur1a[1:V]
          stop_cur1c <- stop_cur1a[1:V]
        } else {
          stop_cur1b <- stop_cur1a[1:V]
          stop_cur1c <- stop_cur2[1:W]
        }
      }
      dataod1c <- as.matrix(dataod1c) # To matrix
      # Removes 1st col which is descriptor 
      dataod1c <- dataod1c[,-1]
      # Removes only the 1st row and last col if of type "by_bus_svc"
      if ("by_bus_svc" %in% heatmap_type()) {
        if (ncol(dataod1c)!=(V-1)){
          dataod1c <- dataod1c[,-ncol(dataod1c)]
        }
        if (nrow(dataod1c)!=(V-1)){
          dataod1c <- dataod1c[-1,]
        }
      }
      # Insert column/row names used for ordering
      colnames(dataod1c) <- paste(stop_cur1b)
      rownames(dataod1c) <- paste(stop_cur1c)
      if ("by_bus_svc" %in% heatmap_type() || route1 == route2) {
        max_length <- 2 * max(nchar(stop_names[,2]))
      } else {
        max_length <- 2 * max(nchar(stop_names2[,2]))
      }
      # Row and column names
      stop_names_in_column <- ("column_names" %in% input$stop_names || display_stop_names()$columns == TRUE)
      column_labels <- get_labels(stop_names_in_column, "column", route1, route2, stop_names, stop_names2)
      stop_names_in_row <- ("row_names" %in% input$stop_names || display_stop_names()$rows == TRUE)
      row_labels <- get_labels(stop_names_in_row, "row", route1, route2, stop_names, stop_names2)
      # Heatmap legend names
      lgd_name1 <- if ("by_bus_svc" %in% heatmap_type()) {
        paste("O-D matrix (en-route)\n",data1$YEAR_MONTH[[1]],"\n",day_type," Demand\n\nService ",route1,"\n",dir_graph,"\n",terminus," Bound\n",sep = "")
      } else if (route1 == route2) {
        paste("O-D matrix (en-route)\n",data1$YEAR_MONTH[[1]],"\n",day_type," Demand\n\n",route1," dir ",dir1," as origin\nand destinatn\n",terminus," Bound\n",sep = "")
      } else {
        paste("O-D matrix (en-route)\n",data1$YEAR_MONTH[[1]],"\n",day_type," Demand\n\n",route1," dir ",dir1," as origin\n",route2," dir ",dir2," as destinatn\n",route1," ",terminus," Bound\n",route2," ",terminus2," Bound\n",sep = "")
      }
      col_title1 <- if ("by_bus_svc" %in% heatmap_type()) {
        paste0(time_period,"\nOrigin Bus Stops")
      } else {
        paste0(time_period,"\n",route1," dir ",dir1," as origin line")
      }
      row_title1 <- if ("by_bus_svc" %in% heatmap_type()) {
        paste("Destination Bus Stops")
      } else {
        paste(route2," dir ",dir2,"as destination line")
      }
      # Coloured text for lines
      line_col1a <- if ("by_mrt_line" %in% heatmap_type()) {line_cols[[route1]]} else {"#000000"}
      line_col1b <- if ("by_mrt_line" %in% heatmap_type()) {line_cols[[route2]]} else {"#000000"}
      # Heatmap size
      img_dims <- list(width = 39 * ncol(dataod1c) + 320, height = 22 * nrow(dataod1c) + 200)
      # Heatmap config
      img <- Heatmap(dataod1c,
       name = lgd_name1,
       show_column_dend = FALSE,
       show_row_dend = FALSE,
       row_dend_reorder = FALSE,
       column_dend_reorder = FALSE,
       column_title = col_title1,
       column_title_side = "top",
       row_title = row_title1,
       row_names_side = "left",
       column_names_side = "top",
       column_names_rot = 40,
       column_labels = column_labels,
       row_labels = row_labels,
       col = cols,
       na_col = "gray60",
       column_gap = unit(2, "mm"),
       cluster_rows = FALSE,
       cluster_columns = FALSE,
       row_order = stop_cur1c,
       column_order = stop_cur1b, 
       row_names_gp = gpar(fontsize = pmin(ncol(dataod1c) / 3 + 9, 15), col = line_col1b, just = "right"),
       column_names_gp = gpar(fontsize = pmin(ncol(dataod1c) / 3 + 9, 15), col = line_col1a),
       row_title_gp = gpar(fontsize = pmin(ncol(dataod1c) / 3 + 12, 25), col = line_col1b, just = "left"),
       column_title_gp = gpar(fontsize = pmin(ncol(dataod1c) / 3 + 12, 25), col = line_col1a),
       row_names_max_width = unit(max_length, "cm"),
       column_names_max_height = unit(max_length, "cm"),
       heatmap_legend_param = list(labels_gp = gpar(fontsize = pmin(ncol(dataod1c) / 3 + 9, 18)), legend_height = unit(pmin(nrow(dataod1c) / 4 + 1, 8), "cm"), at = c(0, 300, 1500, 6000, 30000, 99000), legend_width = unit(2, "cm"), color_bar = "continuous", title_gp = gpar(fontsize = pmin(ncol(dataod1c) / 3 + 9, 18), fontface = 'bold'), break_dist = 1),
       cell_fun = function(j, i, x, y, width, height, fill) {
         if(dataod1c[i, j] > 10000){
           grid.text(sprintf("%.0f", dataod1c[i, j]), x, y, gp = gpar(fontsize = 13, col = "white"))
         } else if (dataod1c[i, j] > 5000) {
           grid.text(sprintf("%.0f", dataod1c[i, j]), x, y, gp = gpar(fontsize = 14, col = "white"))
         } else if(dataod1c[i, j] > 29){
           grid.text(sprintf("%.0f", dataod1c[i, j]), x, y, gp = gpar(fontsize = 14))
         }
       },
       rect_gp = gpar(col = "black", lwd = 0.2))
    } else {
      ori_stops <- str_split(sp_ori(), ",")
      dst_stops <- str_split(sp_dst(), ",")
      l_ori <- length(ori_stops[[1]])
      l_dst <- length(dst_stops[[1]])
      if (l_ori != l_dst) {
        tryCatch({
          result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> The lengths of your origin bus stops/stations and destination bus stops/stations do not match.</span>"))
          stop("Length of origin stops/stations not equal to length of destination stops/stations.")
        }, error = function(e) {
          print(e$message)
          stop()
        })
      }
      dataod2 <- data.frame(c(1:l_ori), c(1:l_ori), c(1:l_ori))
      dataod2a <- data.frame(c(1:l_ori), c(1:l_ori))
      colnames(dataod2) <- c("ori", "dst", "dmd")
      colnames(dataod2a) <- c("ori_name", "dst_name")
      for (t in 1:l_ori) {
        ori_stop <- trimws(ori_stops[[1]][[t]])
        ori_stop2 <- compound_route(ori_stop, data1)
        dst_stop <- trimws(dst_stops[[1]][[t]])
        dst_stop2 <- compound_route(dst_stop, data1)
        dataod2[t, 1] <- ori_stop2
        dataod2[t, 2] <- dst_stop2
        valid_stops <- nrow(filter(data1,
          ORIGIN_PT_CODE %in% dataod2[t, 1] | is.na(ORIGIN_PT_CODE),
          DESTINATION_PT_CODE %in% dataod2[t, 2] | is.na(DESTINATION_PT_CODE),
          !!filter_day_type, !!filter_time_period))
        if (valid_stops == 0) {
          tryCatch({
            result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Invalid bus stop code(s) detected! Check your codes to see if it's a proper O-D pair, or there's absolutely no one going from A to B.</span>"))
            stop("Invalid stop codes or no demand.")
          }, error = function(e) {
            print(e$message)
            stop()
          })
        }
        dataod2[t, 3] <- data1 %>%
          filter(., ORIGIN_PT_CODE == dataod2[t, 1],
            DESTINATION_PT_CODE == dataod2[t, 2],
            !!filter_day_type,
            !!filter_time_period) %>%
          summarise(Total = sum(TOTAL_TRIPS)) %>%
          pull(Total) %>%
          as.numeric()
        if ("row_names" %in% input$stop_names || display_stop_names()$cells == TRUE) {
          if ("by_specific_stops" %in% heatmap_type()) {
            dataod2a[t, 1] <- data3[[as.character(ori_stop)]][[3]]
            dataod2a[t, 2] <- data3[[as.character(dst_stop)]][[3]]
          } else {
            dataod2a[t, 1] <- data3[[as.character(ori_stop)]][[1]]
            dataod2a[t, 2] <- data3[[as.character(dst_stop)]][[1]]
          }
        }
        dataod2[t, 1] <- ori_stop
        dataod2[t, 2] <- dst_stop
      }
      # To base10 from base36, allowing station codes to be stored as numbers
      for (j in 1:2) {
          dataod2[,j] <- base_to_decimal(dataod2[,j],36)
      }
      dataod2 <- as.matrix(dataod2)
      dataod2a <- as.matrix(dataod2a)
      img_dims <- list(width = 520, height = 24 * nrow(dataod2) + 90)
      img <- Heatmap(dataod2,
        name = paste(day_type, "Demand,", time_period),
        show_column_dend = FALSE,
        show_row_dend = FALSE,
        row_dend_reorder = FALSE,
        column_dend_reorder = FALSE,
        column_title = "O-D Matrix for specific stops/stations",
        column_title_side = "top",
        column_names_side = "top",
        column_names_rot = 0,
        column_names_centered = TRUE,
        column_labels = c("Origin", "Destination", "Demand"),
        col = cols,
        na_col = "gray60",
        column_gap = unit(2, "mm"),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        column_names_gp = gpar(fontsize = 15),
        column_title_gp = gpar(fontsize = 25),
        heatmap_legend_param = list(labels_gp = gpar(fontsize = 12), legend_width = unit(10, "cm"), legend_direction = "horizontal", at = c(0, 300, 1500, 6000, 30000, 99000), legend_width = unit(2, "cm"), color_bar = "continuous", break_dist = 1, title_position = "topcenter", heatmap_legend_side = "top", title_gp = gpar(fontsize = 12, fontface = "bold")),
        cell_fun = function(j, i, x, y, width, height, fill) {
          if(j %in% c(1, 2)) {
            # Draw a white background covering the cell.
            grid.rect(
              x = x, y = y, width = width, height = height,
              gp = gpar(fill = "white", col = "black", lwd = 0.2)
            )
            # From base10 to base36, back to station/stop codes.
            code <- decimal_to_base(dataod2[i,j],36)
            if (isTRUE(input$stop_names2) || isTRUE(display_stop_names()$cells)) {
              grid.text(sprintf("%s\n%s", code, dataod2a[i, j]), x, y, gp = gpar(fontsize = 11, col = get_line_colour(code)))
            } else {
              grid.text(sprintf("%s", code), x, y, gp = gpar(fontsize = 15, col = get_line_colour(code)))
            }
          }
          if (j==3) {
            grid.rect(
              x = x, y = y, width = width, height = height,
              gp = gpar(fill = fill, col = NA)
            )
            if(dataod2[i, j] > 5000){
              grid.text(sprintf("%.0f", dataod2[i, j]), x, y, gp = gpar(fontsize = 15, col = "white"))
            }
            else if(dataod2[i, j] >= 0){
              grid.text(sprintf("%.0f", dataod2[i, j]), x, y, gp = gpar(fontsize = 15))
            }
          }
        },
        rect_gp = gpar(col = "black", lwd = 0.2))
      }
      list(img = img, img_dims = img_dims)
  })
  draw_heatmap <- function(heatmap, img_dims) {
    # Retrieve dimensions
    img_width  <- 1.5 * img_dims$width
    img_height <- 1.5 * img_dims$height
    
    # Create a temporary PNG file
    temp_file <- tempfile(fileext = ".png")
    while (dev.cur() > 1L) {
      dev.off()
    }
    png(filename = temp_file, width = img_width, height = img_height, units = "px", res = 96)
    grid::grid.newpage()
    heatmap_type_names = list("by_bus_svc" = "'By bus service'" , "by_specific_stops" = "'By specific stops'", "by_mrt_line" = "By MRT/LRT line", "by_specific_stns" = "By specific stations")
    heatmap_type_name = heatmap_type_names[[heatmap_type()]]
    # Draw the heatmap
    if ("by_bus_svc" %in% heatmap_type() || "by_mrt_line" %in% heatmap_type()) {
      req(result())
      draw(result()$img)
      result_msg(paste("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> Heatmap of type",heatmap_type_name,"successfully drawn!</span>"))
    } else {
      req(result())
      draw(result()$img, heatmap_legend_side = "top")
      result_msg(paste("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> Heatmap of type",heatmap_type_name,"successfully drawn!</span>"))
    }
    dev.off()
    list(
      src = temp_file,
      contentType = "image/png",
      width = img_width,
      height = img_height,
      alt = "Demand Heatmap"
    )
  }
  output$result_out <- renderImage({
    req(result())
    draw_heatmap(result()$img, result()$img_dims)
  }, deleteFile = FALSE)
  discord_image <- eventReactive(result(), {
    req(result())
    discord_img <- draw_heatmap(result()$img, result()$img_dims)
    session$sendCustomMessage("send_image", discord_img)
  })
  observe(result())
  output$upload_conf <- renderText({HTML(conf_msg())})
  output$upload_conf2 <- renderText({HTML(conf_msg2())})
  output$upload_conf3 <- renderText({HTML(conf_msg3())})
  output$result_conf <- renderText({HTML(result_msg())})
}

shinyApp(ui, server)
shinyApp(ui = ui, server = server)