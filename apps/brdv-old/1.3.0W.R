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
  
  titlePanel(tags$p(style = "color: white; text-align: center", "Bus Route Demand Visualiser 1.3.0")),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      style = "background-color: #7F7F7F;",
      tags$div(tags$h4(strong(tags$i(icon("file-import")), "Please import your data from BusRouter and Datamall."))),
      tags$div(tags$h5(strong("Import from Datamall", class = "blue_text"))),
      radioButtons("import_select","Import data from", choices = c("Datamall import" = "datamall_import", "Get from repository" = "repository_import", "File upload" = "file_upload"), inline = T),
      conditionalPanel(
        condition = "input.import_select == 'datamall_import'",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "If the default account key is rate limited, use your own.", class = "red_text"))),
        checkboxInput("use_own_key", "Use your own account key", F),
        conditionalPanel(
          condition = "input.use_own_key == true",
          textInput("own_key", HTML(paste(icon("key"), "Your account key")),value = NA, width = "500px")
        ),
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "Please wait until you receive 'File import successful!'.", class = "red_text"))),
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
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "Please wait until you receive 'File import from Datamall successful!'.", class = "red_text"))),
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
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "Please wait until you receive 'File upload from local storage successful!'.", class = "red_text"))),
        fileInput("data1_in", "Upload LTA Origin-Destination CSV", width = "500px",
                  accept = c(".csv", ".docx", ".doc"),
        )),
      htmlOutput("upload_conf"),
      tags$div(tags$h5(strong("Import from BusRouter", class = "blue_text"))),
      radioButtons("import_select2", "Import data from", choices = c("BusRouter import" = "busrouter_import", "Get from repository" = "repository_import", "File upload" = "file_upload"), inline = T),
      conditionalPanel(
        condition = "input.import_select2 == 'busrouter_import'",
        actionButton("import_busrouter", "Import from BusRouter", width = "190px", icon = icon("file-import")),
      ),
      conditionalPanel(
        condition = "input.import_select2 == 'repository_import'",
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
        fileInput("data2_in", "Upload BusRouter Services JSON", width = "500px",
                  accept = c(".json")),
        fileInput("data3_in", "Upload BusRouter Stops JSON", width = "500px",
                  accept = c(".json"))
      ),
      htmlOutput("upload_conf2"),
      htmlOutput("upload_conf3"),
      tags$div(tags$h4(strong(tags$i(icon("table")), "Select what type of data to view."))),
      checkboxInput("seespecstops", "See specific bus stops", F),
      conditionalPanel(
        condition = "input.seespecstops == false",
        textInput("svc_in", HTML(paste(icon("bus"), "Which bus service would you like to see?")), width = "500px"),
        radioButtons("svc_half_in", HTML(paste(icon("scissors"), "Do you want to split route in half?")), width = "500px", choices = c("Full", "1st half", "2nd half"), inline = T),
        radioButtons("dir_in", HTML(paste(icon("right-left"), "Which direction? For loop", icon("rotate"), ", put as 1.")), width = "500px", choices = c(1,2), inline = T)
      ),
      conditionalPanel(
        condition = "input.seespecstops == true",
        tags$div(tags$h5(strong(tags$i(icon("triangle-exclamation")), "The bus stops you listed in the origin box must be paired with a corresponding bus stop in order in the destination box.", class = "red_text"))),
        tags$div(tags$h5(strong(tags$i(icon("circle-info")), "For example, if you put 10009,10011 as origin, 10017,10018 as destination, 10009 pairs with 10017, 10011 pairs with 10018.", class = "blue_text"))),
        textInput("ori_stops", "Which specific origin stops? Put a comma between bus stops.", width = "500px"),
        textInput("dst_stops", "Which specific destination stops? Put a comma between bus stops.", width = "500px")
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
        condition = "input.seespecstops == false",
        checkboxGroupInput("stop_names", "Display bus stop names in", choices = c("Rows" = "row_names", "Columns" = "column_names"), selected = c("row_names", "column_names"), inline = T),
      ),
      conditionalPanel(
        condition = "input.seespecstops == true",
        checkboxInput("stop_names2", "Display bus stop names in cells", F),
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
  svc <- reactive({if (!identical(discord_data()$svc, NULL)) {discord_data()$svc}
    else {input$svc_in}})
  dir <- reactive({if (!identical(discord_data()$dir, NULL)) {discord_data()$dir}
    else {input$dir_in}})
  svc_half <- reactive({if (!identical(discord_data()$svc_half, NULL)) {discord_data()$svc_half}
    else {input$svc_half_in}})
  sp_ori <- reactive({if (!identical(discord_data()$sp_ori, NULL)) {discord_data()$sp_ori}
    else {input$ori_stops}})
  sp_dst <- reactive({if (!identical(discord_data()$sp_dst, NULL)) {discord_data()$sp_dst}
    else {input$dst_stops}})
  spec_stops <- reactive({if (!identical(discord_data()$type_bool, NULL)) {discord_data()$type_bool}
    else {input$seespecstops}})
  day_filter <- reactive({if (!identical(discord_data()$day_type, NULL)) {discord_data()$day_type}
    else {input$day_filter}})
  time_filter <- reactive({if (identical(discord_data()$day_type, NULL) || identical(input$time_filter, FALSE)) {FALSE}
    else {TRUE}})
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
  data1 <- reactiveVal(NULL)
  data2 <- reactiveVal(NULL)
  data3 <- reactiveVal(NULL)
  conf_msg <- reactiveVal("")
  conf_msg2 <- reactiveVal("")
  conf_msg3 <- reactiveVal("")
  result_msg <- reactiveVal("")
  
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

  observeEvent(input$data1_in, {
    conf_msg("")
    req(input$data1_in, "file_upload" %in% input$import_select)
    pre_data1 <- read.csv(input$data1_in$datapath, colClasses = c("ORIGIN_PT_CODE" = "character", "DESTINATION_PT_CODE" = "character"))
    if (!is.null(pre_data1)) {
      data1(pre_data1)
      conf_msg("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> Datamall CSV upload from local storage successful!</span>")
    } else {
      conf_msg("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Datamall CSV upload failed. Please check for data corruption or correct file format.</span>")
    }
  })
  
  observeEvent(input$data2_in, {
    conf_msg2("")
    req(input$data2_in, "file_upload" %in% input$import_select2)
    pre_data2 <- jsonlite::fromJSON(input$data2_in$datapath)
    if (!is.null(pre_data2[[1]]$routes)) {
      data2(pre_data2)
      conf_msg2("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> BusRouter Services JSON upload from local storage successful!</span>")
    } else {
      conf_msg2("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> BusRouter Services JSON upload failed. Please check for data corruption or correct file format.</span>")
    }
  })
  
  observeEvent(input$data3_in, {
    conf_msg3("")
    req(input$data3_in, "file_upload" %in% input$import_select2)
    pre_data3 <- jsonlite::fromJSON(input$data3_in$datapath)
    if (pre_data3[["10009"]][[3]] != "Bukit Merah Int") {
      data3(pre_data3)
      conf_msg3("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> BusRouter Stops JSON upload from local storage successful!</span>")
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
    session$sendCustomMessage("fetch_datamall", datamall_params())
  })
  
  observeEvent(input$import_busrouter, {
    conf_msg2("")
    conf_msg3("")
    session$sendCustomMessage("fetch_busrouter", "")
  })
  
  observeEvent(input$import_repository, {
    conf_msg2("")
    conf_msg3("")
    session$sendCustomMessage("fetch_drive_datamall", "")
  })
  
  observeEvent(input$import_repository2, {
    conf_msg2("")
    conf_msg3("")
    session$sendCustomMessage("fetch_drive_busrouter", "")
  })
  
  observeEvent(input$csv_data_in$data1, {
    pre_data1 <- read.csv(text = input$csv_data_in$data1, colClasses = c("ORIGIN_PT_CODE" = "character", "DESTINATION_PT_CODE" = "character"))
    data1(pre_data1)
  })
  
  observeEvent(input$discord_data$data1, {
    pre_data1 <- read.csv(text = input$discord_data$data1, colClasses = c("ORIGIN_PT_CODE" = "character", "DESTINATION_PT_CODE" = "character"))
    data1(pre_data1)
  })
  
  observeEvent(input$json_data_in, {
    busrouter_data <- fromJSON(input$json_data_in)
    data2(busrouter_data$data2)
    data3(busrouter_data$data3)
  })
  
  result <- eventReactive(list(input$generate, discord_data()), {
    if (identical(discord_data(), NULL)) {
      req(input$generate)
    } else {
      req(!is.null(discord_data()))
    }
    # Check that data1 exists before proceeding.
    if (is.null(data1())) {
      tryCatch({
        result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> You need to upload Datamall data, if not what do you wanna see?!</span>"))
        stop()
      }, error = function(e) {
        print("data1 not defined.")
        stop()
      })
    }
    # Check that data2 exists before proceeding.
    if (is.null(data2())) {
      tryCatch({
        result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> You need to upload BusRouter Services data, if not what do you wanna see?!</span>"))
        stop()
      }, error = function(e) {
        print("data2 not defined.")
        stop()
      })
    }
    # Check that data3 exists before proceeding.
    if (is.null(data3())) {
      tryCatch({
        result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> You need to upload BusRouter Stops data, if not what do you wanna see?!</span>"))
        stop()
      }, error = function(e) {
        print("data3 not defined.")
        stop()
      })
    }
    svc2 <- as.character(svc())
    dir2 <- as.numeric(dir())
    data2_2 <- data2()
    data3_2 <- data3()
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
      filter_time_period <- quo(TIME_PER_HOUR %in% valid_hours)
    } else {
      filter_time_period <- quo(TRUE)
      time_period <- "Full Day"
    }
    if (identical(spec_stops(), F)) {
      stop_half_opt = svc_half()
      stop_cur <- data2_2[[svc2]]$routes[[dir2]]
      if (is.null(stop_cur)) {
        tryCatch({
          result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Invalid bus service. Is your bus service withdrawn?</span>"))
          stop("Invalid bus service.")
        }, error = function(e) {
          stop(e$message)
        })
      }
      terminus <- data3_2[[(stop_cur[length(stop_cur)])]][[3]]
      is_2way <- length(data2_2[[svc2]]$routes)
      stop_half <- round(length(stop_cur)/2)
      if (is_2way == 2){
        if (identical(stop_half_opt, "1st half")){
          stop_cur2 <- stop_cur[1:(stop_half+3)]
          dir_graph <- paste0("Direction ",dir2,"\n(",stop_half_opt,")")
        } else if (identical(stop_half_opt, "2nd half")) {
          stop_cur2 <- stop_cur[(stop_half-3):length(stop_cur)]
          dir_graph <- paste0("Direction ",dir2,"\n(",stop_half_opt,")")
        } else {
          stop_cur2 <- stop_cur
          dir_graph <- paste("Direction ",dir2)
        } 
      } else {
        if (identical(stop_half_opt, "1st half")){
          stop_cur2 <- stop_cur[1:(3+stop_half)]
          dir_graph <- paste0("Direction 1\n(",stop_half_opt,")")
        } else if (identical(stop_half_opt, "2nd half")) {
          stop_cur2 <- stop_cur[(stop_half-2):length(stop_cur)]
          dir_graph <- paste0("Direction 1\n(",stop_half_opt,")")
        } else {
          stop_cur2 <- stop_cur
          dir_graph <- "Loop Svc"
        }}
      V <- length(stop_cur2)
      stop_names <- data.frame(as.character(c(stop_cur2)),c(1:V))
      for (j in 1:V){
        stop_names[j,2] <- data3_2[[(stop_cur2[j])]][[3]]
      }
      stop_cur0a <- data.frame(org = 1:V, ORIGIN_PT_CODE = stop_cur2)
      stop_cur0b <- data.frame(dst = 1:V, DESTINATION_PT_CODE = stop_cur2)
      dataod <- data1() %>%
      filter(., ORIGIN_PT_CODE %in% stop_cur2 | is.na(ORIGIN_PT_CODE),
            DESTINATION_PT_CODE %in% stop_cur2 | is.na(DESTINATION_PT_CODE),
            !!filter_day_type, !!filter_time_period) %>%  
      group_by(ORIGIN_PT_CODE, DESTINATION_PT_CODE) %>%
      summarise(total = sum(TOTAL_TRIPS, na.rm = TRUE), .groups = "drop")
      missing_org <- setdiff(stop_cur2, unique(dataod$ORIGIN_PT_CODE)) # Check for missing origin stops
      missing_dst <- setdiff(stop_cur2, unique(dataod$DESTINATION_PT_CODE)) # Check for missing destination stops
      if (length(missing_org) > 0) {
        extra_rows <- tibble(ORIGIN_PT_CODE = unique(missing_org), DESTINATION_PT_CODE = NA, total = 0)  
        dataod <- bind_rows(dataod, extra_rows) %>%
          distinct(ORIGIN_PT_CODE, DESTINATION_PT_CODE, .keep_all = TRUE)  # Ensure uniqueness
      }
      if (length(missing_dst) > 0) {
        extra_rows <- tibble(ORIGIN_PT_CODE = missing_dst, DESTINATION_PT_CODE = missing_dst, total = 0)  # Ensure uniqueness
        dataod <- bind_rows(dataod, extra_rows) %>%
          filter(DESTINATION_PT_CODE %in% stop_cur2)  # Ensure proper alignment
      }
      dataod0a <- full_join(dataod, stop_cur0a, by="ORIGIN_PT_CODE")
      dataod0b <- full_join(dataod0a, stop_cur0b, by="DESTINATION_PT_CODE")
      dataod2 <- dataod0b[order(dataod0b$org, dataod0b$dst), ]
      dataod2$total <- as.numeric(dataod2$total)
      dataod2a <- dataod2[,c(5,4,3)]
      dataod2b <- dataod2[,c(2,1,3)]
      dataod2c <- dataod2a %>%
        spread(key = org, value = total, fill = NA)
      extra_row <- setdiff(unique(dataod2c$dst), stop_cur2)  # Identify the unwanted row
      dataod2c[is.na(dataod2c)] <- 0
      dataod2c <- dataod2c %>%
        filter(dst != 0)  # Keeps all valid rows, removes the incorrect one
      stop_cur3a <- stop_cur2[1:V-1]
      stop_cur3b <- stop_cur2[2:V]
      dataod2e <- as.matrix(dataod2c)
      dataod2e <- dataod2e[,-1]
      if (ncol(dataod2e)!=(V-1)){
        dataod2e <- dataod2e[,-ncol(dataod2e)]
      }
      if (nrow(dataod2e)!=(V-1)){
        dataod2e <- dataod2e[-1,]
      }
      dataod2g <- dataod2e
      dataod2h <- dataod2g
      rownames(dataod2g) <- paste(2:V)
      colnames(dataod2h) <- paste(stop_cur3a)
      rownames(dataod2h) <- paste(stop_cur3b)
      odgroup_crowd <- rep("Origin stop",V-1)
      odgroup1 <- odgroup_crowd
      cols = colorRamp2(c(0, 1, 30, 300, 1500, 6000, 30000, 99000), c("gray60","white","white", "yellow", "orange", "red", "darkred","black"))
      max_length <- 2 * max(nchar(stop_names[,2]))
      column_labels <- if ("column_names" %in% input$stop_names || display_stop_names()$columns == TRUE) {
        paste(stop_names[1:nrow(stop_names)-1,1], "     ", stop_names[1:nrow(stop_names)-1,2], sep = "")
      } else {
        paste(stop_names[1:nrow(stop_names)-1,1])
      }
      row_labels <- if ("row_names" %in% input$stop_names || display_stop_names()$rows == TRUE) {
        paste(stop_names[2:nrow(stop_names),2], "     ", stop_names[2:nrow(stop_names),1], sep = "")
      } else {
        paste(stop_names[2:nrow(stop_names),1])
      }
      img_dims <- list(width = 39 * ncol(dataod2h) + 300, height = 22 * nrow(dataod2h) + 200)
      img <- Heatmap(dataod2h,
        name = paste("O-D matrix (en-route)\n",data1()$YEAR_MONTH[[1]], "\n",day_type," Demand\n\nService ",svc2,"\n",dir_graph,"\n",terminus," Bound\n",sep = ""),
        show_column_dend = FALSE,
        show_row_dend = FALSE,
        row_dend_reorder = FALSE,
        column_dend_reorder = FALSE,
        column_title = paste0(time_period, "\nOrigin Bus Stops"),
        column_title_side = "top",
        row_title = "Destination Bus Stops",
        row_names_side = "left",
        column_names_side = "top",
        column_names_rot = 40,
        column_labels = column_labels,
        row_labels = row_labels,
        col = cols,
        na_col = "gray60",
        column_split = odgroup1,
        column_gap = unit(2, "mm"),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_order = stop_cur3b,
        column_order = stop_cur3a, 
        row_names_gp = gpar(fontsize = pmin(ncol(dataod2h) / 3 + 9, 15), just = "right"),
        column_names_gp = gpar(fontsize = pmin(ncol(dataod2h) / 3 + 9, 15)),
        row_title_gp = gpar(fontsize = pmin(ncol(dataod2h) / 3 + 12, 25), just = "left"),
        column_title_gp = gpar(fontsize = pmin(ncol(dataod2h) / 3 + 12, 25)),
        row_names_max_width = unit(max_length, "cm"),
        column_names_max_height = unit(max_length, "cm"),
        heatmap_legend_param = list(labels_gp = gpar(fontsize = pmin(ncol(dataod2h) / 3 + 9, 18)), legend_height = unit(pmin(nrow(dataod2h) / 4 + 1, 8), "cm"), at = c(0, 300, 1500, 6000, 30000, 99000), legend_width = unit(2, "cm"), color_bar = "continuous", title_gp = gpar(fontsize = pmin(ncol(dataod2h) / 3 + 9, 18), fontface = 'bold'), break_dist = 1),
        cell_fun = function(j, i, x, y, width, height, fill) {
          if(dataod2h[i, j] > 10000){
            grid.text(sprintf("%.0f", dataod2h[i, j]), x, y, gp = gpar(fontsize = 13, col = "white"))
          } else if (dataod2h[i, j] > 5000) {
            grid.text(sprintf("%.0f", dataod2h[i, j]), x, y, gp = gpar(fontsize = 14, col = "white"))
          } else if(dataod2h[i, j] > 29){
            grid.text(sprintf("%.0f", dataod2h[i, j]), x, y, gp = gpar(fontsize = 14))
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
          result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> The lengths of your origin stops and destination stops do not match.</span>"))
          stop("Length of origin stops not equal to length of destination stops.")
        }, error = function(e) {
          stop(e$message)
        })
      }
      dataod3 <- data.frame(c(1:l_ori), c(1:l_ori), c(1:l_ori))
      dataod3a <- data.frame(c(1:l_ori), c(1:l_ori))
      colnames(dataod3) <- c("ori", "dst", "dmd")
      colnames(dataod3a) <- c("ori_name", "dst_name")
      for (t in 1:l_ori) {
        ori_stop <- as.numeric(ori_stops[[1]][[t]])
        dst_stop <- as.numeric(dst_stops[[1]][[t]])
        dataod3[t, 1] <- ori_stop
        dataod3[t, 2] <- dst_stop
        valid_stops <- nrow(filter(data1(),
          ORIGIN_PT_CODE %in% dataod3[t, 1] | is.na(ORIGIN_PT_CODE),
          DESTINATION_PT_CODE %in% dataod3[t, 2] | is.na(DESTINATION_PT_CODE),
          !!filter_day_type, !!filter_time_period))
        if (valid_stops == 0) {
          tryCatch({
            result_msg(paste0("<span style='color:#BB0000; font-weight:bold;'><i class='fas fa-triangle-exclamation'></i> Invalid bus stop code(s) detected! Check your codes to see if it's a proper O-D pair, or there's absolutely no one going from A to B.</span>"))
            stop("Invalid stop codes or no demand.")
          }, error = function(e) {
            stop(e$message)
          })
        }
        dataod3[t, 3] <- data1() %>%
          filter(., ORIGIN_PT_CODE == dataod3[t, 1],
            DESTINATION_PT_CODE == dataod3[t, 2],
            !!filter_day_type,
            !!filter_time_period) %>%
          summarise(Total = sum(TOTAL_TRIPS)) %>%
          pull(Total) %>%
          as.numeric()
        if ("row_names" %in% input$stop_names || display_stop_names()$cells == TRUE) {
          dataod3a[t, 1] <- data3_2[[as.character(ori_stop)]][[3]]
          dataod3a[t, 2] <- data3_2[[as.character(dst_stop)]][[3]]
         }
      }
      dataod3 <- as.matrix(dataod3)
      dataod3a <- as.matrix(dataod3a)
      cols = colorRamp2(c(0, 1, 30, 300, 1500, 6000, 30000, 99000), c("gray60","white","white", "yellow", "orange", "red", "darkred","black"))
      img_dims <- list(width = 520, height = 24 * nrow(dataod3) + 90)
      img <- Heatmap(dataod3,
        name = paste(day_type, "Demand,", time_period),
        show_column_dend = FALSE,
        show_row_dend = FALSE,
        row_dend_reorder = FALSE,
        column_dend_reorder = FALSE,
        column_title = "O-D Matrix for specific stops",
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
            if (input$stop_names2 || display_stop_names()$cells == TRUE) {
              grid.text(sprintf("%s\n%s", dataod3[i, j], dataod3a[i, j]), x, y, gp = gpar(fontsize = 11, col = "black"))
            } else {
              grid.text(sprintf("%s", dataod3[i, j]), x, y, gp = gpar(fontsize = 15, col = "black"))
            }
          }
          if (j ==3) {
            if(dataod3[i, j] > 5000){
              grid.text(sprintf("%.0f", dataod3[i, j]), x, y, gp = gpar(fontsize = 15, col = "white"))
            }
            else if(dataod3[i, j] >= 0){
              grid.text(sprintf("%.0f", dataod3[i, j]), x, y, gp = gpar(fontsize = 15))
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
    
    # Draw the heatmap
    if (identical(spec_stops(), FALSE)) {
      req(result())
      draw(result()$img)
      result_msg("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> Heatmap successfully drawn!</span>")
    } else {
      req(result())
      draw(result()$img, heatmap_legend_side = "top")
      result_msg("<span style='color:#00DD00; font-weight:bold;'><i class='fas fa-square-check'></i> Heatmap successfully drawn!</span>")
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