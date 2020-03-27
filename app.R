library(shiny)
library(visNetwork)
library(tidyverse)
library(ggplot2)
library(useful)
library(assertthat)
library(lubridate)
library(RJSONIO)
library(parallel)
library(mwshiny)

source("src/floor.R")
source("src/wall.R")
source("src/network.R")
source("src/external-monitor.R")
source("src/utilities.R")
source("src/campfire_lib.R")

ui_list <- list()

ui_list[["Controller"]] <- fluidPage(
  tags$div(
    style = ("position: absolute; 
    top: 50%; left: 50%; 
    margin-right: -50%; 
    transform: translate(-50%, -50%)"),
    tags$h1("Controller"),
    fileInput("json_file", "JSON Input", accept = c("application/json")),
    textAreaInput("text_input",
                  "JSON Text",
                  value = read_file("test/test_ALLGROUPS_new.json"),
                  height = '455px',
                  width = "550px"),
    selectInput("rdata_file",
                "RDATA File", 
                choices = list.files("/data/BLM/TTimeMachinePeriods"),
                selected = "period_5_5.df.Rdata"),
    actionButton(inputId = "update",
                 label = "Update"),
    downloadButton("download", "Download JSON"),
    actionButton(inputId = "destroy",
                 label = "DESTROY")
  )
)

ui_list[["Wall"]] <- fluidPage(
  
  tags$div(
    style = paste0("background: ", color.back, "; overflow: hidden;",
                 "height: 665px"),
      tags$script(HTML(
        "$(document).on('click', '.clickable', function (event) {
            var text =  $(this).text();
            if(event.ctrlKey) {
              Shiny.onInputChange('open_url', true)
            } else {
              Shiny.onInputChange('open_url', false)
            }
            Shiny.onInputChange('clicked_text', text);
          });"
      )),
      fluidRow(
        uiOutput("wall_info")
      )
    )
)

ui_list[["Floor"]] <- fluidPage(
  tags$div(
    style = paste0("position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: ", color.back,
                   "; height: 1000px; overflow: hidden"),
    visNetworkOutput("network", width = "1000px", height = "1000px")
  )
)


ui_list[["Data_Monitor"]] <- fluidPage(
  tags$div(
    style = paste0("background: ", color.back, ";
                   overflow: hidden;
                   height: 1080px"),
    fluidRow(
      column(12,
             uiOutput("tweets_info")
      )
    )),
  fluidRow(
    column(6,
           plotOutput("top.users.bar.extern", height = "920px")
    ),
    column(6,
           plotOutput("tweets.by.time", height = "920px")
    )
  )
)


ui_list[["URL_Monitor"]] <- fluidPage(
  tags$div(
    htmlOutput("frame")
  )
)

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################


serv_calc <- list()



serv_calc[[1]] <- function(calc, session) {
  # Update text box when JSON file changes.
  observeEvent(calc$parsed_json, {
    if(!is.null(calc$parsed_json))
    {
      calc$text <- toJSON(calc$parsed_json, pretty = TRUE)
      # calc$text_input <- text
      updateTextInput(session, "text_input", value = calc$text)
    }
  })
  
  calc$onInit <- observe({
    calc$network_selected = ""
    calc$network_selected_e = ""
    calc$onInit$destroy()
  })
}


serv_calc[[2]] <- function(calc, session) {
  #' Gets tweet data and updates entire wall and entire floor with updated data.
  #' Load bar will default to the specific window domain this is called in.
  #' We keep track of the monitor domain so the load bar will have priority there if it is open.
  #' TODO: Better implementation of loadbar
  calc$updateComplete <- reactive({
    # if(is.null(calc$monitor.domain))
    # {
    #   d <- getDefaultReactiveDomain()
    # }
    # else
    # {
    #   d <- calc$monitor.domain
    # }
    withProgress(message = "Reloading...", value = 0, {#session = d, {
      tryCatch({
        incProgress(0, detail = "Getting Data...")#, session = d)
        calc$parsed_json <- fromJSON(calc$text_input, nullValue = NA, simplify = FALSE)
        calc$data <- fetchData(calc$parsed_json$data_file)
        calc$data_subset <- calc$data
        calc$url_map <- getUrlMap(calc$data)
        calc$edge_colnames <- calc$parsed_json$edge_colnames
        incProgress(.2, detail = "Creating Nodes...")#, session = d)
        columnQueries <- lapply(calc$parsed_json$nodes, function(query) {
          if (query != "") {
            parseColumnQuery(query)
          }
          else {
            createNodeQuery(NA, NA, NA, NA)
          }
        })
        calc$nodes <- getNodes(calc$data, columnQueries)
        incProgress(.2, detail = "Creating Edges...")#, session = d)
        calc$edges <- getEdges(calc$data, columnQueries, calc$edge_colnames, calc$nodes)
        incProgress(.2, detail = "Creating Network...")#, session = d)
        calc$network <- getNetwork(calc$nodes, calc$edges)
        incProgress(.2, detail = "Creating Wall...")#, session = d)
        calc$col_list <- updateWall(calc$data, calc$nodes)
        incProgress(.2, detail = "Finished")#, session = d)
      })
      # error=function(err) {
      #   print("Error loading JSON")
      #   print(err)
      # },
      # warning=function(warning) {
      #   print("Warning loading JSON")
      #   print(warning)
      # })
    })
  })
}

serv_calc[[3]] <- function(calc, session) {
  #' Updates ServerValues$parsed_json when stuff is changed by the user
  calc$updateJSON <- reactive({
    calc$new <- list()
    calc$new$data_file <- calc$rdata_path
    # Hard code in edge_colnames, not something we change
    calc$new$edge_colnames <- list("hashtags", "user_mentions", "expanded_urls")
    calc$new$nodes <- lapply(1:12, function(x) {
      calc$tmp <- calc$nodes$query.repr[[x]]
      # if(is.na(calc$tmp)) {
      #   ""
      # } else {
        calc$tmp
      # }
    })
    names <- sapply(1:12, function(x) {
      paste0("node", str_pad(x, 2, pad = 0))
    })
    names(calc$new$nodes) <- names
    calc$parsed_json <- calc$new
  })
}

serv_calc[[4]] <- function(calc, session) {
  # Observe when user changes json file from controller
  observeEvent(calc$json_file, {
    # updateValues()
    # Always change parsed_json so that if we reload the same json the text box will update
    calc$parsed_json <- NULL
    # fp <- calc$json_file$datapath
    calc$parsed_json <- fromJSON(calc$json_file$datapath, nullValue = NA, simplify = FALSE)
  }, ignoreInit = TRUE)
}

serv_calc[[5]] <- function(calc, session) {
  observeEvent(calc$rdata_file, {
    # updateValues()
    calc$rdata_path <- paste0("/data/BLM/TTimeMachinePeriods/", calc$rdata_file)
    # calc$updateJSON()
  })
}

serv_calc[[6]] <- function(calc, session) {
  observeEvent(calc$destroy, {
    stopApp()
  })
}

serv_calc[[7]] <- function(calc, session) {
  observeEvent(calc$update, {
    # Update the application from the controller text box queries when the update button is pressed.
    #
    # Event:
    #   Controller update button is pressed
    calc$updateComplete()
  })
}

serv_calc[[8]] <- function(calc, session) {
  # Observe when a node is clicked.
  observeEvent(calc$network_selected, {
    # updateValues()
    if(calc$network_selected != "")
    {
      # Update data_subset
      node_info <- calc$nodes[calc$nodes$id == calc$network_selected, ]
      node_info <- node_info[!is.na(node_info$id), ]
      calc$data_subset <- getSubset(calc$data, list(q = node_info$id, colname = node_info$colname))
    }
    else
    {
      calc$data_subset <- calc$data
    }
  })
}

serv_calc[[9]] <- function(calc, session) {
  observeEvent(calc$delete_node, {
    # Update the data when a node is deleted.
    #
    # Event:
    #   Node is double clicked on the floor
    #ServerValues$nodes <- ServerValues$nodes[ServerValues$nodes$id != input$delete_node, ]
    # updateValues()
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#}, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      deletedIndex <- which(!is.na(calc$nodes$id) & calc$nodes$id == calc$delete_node)[1]
      calc$nodes[deletedIndex, ]$hidden <- TRUE
      calc$nodes[deletedIndex, ]$id <- NA
      calc$nodes[deletedIndex, ]$name <- NA
      calc$nodes[deletedIndex, ]$colname <- NA
      calc$nodes[deletedIndex, ]$colvalue <- NA
      calc$nodes[deletedIndex, ]$query.query.q <- NA
      calc$nodes[deletedIndex, ]$query.query.colname <- NA
      calc$nodes[deletedIndex, ]$query.name <- NA
      calc$nodes[deletedIndex, ]$query.repr <- NA
      calc$nodes[deletedIndex, ]$orig_indices <- NA
      calc$col_list[[deletedIndex]] <- getEmptyColumn(deletedIndex)
      calc$data_subset <- calc$data
      calc$network_selected <- ""
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
}

serv_calc[[10]] <- function(calc, session) {
  # Observe all wall buttons, then update query and wall/floor
  observeEvent({
    calc$button.column.1
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.1, 1)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.2
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.2, 2)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.3
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.3, 3)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.4
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.4, 4)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.5
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.5, 5)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.6
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.6, 6)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.7
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.7, 7)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.8
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.8, 8)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.9
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.9, 9)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.10
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.10, 10)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.11
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.11, 11)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
  observeEvent({
    calc$button.column.12
  }, {
    # updateValues()
    withProgress(message = "Reloading...", value = 0, {#, session = calc$monitor.domain, {
      incProgress(0, detail = "Updating Column...")#, session = calc$monitor.domain)
      # ServerValues <- updateAll(ServerValues, calc$text.column.12, 12)
      incProgress(1, detail = "Finished...")#, session = calc$monitor.domain)
      # calc$updateJSON()
    })
  })
}

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

serv_out <- list()

serv_out[["wall_info"]] <- function(calc, session) {
  renderUI({
    lapply(1:12, function(col.num) {
      calc$col_list[[col.num]]
    })
  })
}

serv_out[["download"]] <- function(calc, session) {
  downloadHandler(
    filename = "ttm_output.json",
    content = function(file) {
      write(calc$text_input, file)
    },
    contentType = "application/json"
  )
}

# render the floor
serv_out[["network"]] <- function(calc, session) {
  renderVisNetwork({
    if (!is.null(calc$network)) {
      calc$network %>%
        visEvents(type = "once", beforeDrawing = "function() {
                this.moveTo({
                              position: {
                                x: 0,
                                y: 0
                              },
                        scale: 1
                })
              }") %>%
        visEvents(
          doubleClick = "function() {
                                         if(this.getSelectedNodes().length == 1) {
                                           Shiny.onInputChange('delete_node', this.getSelectedNodes()[0]);
                                           this.deleteSelected();
                                           Shiny.onInputChange('current_node_id', -1);
                                           Shiny.onInputChange('current_edge_index', -1);
                                         }
                                       }"
        )
    }
  }
  )
}


serv_out[["tweets_info"]] <- function(calc, session) {
  renderUI(
  {
    if(calc$network_selected != "")
    {
      calc$node_info <- calc$nodes[calc$nodes$id == calc$network_selected, ]
      node_info <- calc$node_info[!is.na(node_info), ]
      calc$node_size <- calc$node_info$value
      calc$node_name <- calc$node_info$name
      tags$div(
        tags$h1(style = paste0("color:", color.blue), calc$node_name),
        tags$h2(style = paste0("color:", color.blue), paste("Size:", calc$node_size)))
    }
    else if(calc$network_selected_e != "")
    {
      calc$edge_data <- calc$edges[calc$network_selected_e, ]
      calc$to_node_name <- calc$nodes$name[calc$nodes$id == calc$edge_data$to]
      calc$to_node_name <- to_node_name[!is.na(calc$to_node_name)]
      calc$from_node_name <- calc$nodes$name[calc$nodes$id == calc$edge_data$from]
      calc$from_node_name <- calc$from_node_name[!is.na(alc$from_node_name)]
      calc$edge_name <- paste(calc$to_node_name, "AND", calc$from_node_name)
      calc$edge_size <- calc$edge_data$value
      tags$div(
        tags$h1(style = paste0("color:", color.blue), calc$edge_name),
        tags$h2(style = paste0("color:", color.blue), paste("Type:", calc$edge_data$colname)),
        tags$h2(style = paste0("color:", color.blue), paste("Size:", calc$edge_size)))
    }
    else
    {
      tags$div(
        tags$h1(style = paste0("color:", color.blue), "Twitter Time Machine"),
        tags$h2(style = paste0("color:", color.blue), paste("Total number of tweets:", nrow(calc$data))),
        tags$h2(style = paste0("color:", color.blue), paste("Data:", isolate(calc$parsed_json$data_file)))
      )
    }
  })
}

serv_out[["top.users.bar.extern"]] <- function(calc, session) {
  renderPlot({
    calc$monitor.domain <- getDefaultReactiveDomain()
    calc$data_subset %>%
      count(user_screen_name) %>%
      arrange(desc(n)) %>%
      slice(1:10) %>%
      ggplot(aes(reorder(user_screen_name, n), n)) +
      geom_col(fill = color.blue, color = color.blue) +
      coord_flip() +
      labs(x = "Screen Name", y = "Tweets", title = "Top 10 Users") +
      theme_dark() +
      theme(plot.background = element_rect(fill = color.back, color = NA),
            axis.text = element_text(size = 20, colour = color.white),
            text = element_text(size = 20, colour = color.blue))
  })
}

serv_out[["tweets.by.time"]] <- function(calc, session) {
  renderPlot({
    calc$data_subset %>%
      group_by(group) %>%
      mutate(date = date(as_datetime(created_at))) %>%
      count(date) %>%
      ggplot(aes(x = date, y = n, group = group)) +
      scale_color_brewer(palette = "Spectral") +
      geom_line(aes(color = factor(group)), size = 2) +
      labs(x = "Date", y = "Number of Tweets", title = "Tweets vs. Time", color = "Group") +
      theme_dark() +
      theme(plot.background = element_rect(fill = color.back, color = NA),
            axis.text = element_text(size = 20, colour = color.white),
            text = element_text(size = 20, colour = color.blue),
            legend.background = element_rect(fill = color.back))
  })
}

serv_out[["frame"]] <- function(calc, session) {
  renderUI({
    if(!is.null(calc$url)) {
      redirectScript <- paste0("window = window.open('", calc$url, "');")
      tags$script(HTML(redirectScript))
    } else {
      redirectScript <- paste0("window = window.open('", "https://docs.google.com/presentation/d/1A8A3tWGPeEGItIQnkTMMNSsm1lfcrVE73O22KJ7t--I/present", "');")
      tags$script(HTML(redirectScript))
    }
  })
}

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

mwsApp(ui_list, serv_calc, serv_out)
