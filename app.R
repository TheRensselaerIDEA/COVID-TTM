library(shiny)
library(visNetwork)
library(tidyverse)
library(ggplot2)
library(useful)
library(assertthat)
library(lubridate)
library(RJSONIO)
library(parallel)
# library(mwshiny)

# source("src/floor.R")
source("src/wall.R")
source("src/network.R")
# source("src/external-monitor.R")
source("src/utilities.R")
# source("src/campfire_lib.R")

ui <- fluidPage(
  tags$head(includeCSS("ttm.css")),
  tags$div(
    class = "page",
    fluidRow(
      column(4,
             uiOutput("wall_info")
             ),
      column(8,
             tags$h1(
               textAreaInput("text_input",
                             "JSON Text",
                             value = read_file("test/test_ALLGROUPS_new.json"),
                             height = '455px',
                             width = "550px")
             )
             )
    )
  )
)

server <- function(input, output, session) {
  
  row_list <- reactiveVal()
  text_input <- read_file("test/test_ALLGROUPS_new.json")
  
  # updateComplete()
  
  # observeEvent(delete_node, {
  #   # Update the data when a node is deleted.
  #   #
  #   # Event:
  #   #   Node is double clicked on the floor
  #   #ServerValues$nodes <- ServerValues$nodes[ServerValues$nodes$id != input$delete_node, ]
  #   # updateValues()
  #   # updateValues()
  #   withProgress(message = "Reloading...", value = 0, {#}, session = monitor.domain, {
  #     incProgress(0, detail = "Updating Column...")#, session = monitor.domain)
  #     deletedIndex <- which(!is.na(nodes$id) & nodes$id == delete_node)[1]
  #     nodes[deletedIndex, ]$hidden <- TRUE
  #     nodes[deletedIndex, ]$id <- NA
  #     nodes[deletedIndex, ]$name <- NA
  #     nodes[deletedIndex, ]$colname <- NA
  #     nodes[deletedIndex, ]$colvalue <- NA
  #     nodes[deletedIndex, ]$query.query.q <- NA
  #     nodes[deletedIndex, ]$query.query.colname <- NA
  #     nodes[deletedIndex, ]$query.name <- NA
  #     nodes[deletedIndex, ]$query.repr <- NA
  #     nodes[deletedIndex, ]$orig_indices <- NA
  #     row_list[[deletedIndex]] <- getEmptyColumn(deletedIndex)
  #     data_subset <- data
  #     network_selected <- ""
  #     incProgress(1, detail = "Finished...")#, session = monitor.domain)
  #     # updateJSON()
  #   })
  # })
  
  updateComplete <- reactive({
    # if(is.null(monitor.domain))
    # {
    #   d <- getDefaultReactiveDomain()
    # }
    # else
    # {
    #   d <- monitor.domain
    # }
    withProgress(message = "Reloading...", value = 0, {#session = d, {
      tryCatch({
        incProgress(0, detail = "Getting Data...")#, session = d)
        parsed_json <- fromJSON(text_input, nullValue = NA, simplify = FALSE)
        data <- fetchData(parsed_json$data_file)
        data_subset <- data
        url_map <- getUrlMap(data)
        edge_colnames <- parsed_json$edge_colnames
        incProgress(.2, detail = "Creating Nodes...")#, session = d)
        columnQueries <- lapply(parsed_json$nodes, function(query) {
          if (query != "") {
            parseColumnQuery(query)
          }
          else {
            createNodeQuery(NA, NA, NA, NA)
          }
        })
        nodes <- getNodes(data, columnQueries)
        incProgress(.2, detail = "Creating Edges...")#, session = d)
        edges <- getEdges(data, columnQueries, edge_colnames, nodes)
        incProgress(.2, detail = "Creating Network...")#, session = d)
        network <- getNetwork(nodes, edges)
        incProgress(.2, detail = "Creating Wall...")#, session = d)
        # row_list <- reactiveVal(envir = .GlobalEnv)
        row_list(updateWall(data, nodes))
        # browser()
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
  
  onInit <- observe({
    # row_list <- list()
    updateComplete()
    # browser()
    onInit$destroy()
  })

  output$wall_info <- renderUI({
    tags$h1(
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
        # fluidRow(
          lapply(1:12, function(col.num) {
            fluidRow(
              row_list()[[col.num]]
            )
          })
        # )
      )
    )
  })
  
  
  
}

shinyApp(ui, server)