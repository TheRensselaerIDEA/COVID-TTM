library(shiny)
library(shinyjs)
library(visNetwork)
library(tidyverse)
library(ggplot2)
library(useful)
library(assertthat)
library(lubridate)
library(RJSONIO)
library(parallel)
# library(mwshiny)

source("src/floor.R")
source("src/wall.R")
source("src/network.R")
# source("src/external-monitor.R")
source("src/utilities.R")
# source("src/campfire_lib.R")

ui <- fluidPage(
  tags$head(includeCSS("ttm.css")),
  tags$div(
    class = "page node_bar",
    fluidRow(
      column(5,
             tags$div(
               column(7,
                      uiOutput("node_bar")
               ),
               column(5,
                      uiOutput("tweet_bar")
                      # textOutput('click2')
               )
             )
            ),
      column(7,
             tags$div(
               class = "right_side",
               visNetworkOutput("network", height = '95vh', width = '100%')
                                  
                 # textAreaInput("text_input",
                 #               "JSON Text",
                 #               value = read_file("test/test_ALLGROUPS_new.json"),
                 #               height = '455px',
                 #               width = "550px")
               )
            )
    )
  )
)

nodeCorrector <- function(nodes, num) {
  it = 0
  lapply(1:num, function(x) {
    if (!is.na(nodes[[7]][[x]])) {
      # browser()
      it <<- it + 1
    }
  })
  # browser()
  return(it)
}

selCorrector <- function(nodes, num) {
  it = 0
  lapply(1:12, function(x) {
    if (is.na(nodes[[7]][[x]])  & ((x - it) <= num)) {
      # browser()
      it <<- it + 1
    }
  })
  # browser()
  return(it + num)
}

server <- function(input, output, session) {
  
  row_list <- reactiveVal()
  text_input <- read_file("test/test_ALLGROUPS_new.json")
  node_shown <- reactiveVal(2)
  
  

  network <- reactiveVal()
  edges <- reactiveVal()
  nodes <- reactiveVal()
  parsed_json <- reactiveVal()
  

  updateComplete <- reactive({
    # if(is.null(monitor.domain))
    # {
    #   d <- getDefaultReactiveDomain()
    # }
    # else
    # {a
    #   d <- monitor.domain
    # }
    withProgress(message = "Reloading...", value = 0, {#session = d, {
      tryCatch({
        incProgress(0, detail = "Getting Data...")#, session = d)
        parsed_json(fromJSON(text_input, nullValue = NA, simplify = FALSE))
        data <- fetchData(parsed_json()$data_file)
        data_subset <- data
        url_map <- getUrlMap(data)
        edge_colnames <- parsed_json()$edge_colnames
        incProgress(.2, detail = "Creating Nodes...")#, session = d)
        columnQueries <- lapply(parsed_json()$nodes, function(query) {
          if (query != "") {
            parseColumnQuery(query)
          }
          else {
            createNodeQuery(NA, NA, NA, NA)
          }
        })
        nodes(getNodes(data, columnQueries))
        incProgress(.2, detail = "Creating Edges...")#, session = d)
        edges(getEdges(data, columnQueries, edge_colnames, nodes()))
        incProgress(.2, detail = "Creating Network...")#, session = d)
        network(getNetwork(nodes(), edges()))
        incProgress(.2, detail = "Creating Wall...")#, session = d)
        # row_list <- reactiveVal(envir = .GlobalEnv)
        row_list(updateWall(data, nodes()))
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
  
  
  # This is the button list of available nodes so you can pull up
  # that node's stream
  output$node_bar <- renderUI({
    tags$div(
      # class = "node_bar",
      tags$div(
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node1",
                         paste0("Node 1: ",
                                ifelse(!is.na(nodes()[[7]][[1]]), nodes()[[7]][[1]], "NA")
                                ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node2",
                         paste0("Node 2: ",
                                ifelse(!is.na(nodes()[[7]][[2]]), nodes()[[7]][[2]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node3",
                         paste0("Node 3: ",
                                ifelse(!is.na(nodes()[[7]][[3]]), nodes()[[7]][[3]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node4",
                         paste0("Node 4: ",
                                ifelse(!is.na(nodes()[[7]][[4]]), nodes()[[7]][[4]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node5",
                         paste0("Node 5: ",
                                ifelse(!is.na(nodes()[[7]][[5]]), nodes()[[7]][[5]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node6",
                         paste0("Node 6: ",
                                ifelse(!is.na(nodes()[[7]][[6]]), nodes()[[7]][[6]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node7",
                         paste0("Node 7: ",
                                ifelse(!is.na(nodes()[[7]][[7]]), nodes()[[7]][[7]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node8",
                         paste0("Node 8: ",
                                ifelse(!is.na(nodes()[[7]][[8]]), nodes()[[7]][[8]], "NA")
                         ),
                         class="node_box")
            # tags$div(

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node9",
                         paste0("Node 9: ",
                                ifelse(!is.na(nodes()[[7]][[9]]), nodes()[[7]][[9]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node10",
                         paste0("Node 10: ",
                                ifelse(!is.na(nodes()[[7]][[10]]), nodes()[[7]][[10]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node11",
                         paste0("Node 11: ",
                                ifelse(!is.na(nodes()[[7]][[11]]), nodes()[[7]][[11]], "NA")
                         ),
                         class="node_box")

          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node12",
                         paste0("Node 12: ",
                                ifelse(!is.na(nodes()[[7]][[12]]), nodes()[[7]][[12]], "NA")
                         ),
                         class="node_box")
          )
        )
      )
    )
  })
  
  # Shows list of tweets from that node
  output$tweet_bar <- renderUI({
    # if (node_shown() == 0) {
    #   tags$div(
    #     # class = "node_bar",
    #     tags$div(
    #       class = "node_box"
    #     )
    #   )
    # }
    if (node_shown() != 0) {
      # browser()
      tags$div(
        row_list()[[node_shown()]]
      )
    }
  })
  

  
  # render the floor
  output$network <- renderVisNetwork({
    updateComplete()
    if (!is.null(network())) {
      network() %>%
        visEvents(type = "once", beforeDrawing = "function() {
              this.moveTo({
                            position: {
                              x: 0,
                              y: 0
                            },
                      scale: .65
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
        ) %>%
        visEvents(
          selectNode = "function(node) {
                                  Shiny.onInputChange('click', node.nodes[0]);
                               }"
        )
    }
  }
  )
  
  proxy <- visNetworkProxy("network")
  
  observeEvent(input$click, {
    # browser()
    node_shown(selCorrector(nodes(), as.numeric(input$click)))
  })
  
  
  observeEvent(input$node1, {
    # browser()
    node_shown(1)
    if (parsed_json()$nodes$node01 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),1))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node2, {
    # browser()
    node_shown(2)
    if (parsed_json()$nodes$node02 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),2))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node3, {
    # browser()
    node_shown(3)
    if (parsed_json()$nodes$node03 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),3))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node4, {
    # browser()
    node_shown(4)
    if (parsed_json()$nodes$node04 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),4))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node5, {
    # browser()
    node_shown(5)
    if (parsed_json()$nodes$node05 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),5))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node6, {
    # browser()
    node_shown(6)
    if (parsed_json()$nodes$node06 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),6))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node7, {
    # browser()
    node_shown(7)
    if (parsed_json()$nodes$node07 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),7))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node8, {
    # browser()
    node_shown(8)
    if (parsed_json()$nodes$node08 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),8))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node9, {
    # browser()
    node_shown(9)
    if (parsed_json()$nodes$node09 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),9))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node10, {
    # browser()
    node_shown(10)
    if (parsed_json()$nodes$node10 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),10))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node11, {
    # browser()
    node_shown(11)
    if (parsed_json()$nodes$node11 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),11))
    else {
      visUnselectAll(proxy)
    }
  })
  
  observeEvent(input$node12, {
    # browser()
    node_shown(12)
    if (parsed_json()$nodes$node12 != "")
      visSelectNodes(proxy, nodeCorrector(nodes(),12))
    else {
      visUnselectAll(proxy)
    }
  })
    
  # output$click2 <- renderText({
  #   req(input$click)
  #   selCorrector(nodes, as.numeric(input$click))
  # })
    


  
}

shinyApp(ui, server)