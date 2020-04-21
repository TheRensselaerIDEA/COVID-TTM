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

server <- function(input, output, session) {
  
  row_list <- reactiveVal()
  text_input <- read_file("test/test_ALLGROUPS_new.json")
  node_shown <- reactiveVal(2)
  
  

  network <- reactiveVal()
  edges <- reactiveVal()
  nodes <- reactiveVal()
  
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
            actionButton("node1", "Node 1",
                         class="node_box")
            # tags$div(
            #   id="node1",
            #   class = "node_box",
            #   tags$p(paste0("Node 1"))
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node2", "Node 2",class="node_box")
            # tags$div(
            #   id="node2",
            #   class = "node_box",
            #   tags$p("Node 2")
            #   
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node3", "Node 3",class = "node_box")
            # tags$div(
            #   id="node3",
            #   class = "node_box",
            #   tags$p("Node 3")
            #   
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node4", "Node 4",class = "node_box")
            # tags$div(
            #   id="node4",
            #   class = "node_box",
            #   tags$p("Node 4")
            #   
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node5", "Node 5",class = "node_box")
            # tags$div(
            #   id="node5",
            #   class = "node_box",
            #   tags$p("Node 5")
            #   
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node6", "Node 6",class = "node_box")
            # tags$div(
            #   id="node6",
            #   class = "node_box",
            #   tags$p("Node 6")
            #   
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node7", "Node 7",class = "node_box")
            # tags$div(
            #   id="node7",
            #   class = "node_box",
            #   tags$p("Node 7")
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node8", "Node 8",class = "node_box")
            # tags$div(
            #   id="node8",
            #   class = "node_box",
            #   tags$p("Node 8")
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node9", "Node 9",class = "node_box")
            # tags$div(
            #   id="node9",
            #   class = "node_box",
            #   tags$p("Node 9")
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node10", "Node 10",class = "node_box")
            # tags$div(
            #   id="node10",
            #   class = "node_box",
            #   tags$p("Node 10")
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node11", "Node 11",class = "node_box")
            # tags$div(
            #   id="node11",
            #   class = "node_box",
            #   tags$p("Node 11")
            # )
          )
        ),
        fluidRow(
          tags$div(
            class = "node_box_outer",
            actionButton("node12", "Node 12",class = "node_box")
            # tags$div(
            #   id="node12",
            #   class = "node_box",
            #   tags$p("Node 12")
            # )
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
  

  observeEvent(input$node1, {
    # browser()
    node_shown(1)
  })
  
  observeEvent(input$node2, {
    # browser()
    node_shown(2)
  })
  
  observeEvent(input$node3, {
    # browser()
    node_shown(3)
  })
  
  observeEvent(input$node4, {
    # browser()
    node_shown(4)
  })
  
  observeEvent(input$node5, {
    # browser()
    node_shown(5)
  })
  
  observeEvent(input$node6, {
    # browser()
    node_shown(6)
  })
  
  observeEvent(input$node7, {
    # browser()
    node_shown(7)
  })
  
  observeEvent(input$node8, {
    # browser()
    node_shown(8)
  })
  
  observeEvent(input$node9, {
    # browser()
    node_shown(9)
  })
  
  observeEvent(input$node10, {
    # browser()
    node_shown(10)
  })
  
  observeEvent(input$node11, {
    # browser()
    node_shown(11)
  })
  
  observeEvent(input$node12, {
    # browser()
    node_shown(12)
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
                      scale: .7
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

shinyApp(ui, server)