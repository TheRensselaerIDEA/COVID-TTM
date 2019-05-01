# MW Shiny ----------------------------------------------------------------

campfireApp = function(controller = NA, wall = NA, floor = NA, datamonitor = NA,
                       urlmonitor = NA, serverFunct = NA) 
  {
  
  ui <- campfireUI(controller, wall, floor, datamonitor, urlmonitor)
  
  # MW Shiny central reactive values. initialized makes sure default search is done on startup.
  ServerValues<- reactiveValues(initialized = FALSE, network_selected = "", network_selected_e = "")
                                 # data_subset = NULL,
                                 # type = "none")
  
  campfire_server <- shinyServer(function(input, output, session) 
  {
    
    #' Updates the central serverValues with the input values from a SPECIFIC window.
    #' Whatever domain calls this will update its input values with serverValues.
    updateValues <- reactive(
    {
      for(inputId in names(input))
      {
        ServerValues[[inputId]] <- input[[inputId]]
      }
    })
    
    #' Gets tweet data and updates entire wall and entire floor with updated data.
    #' Load bar will default to the specific window domain this is called in. 
    #' We keep track of the monitor domain so the load bar will have priority there if it is open.
    #' TODO: Better implementation of loadbar
    updateComplete <- reactive(
    {
      if(is.null(ServerValues$monitor.domain)) 
      {
        d <- getDefaultReactiveDomain()
      } 
      else
      {
        d <- ServerValues$monitor.domain
      }
      withProgress(message = "Reloading...", value = 0, session = d, {
        if(is.null(ServerValues$json_file))
        {
          ServerValues$json_file <- data.frame(datapath = "test/test_ALLGROUPS.json", stringsAsFactors = FALSE)
          fp <- ServerValues$json_file$datapath
          tryCatch({
            incProgress(0, detail = "Getting Data...", session = d)
            parsed_json <- fromJSON(fp, nullValue = NA, simplify = FALSE)
            ServerValues$data <- fetchData(parsed_json$data_file)
            ServerValues$edge_colnames <- parsed_json$edge_colnames
            incProgress(.2, detail = "Creating Nodes...", session = d)
            ServerValues$nodes <- getNodes(ServerValues$data, parsed_json$nodes)
            incProgress(.2, detail = "Creating Edges...", session = d)
            ServerValues$edges <- getEdges(ServerValues$data, parsed_json$nodes, ServerValues$edge_colnames, ServerValues$nodes)
            print("We got the graph")
            incProgress(.2, detail = "Creating Network...", session = d)
            ServerValues$network <- getNetwork(ServerValues$nodes, ServerValues$edges)
            incProgress(.2, detail = "Creating Wall...", session = d)
            ServerValues$col_list <- updateWall(ServerValues$data, ServerValues$nodes)
            incProgress(.2, detail = "Finished", session = d)
          },
          error=function(err) {
            print(paste0("Error loading JSON at ", fp))
            print(err)
          },
          warning=function(warning) {
            print(paste0("Warning loading JSON at ", fp))
            print(warning)
          }
        )
      }
    })
    })  
    
    #' Use the text box input from the controller to replace the current query and update.
    updateFromController <- reactive({
      updateValues()
      # ServerValues$queries <- StringQueryToVector(serverValues$queries_string)
      updateComplete()
    })
    
    #' If we are starting for the first time, update everything.
    #' serverValues$initialized is used to determine if first time bootup. 
    isolate({
      if(!ServerValues$initialized) {
        updateFromController()
        ServerValues$initialized <- TRUE
      }
    })
    
    observeEvent(input$update, {
      # Update the application from the controller text box queries when the update button is pressed.
      #
      # Event:
      #   Controller update button is pressed
      updateFromController()
    })
    
    # Observe when a node is clicked.
    observeEvent(input$network_selected, {
      updateValues()
      if(ServerValues$network_selected != "")
      {
        # Update data_subset
        node_info <- ServerValues$nodes[ServerValues$nodes$id == ServerValues$network_selected, ]
        node_info <- node_info[!is.na(node_info$id), ]
        ServerValues$data_subset <- getSubset(ServerValues$data, list(q = node_info$id, colname = node_info$colname))
      }
      else
      {
        ServerValues$data_subset <- NULL
      }
    })
    
    # Observe when an edge is clicked.
    observeEvent(input$network_selected_e, {
      updateValues()
      if(ServerValues$network_selected_e != "")
      {
        edge_info <- ServerValues$edges[ServerValues$network_selected_e, ]
        to_info <- ServerValues$nodes[ServerValues$nodes$id == edge_info$to, ]
        to_info <- to_info[!is.na(to_info$id), ]
        to_query <- list(q = edge_info$to, colname = to_info$colname)
        from_info <- ServerValues$nodes[ServerValues$nodes$id == edge_info$from, ]
        from_info <- from_info[!is.na(from_info$id), ]
        from_query <- list(q = edge_info$from, colname = from_info$colname)
        ServerValues$data_subset <- getEdgeSubset(ServerValues$data,
                                                  to_query,
                                                  from_query,
                                                  edge_info$colname,
                                                  ServerValues$nodes[!is.na(ServerValues$nodes$id), ])
      }
      else
      {
        ServerValues$data_subset <- NULL
      }
    })
  
    # Observe when a node is chosen to be deleted after a doubleclick, the
    # remove the data associated
    observeEvent(input$delete_node, {
      # Update the data when a node is deleted.
      #
      # Event:
      #   Node is double clicked on the floor
      #ServerValues$nodes <- ServerValues$nodes[ServerValues$nodes$id != input$delete_node, ]
      deletedIndex <- which(!is.na(ServerValues$nodes$id) & ServerValues$nodes$id == input$delete_node)[1]
      ServerValues$nodes[deletedIndex]$hidden <- TRUE
      ServerValues$col_list[[deletedIndex]] <- getEmptyColumn(deletedIndex)
      updateValues()
      #serverValues$data_subset <- NULL
      # serverValues$col_list <- UpdateWall(serverValues$data, serverValues$queries)
    })

    # Observe when text on the wall is clicked, and update query and wall/floor
    observeEvent(input$clicked_text, {
      # Determine what was clicked on the wall and update the appropriate values.
      #
      # Event:
      #   Text is clicked on the wall.
      updateValues()
      if(substr(ServerValues$clicked_text, 1, 1) == "#" ||  substr(ServerValues$clicked_text, 1, 1) == "@") {
        # if(toupper(ServerValues$clicked_text) %in% toupper(ServerValues$queries)) {
        #   index <- which(toupper(ServerValues$queries) %in% toupper(ServerValues$clicked_text))
        #   text <- ServerValues$queries[index]
        #   serverValues$current_node_id <- text
        #   serverValues$data_subset <- getDataSubset(ServerValues$data, text)
        #   serverValues$current_node_id = -1
        #   serverValues$current_edge_index = -1
        # } else {
        #   index <- which(is.na(ServerValues$queries))[1]
        #   if(!is.na(index)) {
        #     serverValues$queries[[index]] <- ServerValues$clicked_text
        #     updateComplete()
        #   }
        # }
      } else {
        ServerValues$url <- input$clicked_text
      }
    })
    # 
    # observeEvent(input$end_position, {
    #   # Change column positions based on moved nodes.
    #   #
    #   # Event:
    #   #   Node is released from drag on the foor.
    #   updateValues()
    #   angle <- cart2pol(serverValues$end_position[[1]]$x, -serverValues$end_position[[1]]$y)$theta
    #   angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
    #   angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2], 2*pi)
    #   # Find the closest angle value to the newly calculated
    #   to_index <- which(abs(angles - angle) == min(abs(angles - angle)))
    #   # If angle is close to 2pi, set the index to 10
    #   if(to_index == 13) {
    #     to_index <- 10
    #   }
    #   # Store old values to move the old node
    #   to_node <- serverValues$queries[[to_index]]
    #   from_index <- which(serverValues$queries %in% serverValues$current_node_id)
    #   to_col <- serverValues$col_list[[to_index]]
    #   start_distance <- ((serverValues$start_position[[1]]$x)^2 + (serverValues$start_position[[1]]$y)^2)^.5
    #   end_distance <- ((serverValues$end_position[[1]]$x)^2 + (serverValues$end_position[[1]]$y)^2)^.5
    #   if(start_distance < 187 && end_distance >= 187) {
    #     # When we move a node from the center to an edge
    #     visNetworkProxy("network") %>%
    #       visMoveNode(to_node, 0, 0)
    #     serverValues$queries[[to_index]] <- serverValues$current_node_id
    #     serverValues$queries[[from_index]] <- to_node
    #     serverValues$col_list[[to_index]] <- UpdateColumn(
    #       getDataSubset(serverValues$data, serverValues$current_node_id),
    #       serverValues$queries,
    #       to_index)
    # 
    #   } else if(start_distance >= 187 && end_distance < 187) {
    #     # When we move a node from the edge to the center
    #     serverValues$queries <- c(serverValues$queries, serverValues$current_node_id)
    #     serverValues$queries[[from_index]] <- NA
    #     serverValues$col_list[[from_index]] <- column(width = 1,
    #                                                  textInput(paste0("text.column.", from_index), label = ""),
    #                                                  actionButton(paste0("button.column.", from_index), "Submit"))
    # 
    # 
    # 
    #   # Normal movement, when both nodes are on the edge
    #   } else if(start_distance >= 187 && end_distance >= 187) {
    #     # Change the position of the node moved onto
    #     if(from_index != new_index) {
    #       visNetworkProxy("network") %>%
    #         visMoveNode(to_node, serverValues$start_position[[1]]$x, serverValues$start_position[[1]]$y)
    #       serverValues$queries[to_index] <- serverValues$current_node_id
    #       serverValues$queries[from_index] <- to_node
    #       serverValues$col_list[[to_index]] <- serverValues$col_list[[from_index]]
    #       serverValues$col_list[[from_index]] <- to_col
    #     }
    # 
    #   # Move two nodes in the center
    #   } else {
    #     # Do nothing
    #   }
      
      
    # })
    # Observe all wall buttons, then update query and wall/floor
    observeEvent({
      input$button.column.1
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.1, 1) 
      updateComplete()
    })
    observeEvent({
      input$button.column.2
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.2, 2) 
      updateComplete()
    })
    observeEvent({
      input$button.column.3
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.3, 3) 
      updateComplete()
    })
    observeEvent({
      input$button.column.4
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.4, 4)  
      updateComplete()
    })
    observeEvent({
      input$button.column.5
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.5, 5) 
      updateComplete()
    })
    observeEvent({
      input$button.column.6
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.6, 6) 
      #ServerValues$edges <- getEdges(ServerValues$data, ServerValues$nodes, ServerValues$edge_colnames)
      #ServerValues$network <- getNetwork(ServerValues$nodes, ServerValues$edges)
      updateComplete()
    })
    observeEvent({
      input$button.column.7
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.7, 7)
      updateComplete()
    })
    observeEvent({
      input$button.column.8
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.8, 8)
      updateComplete()
    })
    observeEvent({
      input$button.column.9
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.9, 9)
      updateComplete()
    })
    observeEvent({
      input$button.column.10
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.10, 10)
      updateComplete()
    })
    observeEvent({
      input$button.column.11
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.11, 11)
      updateComplete()
    })
    observeEvent({
      input$button.column.12
    }, {
      updateValues()
      ServerValues <- updateAll(ServerValues, input$text.column.12, 12)
      updateComplete()
    })
    
    # editing column labels
    observeEvent({
      input$button.label.column.1
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.1, 1)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.2
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.2, 2)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.3
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.3, 3)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.4
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.4, 4)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.5
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.5, 5)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.6
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.6, 6)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.7
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.7, 7)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.8
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.8, 8)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.9
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.9, 9)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.10
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.10, 10)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.11
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.11, 11)
      updateComplete()
    })
    
    observeEvent({
      input$button.label.column.12
    }, {
      updateValues()
      ServerValues <- editLabel(ServerValues, input$text.label.column.12, 12)
      updateComplete()
    })
    
    serverFunct(ServerValues, output, session)
    
  })
  shinyApp(ui, server = campfire_server
           #,options = list(port = 8000, shiny.autoreload = TRUE)
           )
}

campfireUI = function(controller, wall, floor, datamonitor, urlmonitor) {
  ui <- shinyUI(bootstrapPage(
    HTML('<script type="text/javascript">
         $(function() {
         $("div.Window").hide(); 
         var tokens = window.location.href.split("?");
         if (tokens.length > 1) {
         var shown_window = tokens[1];
         $("div."+shown_window).show();
         } else {
         $("div.WindowSelector").show();
         }
         });
         </script>'),
    div(class="WindowSelector Window",
        HTML('<h2><a href="?Controller">Controller</a></h2>'),
        HTML('<h2><a href="?Wall">Wall</a></h2>'),
        HTML('<h2><a href="?Floor">Floor</a></h2>'),
        HTML('<h2><a href="?Monitor">External Monitor</a></h2>'),
        HTML('<h2><a href="?URLMonitor">URL Monitor</a></h2>'),
        style = 'position: absolute; 
        top: 50%; left: 50%; 
        margin-right: -50%; 
        transform: translate(-50%, -50%)'
    ),
    div(class = "Controller Window",
        controller
    ),
    div(class = "Wall Window",
        wall 
    ),
    div(class = "Floor Window",
        floor
    ),
    div(class = "Monitor Window",
        datamonitor
    ),
    div(class = "URLMonitor Window",
        urlmonitor
    )
    
    ))
  
  return(ui)
}


updateAll <- function(serverValues, queryString, colNum) {
  newQuery <- parseColumnQuery(queryString)
  newNode <- getNode(serverValues$data, newQuery)
  serverValues$nodes[colNum, ] <- newNode
  serverValues$nodes <- updatePositions(serverValues$nodes)
  nextId <- max(serverValues$edges$id)+1
  for (i in 1:nrow(serverValues$nodes)) {
    if (!is.na(serverValues$nodes$id[i])) {
      if (serverValues$nodes$id[i] != newNode$id) {
        oldQuery <- createNodeQuery(q = serverValues$nodes$query.query.q[i], 
                                    colname = serverValues$nodes$query.query.colname[i], 
                                    name = serverValues$nodes$query.name[i])
        
        rounds <- seq(0, .5, length.out = length(serverValues$edge_colnames))
        edge_colors <- c("#c51f5d", "white", "#008080")
        for (i in 1:length(serverValues$edge_colnames)) {
          newEdge <- getEdge(data = serverValues$data, 
                             to_query = newQuery$query, 
                             from_query = oldQuery$query, 
                             colname = serverValues$edge_colnames[[i]], 
                             nodes = serverValues$nodes,
                             edge_color = edge_colors[[i]], 
                             edge_rounds = rounds[[i]],
                             edge_id = nextId)
          nextId <- nextId + 1
          serverValues$edges <- rbind(serverValues$edges, newEdge)
        }
      }
    }
  }
  serverValues$network <- getNetwork(serverValues$nodes, serverValues$edges)
  serverValues$col_list[[colNum]] <- getColumn(serverValues$data, newNode, colNum) 
  return(serverValues)
}

editLabel <- function(serverValues, newLabel, colNum) {
  serverValues$nodes[colNum, ]$name <- newLabel
  # TODO need more performant way to change the h2 in the column
  serverValues$col_list[[colNum]] <- getColumn(serverValues$data, serverValues$nodes[colNum, ], colNum)
  return(serverValues)
}