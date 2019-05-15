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
        tryCatch({
          incProgress(0, detail = "Getting Data...", session = d)
          ServerValues$parsed_json <- fromJSON(ServerValues$text_input, nullValue = NA, simplify = FALSE)
          ServerValues$data <- fetchData(ServerValues$parsed_json$data_file)
          ServerValues$data_subset <- ServerValues$data
          ServerValues$url_map <- getUrlMap(ServerValues$data)
          ServerValues$edge_colnames <- ServerValues$parsed_json$edge_colnames
          incProgress(.2, detail = "Creating Nodes...", session = d)
          columnQueries <- lapply(ServerValues$parsed_json$nodes, function(query) {
            if (query != "") {
              parseColumnQuery(query)
            }
            else {
              createNodeQuery(NA, NA, NA, NA)
            }
          })
          ServerValues$nodes <- getNodes(ServerValues$data, columnQueries)
          incProgress(.2, detail = "Creating Edges...", session = d)
          ServerValues$edges <- getEdges(ServerValues$data, columnQueries, ServerValues$edge_colnames, ServerValues$nodes)
          incProgress(.2, detail = "Creating Network...", session = d)
          ServerValues$network <- getNetwork(ServerValues$nodes, ServerValues$edges)
          incProgress(.2, detail = "Creating Wall...", session = d)
          ServerValues$col_list <- updateWall(ServerValues$data, ServerValues$nodes)
          incProgress(.2, detail = "Finished", session = d)
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
    
    #' Use the text box input from the controller to replace the current query and update.
    updateFromController <- reactive({
      updateValues()
      # ServerValues$queries <- StringQueryToVector(serverValues$queries_string)
      updateComplete()
    })
    
    #' Updates ServerValues$parsed_json when stuff is changed by the user
    updateJSON <- reactive({
      new <- list()
      new$data_file <- ServerValues$rdata_path
      # Hard code in edge_colnames, not something we change
      new$edge_colnames <- list("hashtags", "user_mentions", "expanded_urls")
      new$nodes <- lapply(1:12, function(x) {
        tmp <- ServerValues$nodes$query.repr[[x]]
        if(is.na(tmp)) {
          ""
        } else {
          tmp
        }
      })
      names <- sapply(1:12, function(x) {
        paste0("node", str_pad(x, 2, pad = 0))
      })
      names(new$nodes) <- names
      ServerValues$parsed_json <- new
    })
    
    # Observe when user changes json file from controller
    observeEvent(input$json_file, {
      updateValues()
      # Always change parsed_json so that if we reload the same json the text box will update
      ServerValues$parsed_json <- NULL
      fp <- ServerValues$json_file$datapath
      ServerValues$parsed_json <- fromJSON(fp, nullValue = NA, simplify = FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$rdata_file, {
      updateValues()
      ServerValues$rdata_path <- paste0("/data/BLM/TTimeMachinePeriods/", ServerValues$rdata_file)
      updateJSON()
    })
    
    #' If we are starting for the first time, update everything.
    #' serverValues$initialized is used to determine if first time bootup. 
    isolate({
      if(!ServerValues$initialized) {
        updateFromController()
        ServerValues$initialized <- TRUE
      }
    })
    
    observeEvent(input$destroy, {
      stopApp()
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
        ServerValues$data_subset <- ServerValues$data
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
        ServerValues$data_subset <- ServerValues$data
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
      updateValues()
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        deletedIndex <- which(!is.na(ServerValues$nodes$id) & ServerValues$nodes$id == input$delete_node)[1]
        ServerValues$nodes[deletedIndex, ]$hidden <- TRUE
        ServerValues$nodes[deletedIndex, ]$id <- NA
        ServerValues$nodes[deletedIndex, ]$name <- NA
        ServerValues$nodes[deletedIndex, ]$colname <- NA
        ServerValues$nodes[deletedIndex, ]$colvalue <- NA
        ServerValues$nodes[deletedIndex, ]$query.query.q <- NA
        ServerValues$nodes[deletedIndex, ]$query.query.colname <- NA
        ServerValues$nodes[deletedIndex, ]$query.name <- NA
        ServerValues$nodes[deletedIndex, ]$query.repr <- NA
        ServerValues$nodes[deletedIndex, ]$orig_indices <- NA
        ServerValues$col_list[[deletedIndex]] <- getEmptyColumn(deletedIndex)
        ServerValues$data_subset <- ServerValues$data
        ServerValues$network_selected <- ""
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })

    # Observe when text on the wall is clicked, and update query and wall/floor
    observeEvent(input$clicked_text, {
      # Determine what was clicked on the wall and update the appropriate values.
      #
      # Event:
      #   Text is clicked on the wall.
      updateValues()
      if(substr(ServerValues$clicked_text, 1, 1) == "#" ||  substr(ServerValues$clicked_text, 1, 1) == "@") {
        openColumns <- which(is.na(ServerValues$nodes$id))
        if(length(openColumns) > 0) {
          colNum <- openColumns[1]
          queryString <- paste0("label:", ServerValues$clicked_text, " ", ServerValues$clicked_text)
          ServerValues <- updateAll(ServerValues, queryString, colNum)
        }
      } 
      else {
        if(!is.na(ServerValues$url_map[ServerValues$clicked_text])) {
          #ServerValues$url <- ServerValues$clicked_text
          if(ServerValues$open_url) {
            ServerValues$url <- ServerValues$clicked_text 
          } else {
            openColumns <- which(is.na(ServerValues$nodes$id))
            if(length(openColumns) > 0) {
              colNum <- openColumns[1]
              expandedUrl <- as.character(ServerValues$url_map[ServerValues$clicked_text])
              queryString <- paste0("label:", expandedUrl, " url:", expandedUrl)
              ServerValues <- updateAll(ServerValues, queryString, colNum)
            }
          }
        }  
      }
      updateJSON()
    })
    
    # Observe all wall buttons, then update query and wall/floor
    observeEvent({
      input$button.column.1
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.1, 1)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.2
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.2, 2)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.3
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.3, 3)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.4
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.4, 4)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.5
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.5, 5)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.6
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.6, 6)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.7
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.7, 7)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.8
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.8, 8)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.9
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.9, 9)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.10
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.10, 10)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.11
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.11, 11)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
    })
    observeEvent({
      input$button.column.12
    }, {
      updateValues()
      withProgress(message = "Reloading...", value = 0, session = ServerValues$monitor.domain, {
        incProgress(0, detail = "Updating Column...", session = ServerValues$monitor.domain)
        ServerValues <- updateAll(ServerValues, ServerValues$text.column.12, 12)
        incProgress(1, detail = "Finished...", session = ServerValues$monitor.domain)
        updateJSON()
      })
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
                                    name = serverValues$nodes$query.name[i],
                                    repr = queryString)
        
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