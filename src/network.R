#' Fetches tweet from database.
#'
#' @param Rdata_file Path to an Rdata file.
#' @param name Name of the dataframe the Rdata file contains
#' @return Dataframe of tweet data.
fetchData <- function(Rdata_file) {
  df_name <- load(Rdata_file)
  data <- eval(parse(text = df_name))
  data <- data[!is.na(data$id_str), ]
  # Convert hashtag column to better format
  data$hashtags <- lapply(data$hashtags, function(x) {
    unlist(x)
  })
  data$user_mentions <- lapply(data$user_mentions, function(x) {
    unlist(x)
  })
  data$expanded_urls <- lapply(data$expanded_urls, function(x) {
    unlist(x)  
  })
  data$created_at <- sapply(data$created_at, function(x) {
    as.POSIXct(x, format="%a %b %d %H:%M:%S %z %Y")
  })
  # Add column to track row of tweet in main dataframe.
  data$orig_index <- 1:nrow(data)
  remove(df_name)
  return(data)
}

#' Gets subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_query Query object to subset from. 
#' @return Subset of main data based on query and type.
getSubset <- function(data, subset_query) {
  if (is.na(subset_query$q) | is.na(subset_query$colname)) {
    return(NULL)
  }
  if(subset_query$colname %in% colnames(data)) {
    contains <- sapply(1:nrow(data), function(i) {
      if (is.vector(subset_query$q)) {
        # if q is a vector and colname is a vector, check q[j] %in% data[i, colname[j]]
        if (is.vector(subset_query$colname)) {
          for (j in 1:length(subset_query$colname)) {
            q <- subset_query$q[j]
            colname <- subset_query$colname[j]
            if (tolower(q) %in% tolower(unlist(data[i, colname]))) {
              return(TRUE)
            }
          }
          return(FALSE)
        } 
        # if q is a vector and colname is not, check q[j] $in$ data[i, colname]
        else {
          colname <- subset_query$colname
          intersection <- intersect(tolower(subset_query$q), tolower(unlist(data[i, colname])))
          if (length(intersection) > 0) {
            return(TRUE)
          }
          else {
            return(FALSE)
          }
        }
      }
      else {
        # if q is not a vector and colname is vector, check q %in% data[i, colname[j]]
        if (is.vector(subset_query$colname)) {
          for (j in 1:length(subset_query$colname)) {
            q <- subset_query$q
            colname <- subset_query$colname[j]
            if (tolower(q) %in% tolower(unlist(data[i, colname]))) {
              return(TRUE)
            }
          }
          return(FALSE)
        }
        # if q is not a vector and colname is not, check q %in% data[i, colname]
        else {
          if (tolower(subset_query$q) %in% tolower(unlist(data[i, subset_query$colname]))) {
            return(TRUE)
          } 
          else {
            return(FALSE)
          }
        }
      }
    })
    data_subset <- filter(data, contains)
  }
  else {
    data_subset <- NULL
  }
  data_subset
}


#' Gets subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_queries Query objects to subset from. MUST all have the same colname. 
#' @param colname Column name to subset from.
#' @return Subset of main data based on queries and type.
getSubsetColname <- function(data, subset_queries, colname)
{
  queries <- sapply(subset_queries, function(x){
    x$q
  })
  if(colname %in% colnames(data))
  {
    contains <- sapply(1:nrow(data), function(i){
      if(any(queries %in% unlist(data[i, colname]))) {TRUE}
      else {FALSE}
    })
    data_subset <- filter(data, contains)
  }
  else
  {
    data_subset <- NULL
  }
  data_subset
}

#' Gets intersection subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_queries List of query objects to subset from. 
#' @return Subset of main data based on query and type.
getSubset2AND <- function(data, subset_queries)
{
  subset <- data
  for(i in 1:length(subset_queries))
  {
    subset <- getSubset(subset, subset_queries[[i]])
  }
  subset
}

#' Gets union subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_queries List of query objects to subset from. 
#' @return Subset of main data based on query and type.
getSubset2OR <- function(data, subset_queries)
{
  subsets <- vector(mode = "list", length = length(subset_queries))
  for(i in 1:length(subsets))
  {
    subsets[[i]] <- getSubset(data, subset_queries[[i]])
  }
  subset <- do.call("rbind", subsets) %>%
    distinct()
  subset
}

#' Gets graph node from data.
#' 
#' @param data Dataframe with tweet data.
#' @param node_query NodeQuery object.
#' @return Dataframe of node data for graph. Node is located at (0,0).
getNode <- function(data, node_query)
{
  node_subset <- getSubset(data, node_query$query)
  if(is.null(node_subset)) 
  {
    node_value <- NA
    node_orig_indices <- NA
  } 
  else 
  {
    node_value <- nrow(node_subset)
    node_orig_indices <- unlist(node_subset$orig_index)
  }
  node <- data.frame(id = node_query$query$q,
                     color = color.blue,
                     font = paste0("10px arial ", color.white),
                     value = node_value,
                     x = 0,
                     y = 0,
                     name = node_query$name,
                     colname = node_query$query$colname,
                     colvalue = node_query$query$q,
                     hidden = is.na(node_value),
                     stringsAsFactors = FALSE,
                     query = node_query)
  node$orig_indices <- list(node_orig_indices)
  node
}

#' Updates the x and y values of each node.
#' 
#' @param nodes Dataframe of node data.
#' @returns Dataframe of node data with correct positions.
updatePositions <- function(nodes) {
  radius <- 6
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  angles <- angles - (angles[1] - angles[2])/2
  for(i in 1:min(nrow(nodes), 12)) 
  {
    nodes$x[i] <- scale * radius * cos(angles[[i]])
    nodes$y[i] <- -scale * radius * sin(angles[[i]])
  }
  nodes
}

#' Gets nodes to graph in the network.
#' 
#'  @param data Dataframe with tweet data.
#'  @param node_queries List of NodeQuery objects.
#'  @return Node data frame.
getNodes <- function(data, node_queries)
{
  # node_dfs <- vector(mode = "list", length = length(node_queries))
  # for(i in 1:length(node_dfs))
  # {
  #   node_dfs[[i]] = getNode(data, node_queries[[i]]) 
  # }
  node_dfs <- mclapply(1:length(node_queries), function(i, node_queries) {
    getNode(data, node_queries[[i]])
  }, node_queries, mc.cores = length(node_queries))
  nodes <- do.call("rbind", node_dfs)
  nodes <- updatePositions(nodes)
  nodes
}

#' Gets edge subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param to_query Query representing the end node.
#' @param from_query Query representing the start node.
#' @param colname Column name to search in to create edges.
#' @return Subset of data containing rows from to_query, from_query that share common colname content.
getEdgeSubset <- function(data, to_query, from_query, colname, nodes)
{
  node1_indices <- nodes$orig_indices[nodes$id == to_query$q][[1]]
  node2_indices <- nodes$orig_indices[nodes$id == from_query$q][[1]]
  to_node_subset <- data[node1_indices, ]
  from_node_subset <- data[node2_indices, ]
  subset <- rbind(to_node_subset, from_node_subset)
  to_content <- unique(unlist(to_node_subset[ , colname]))
  from_content <- unique(unlist(from_node_subset[ , colname]))
  shared_content <- intersect(to_content, from_content)
  tmps <- vector(mode = "list", length = length(shared_content))
  if(length(shared_content > 0))
  {
    for(i in 1:length(tmps))
    {
      tmps[[i]] <- structure(list(q = shared_content[i], colname = colname),
                       class = "Query")
    }
  }
  edge_subset <- getSubsetColname(subset, tmps, colname)
  edge_subset
}

#' Gets single edge dataframe row.
#'
#' @param data Dataframe with tweet data.
#' @param to_query Query representing the start node.
#' @param from_query Query representing the end node.
#' @param colname Column name to search in to create edges.
#' @return Edge data frame to to_query from from_query.
getEdge <- function(data, to_query, from_query, colname, nodes, edge_color, edge_rounds, edge_id)
{
  size <- 0
  edge_subset <- getEdgeSubset(data, to_query, from_query, colname, nodes)
  if(!is.null(nrow(edge_subset)))
  {
    size <- nrow(edge_subset)
  }
  if(size != 0)
  {
    edge <- data.frame(to = to_query$q,
                       from = from_query$q,
                       value = size,
                       stringsAsFactors = FALSE)
    edge$colname <- colname
    edge$color <- edge_color
    edge$smooth <- list(list("type" = "continuous", "roundness" = edge_rounds))
    edge$id <- edge_id
    edge
  }
  else {NULL}
}

#' Gets all graph node edge data from data.
#' 
#' @param data Dataframe with tweet data.
#' @param subset_queries List of query objects to subset from.  
#' @param edge_colname Column name to search in to create edges.
#' @returns Dataframe of edge data.
getEdges <- function(data, node_queries, edge_colnames, nodes)
{
  nodes <- nodes[!is.na(nodes$id), ]
  subset_queries <- lapply(node_queries, function(x) {
    x$query
  })
  edges <- data.frame()
  node_combinations <- combn(1:length(subset_queries), 2)
  rounds <- seq(0, .5, length.out = length(edge_colnames))
  edge_colors <- c("#c51f5d", "white", "#008080")
  next_id <- 1
  for(i in 1:ncol(node_combinations))
  {
    if(!is.na(subset_queries[[node_combinations[ ,i][1]]]) && !is.na(subset_queries[[node_combinations[ ,i][2]]]))
    {
      for(j in 1:length(edge_colnames))
      {
        edge <- getEdge(data,
                        subset_queries[[node_combinations[ ,i][1]]],
                        subset_queries[[node_combinations[ ,i][2]]],
                        edge_colnames[[j]],
                        nodes,
                        edge_colors[[j]],
                        rounds[[j]],
                        next_id)
        next_id <- next_id + 1
        edges <- rbind(edges, edge)
      }
    }
  }
  edges
}

#' Get visNetwork object for the floor.
#' 
#' @param nodes Node dataframe.
#' @param edges Edge dataframe.
getNetwork <- function(nodes, edges)
{
  nodes <- nodes[!is.na(nodes$id), ]
  visNetwork(nodes, edges) %>%
    visEdges(scaling = list("min" = 2), smooth = list("enabled" = TRUE, type = "dynamic")) %>%
    visNodes(scaling = list("min" = 25, "max" = 75)) %>%
    # After drawing the network, center on 0,0 to keep position
    # independant of node number
    visPhysics(stabilization = FALSE, enabled = FALSE) %>%
    #visPhysics(solver = "barnesHut", 
    #           barnesHut = list(springConstant = 0)) %>%
    visInteraction(dragView = FALSE, zoomView = FALSE, dragNodes = FALSE) %>%
    visOptions(nodesIdSelection = TRUE) %>%
    visEvents(deselectEdge = "function(e) {Shiny.onInputChange('network_selected_e', '');}") %>%
    visEvents(selectEdge = "function(e) {if(e.nodes.length == 0){Shiny.onInputChange('network_selected_e', e.edges);}}") %>%
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