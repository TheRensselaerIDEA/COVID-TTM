# # Node Functions ----------------------------------------------------------
getPosition <- function(queries) {
  # Get location around the circle of each node.
  # 
  # Args:
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #
  # Returns:
  #   Data frame column with the position of each query's node, # from 1-12.
  position <- vapply(1:length(queries), function(i){
    if(!is.na(queries[[i]])){
      i
    } else {
      0
    }
  }, numeric(1))
  position_ <- position[position != 0]
  return(position_) 
}

getCoords <- function(nodes) {
  # Get location (x,y) for each node.
  # 
  # Args:
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #   nodes: Data frame 
  #
  # Returns:
  #   Data frame column with the position of each query's node.
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  nodes$x <- vapply(nodes$position_, function(i) {
    if(i <= 12) {
      scale * radius * cos(angles[[i]])
    } else {
      0
    }
  }, numeric(1))
  nodes$y <- vapply(nodes$position_, function(i) {
    if(i <= 12) {
      -scale * radius * sin(angles[[i]])
    } else {
      0
    }
  }, numeric(1))
  return(nodes)
}