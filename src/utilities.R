###Colors###
color.green <- "#1dee7e"
color.pink <- "#ee1d8d"
color.orange <- "#ee7e1d"
color.white <- "#ffffff"
color.blue <- "#1D8DEE"
color.back <- "#151E29"
color.offback <- "#1B2737"
colors <- c("#1D8DEE", "#1dee7e", "#ee7e1d", "#ee1d8d", "#64B0F3", "#64F3A6", "#F3A664", "#B10D65", "#0D65B1", "#0DB159", "#B1590D", "#F364B0")


# Misc Functions ----------------------------------------------------------

parseColumnQuery <- function(string) {
  if (substring(string, 1, 1) == "#") {
    hashtagText <- substring(string, 2)
    query <- createNodeQuery(hashtagText, "hashtags", string)
    return(query)
  }
  if (substring(string, 1, 1) == "@") {
    mentionText <- substring(string, 2)
    query <- createNodeQuery(mentionText, "user_screen_name", string)
    return(query)
  }
  if (grepl(" ", string)) {
    splitString <- strsplit(string, " ", fixed = TRUE)
    if (length(splitString) == 0 & length(splitString[[1]]) < 2) {
      return(NULL)
    }
    colname <- splitString[[1]][1]
    value <- splitString[[1]][2]
    name <- if (length(splitString[[1]]) > 2) {
      splitString[[1]][3]
    } else {
      string
    }
    if (colname %in% c("mention", "mentions")) {
      query <- createNodeQuery(value, "user_mentions", name)
      return(query)
    }
    query <- createNodeQuery(value, colname, name)
    return(query)
  }
  return(NULL)
}

createNodeQuery <- function(q, colname, name)
{
  query <- structure(list(q = q, colname = colname))
  node_query <- structure(list(query = query, name = name))
  node_query
}


StringQueryToVector <- function(queries_string) {
  # Converts a string of queries to a vector of queries
  # 
  # Args:
  #   queries_string: String of queries, max length 12.
  #                   Individual queries longer than a word are put in double quotes.
  # Returns:
  #   Vector of queries from the string.
  queries <- scan(text = queries_string, what = "character", quiet = TRUE)
  if(length(queries) < 12) {
    queries[(length(queries) + 1):12] <- NA
  }
  return(queries)
}