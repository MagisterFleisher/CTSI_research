
#' @title Visualize Graph
#'
#' @param nodes
#' @param edges
#' @param layout
#' @param arrows
#' @param smooth_edges
#' @param path_length
#' @param hover
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
#'
GraphToVisualization0 <- function(nodes, edges, layout, arrows, smooth_edges, path_length, hover, width, height) {
  visNetwork::visNetwork(nodes = nodes, edges = edges, width = width, height = height) %>%
    visNetwork::visIgraphLayout(layout = layout) %>%
    visExport(type = "png", name = "CTSI_Research_Network.png", label = "print") %>%
    visNetwork::visOptions(nodesIdSelection = F, highlightNearest = list(enabled = T, degree = path_length, hover = hover))
}
GraphToVisualization <- memoise::memoise(GraphToVisualization0)
