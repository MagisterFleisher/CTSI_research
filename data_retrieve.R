source( file = "global.R",
  local = T )$value


#' Title
#'
#' @param attribute
#'
#' @return
#' @export
#'
#' @examples
AttributePathFile0 <- function( attribute ) {
  paste0( "data/", {attribute  %>%
    NodeOrEdge %>%
    NodeOrEdgePrefix }, "_" , attribute , ".csv" ) }
AttributePathFile <- memoise::memoise( AttributePathFile0 )

CacheF <- cache_memory( algo = "sha512" )

#' @title Node or Edge
#'
#' @param attribute
#'
#' @return
#' @export
#'
#' @examples
NodeOrEdge0 <- function( attribute2 ) {
  fread( "data/attributes.csv" )[ attribute %in% attribute2, type ,]
}
NodeOrEdge <- memoise::memoise( NodeOrEdge0 )


#' @title Type to file prefix
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
NodeOrEdgePrefix0 <- function( edge_or_node ) {
  switch( edge_or_node,
    "node" = "nid",
    "edge" = "eid" )
}
NodeOrEdgePrefix <- memoise::memoise( NodeOrEdgePrefix0 )




#' Edgelist to igraph graph
#'
#' @param edgelist
#'
#' @return
#' @export
#'
#' @examples
EdgeToIgraph0 <- function( edgelist ) {
  igraph::graph_from_edgelist(
    as.matrix(
      edgelist[,.( from, to ),]),
    directed = F )
}
EdgeToIgraph <- memoise::memoise( EdgeToIgraph0 )


#' @title layout
#'
#' @param layout
#' @param edge
#'
#' @return
#' @export
#'
#' @examples
#'
Layout0 <- function( layout_choice, edgelist ) {
  igraph_graph <- EdgeToIgraph( edgelist )
  switch( layout_choice,
    "fr" = "layout_with_fr",
    "kk" = "layout_with_kk",
    "mds" = "layout_with_mds",
    "tree" = "layout_as_tree",
    "circle" = "layout_in_circle",
    "sugiyama" = "layout_with_sugiyama" )}
Layout <- memoise::memoise( Layout0 )


# new ---------------------------------------------------------------------


#' @title Node to Attribute
#'
#' @param n
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#'
NodeToAttributeValue0 <- function( node, attribute2 ) {
  node2 <- as.character( node )
  attribute3 <- as.character( attribute2 )

  attribute_id <- fread( AttributePathFile( attribute3 ),
    colClasses = c( "character", "character" ),
    key = "nid")[ node2 ][,.(nid, aid),]

setkey( attribute_id, aid )

fread( paste0("data/aid_", attribute3 ,".csv"),
  colClasses = c( "character", "character" ),
  key = "aid" )[ attribute_id ][,.( nid, attribute ),]
}
NodeToAttributeValue <- memoise::memoise( NodeToAttributeValue0 )


#' @title Edge to Node
#'
#' @param edgelist
#'
#' @return
#' @export
#'
#' @examples
EdgeToNode0 <- function(edgelist) {
  list(unique(unlist(edgelist)))
  }
EdgeToNode <- memoise::memoise( EdgeToNode0 )


#' @title Membership
#'
#' @param membership
#' @param e
#'
#' @return
#' @export
#'
#' @examples
#'
NodeMembership0 <- function( membership, edegelist ) {
  switch( membership,
    "department" = NULL,
    "louvain" = NULL )}
NodeMembership <- memoise::memoise( NodeMembership0 )


#' @title Attribute to Node
#'
#' @param a
#' @param e
#'
#' @return
#' @export
#'
#' @examples
#'
AttributeToNode0 <- function( attribute2, edge ) {
  prefix <- attribute2 %>%
    NodeOrEdge %>%
    NodeOrEdgePrefix

  attribute_file <- fread( paste0( "data/aid_", attribute2, ".csv" ),
    col.names = c( "aid", "attribute" ), key = "attribute" )

  setkey( attribute_file , aid )

  # nid or eid file # is file nid or eid?  # join on the att file
  fread( paste0( "data/", prefix , "_", attribute2 , ".csv"),
    key = "aid" )[ attribute_file ][ aid %in% edge ][, aid := NULL ,][, attribute := NULL, ]}

AttributeToNode <- memoise::memoise( AttributeToNode0 )


#' @title Node to Edge
#'
#' @param node
#'
#' @return
#' @export
#'
#' @examples
NodeToEdge0 <- function( node, edge ) {
  unique(
  fread( "data/edges.csv",
  key = "eid")[ edge ][,.( from, to ),][ from %in% node | to %in% node ][ !is.na(from) ][ !is.na(to) ])}
NodeToEdge <- memoise::memoise( NodeToEdge0 )


#' @title Attribute to Node or Edge
#'
#' @param attribute
#'
#' @return
#' @export
#'
#' @examples
AttributeToNodeOrEdge0 <- function( attribute2, value ) {
# aids from at # create file name # pull by attributes
attribute_file <- fread( paste0( "data/aid_", attribute2 ,".csv"),
  col.names = c( "aid", "attribute" ),
  key = "attribute" )[ attribute %in% value ]

  setkey( attribute_file, aid )

  ################# TEMPORARY ##################
  # get rid of NA responses

#nid or eid file # is file nid or eid?  # join on the att file
fread( AttributePathFile( attribute2 ),
  key = "aid")[ attribute_file ][, aid := NULL,]
}
AttributeToNodeOrEdge <- memoise::memoise(AttributeToNodeOrEdge0)




