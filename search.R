
source("data_retrieve.R")$source
#' Useful data retrieval functions
#'
#' @title AttributePathFile
#' @description Attribute to Node or Edge
#'
#' @title NodeToEdge( node, edge )
#' @description Node to Edge
#'
#' @title AttributeToNode( attribute2, edge )
#' @description Attribute to Node
#'
#' @title EdgeToNode(edgelist)
#' @description Edge to Node
#'
#' @title NodeToAttributeValue( node, attribute2 )
#' @description Node to Attribute
#'
#' @title EdgeToIgraph( edgelist )
#' @description Edgelist to igraph graph
#'
#' @title NodeOrEdge0( attribute2 )
#' @description Node or Edge
#'
#' @title AttributePathFile( attribute )


#' @title Search Though Everything
#'
#' @param user_input
#'
#' @return relevant_results
#' @return companian_meta_data
#'
#' @export
#'
#' @examples
SearchThroughEverything <- function( user_input ) {
attributes_file <- fread("data/attributes.csv")
attributes_file
user_input <- "Mull"
attributes_file[!(attribute %in% "month")][,str_remove(att_filename, "new/"),]
lapply(1:(dim(attributes_file)[1] - 1), function(x) {
  search_file <- str_remove(attributes_file[x]$att_filename, "new/") %>% fread(., key = "attribute")
  aids <- search_file[attribute %like% user_input,.(aid, value = attribute),]
  setkey(search_file, aid)
  data_file <- str_remove(attributes_file[x]$filename, "new/") %>% fread(., key = "aid")
  data_file[aids][,attribute := attributes_file[x]$attribute,][,type := attributes_file$type,][,.(aid, value, attribute, type),]
  }) %>% rbindlist()
}
