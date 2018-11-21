#' @title CTSIreasearch
#'   @author Aaron Fleisher
#'
#'   @include functions.R
#'
#'   @note Naming Scheme
#'   variables - snake_case
#'   functions - camelCase
#'
#'   variables - noun_adjective
#'   functions - nounVerbnoun
#'


# libraries ----
library( shiny )
library( shinyWidgets )
library( shinydashboard )
library( flexdashboard )
library( data.table )
library( visNetwork )
library( igraph )
library( colorspace )
library( wesanderson )
library( stringr )
library( memoise )
library( future.apply )


# headers ----
source( file = "data_retrieve.R", local = T )$value
source( file = "old.R", local = T) $value
source( file = "global.R" )$value
source( file = "search.R" )$value
source( file = "visualization.R" )$value


# ui ----
ui <- shinydashboard::dashboardPage(
  ## skin ====
  skin = sample(skin_colors, 1),

  ## Application title ----
  header = shinydashboard::dashboardHeader(title = app_title),

  ## Sidebar ====
  sidebar = shinydashboard::dashboardSidebar(
    ### choose graph actionButton ----
    shinydashboard::sidebarSearchForm(textId = "search_side", buttonId = "search_side_enter", label = "search")),

  ## body ====
  body = shinydashboard::dashboardBody(
    visNetwork::visNetworkOutput(outputId = "vis", width = "100%", height = "100vh"))
)


# server ----
server <- function( input, output ) {

  ## reactiveValues ====
  ### graph - reactiveValues ####
  graph <- reactiveValues( graph = NULL, edge = NULL, node = NULL, x = NULL)

  ## reactive ====
  ### buildGraph - reactive ####
  buildGraph <-  shiny::reactive({
    all <- searchInit()
    nodes <- all[type %in% "node",.(nid),]
    edges <- all[type %in% "edge", .(eid = nid)]
    graph$graph <- NodeToEdge( unlist( nodes ), edges )
    visualize()
    shiny::removeModal()})

  ### searchInit - reactive ####
  searchInit  <- shiny::reactive({

  })

  ## observe ====

  ### makeModal observe ####
  shiny::observe({
    if(load_status == FALSE ) {
      makeModal()
      load_status <- TRUE }})

  ### visNetworkProxy - observe ####
  observe({
    req( input$build )
    visNetworkProxy( "vis" ) %>%
      visFocus( id = input$focus, scale = 2 )})

  ## observeEvent ====

  shiny::observeEvent(input$refined, {
    results_refined <- input$refined
    results_evermore_refined <- stringr::str_split(string = results_refined, pattern = ",", simplify = T) %>% as.data.table()
    names(results_evermore_refined) <- c("attribute", "value")
    graph$x <- Map(AttributeToNodeOrEdge, results_evermore_refined$attribute, results_evermore_refined$value)
  })

  ###  input$search_init_text - observeEvent ####
  shiny::observeEvent(input$search_init_text, {
    results <- SearchThroughEverything(input$search_init_text)
    output$refine_search <- renderUI({
      selectInput(inputId = "refined", label = "Refine your search",
                  choices = results[,paste0(attribute,",", value),],
                  multiple = T, selectize = T, width = "100%")
      })
    })

  shiny::observeEvent(input$graph_build, {
    req(graph$x)
    nodes <- rbindlist(graph$x)[,nid,]
    graph$graph <- NodeToEdge(nodes)
    Visualize()
    shiny::removeModal()
  })
  ## output ====

  ### makeModel() NULL -> void ####
  makeModal <- reactive({
    showModal(modalDialog(
      fade = T, title = "search",
      size = "l", footer = NULL,

      shinyWidgets::searchInput(
        inputId = "search_init_text",
        label = "",
        placeholder = "Try searching for anthropology, or Chris McCarty, or literary theory",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "100%"),
      shiny::span(style = "text-align: right;", shiny::helpText("Create a network using keywords or centered around a person, department, country, or city." )),
      conditionalPanel(
        condition = "input.search_init_text",
        shiny::verbatimTextOutput(output= "debug"),
        shiny::uiOutput(outputId = "refine_search"),
        shinyWidgets::actionBttn(inputId = "graph_build", label = "CREATE NETWORK", style = "unite", color = "royal", block = T)
        )
      ))})
  ### selection ui ####
  output$select_node <- shiny::renderUI({
    req( graph$graph )

    shiny::selectInput( inputId = "focus", label = "zoom", choices = {
      nodes <- graph$graph[,.( id = as.character(c( to, from ))),][, .N, by = id ] %>% unique
      nodes[,.(nid = id) ,][ NodeToAttributeValue( nodes, "name" ), on = "nid" ][,c( attribute ),]},
      multiple = F, selectize = T )})

  ### result_network_description - output ####
  output$result_network_description <- renderPlot({
    req(graph$graph)
    people <- length( unlist( unlist( graph$graph[,.( from, to ),])))
    connections <- graph$graph[, .N, ]
    stat_list <- c( people, connections )
    barplot(stat_list)})


  IncreaseRadius <- reactive({
    graph$graph
    nodes1 <- graph$graph[,.(id = c(from,to)),][,.N, by = id][,.(id, size = N),]
    nodes2 <- fread("data/edges.csv")[from %in% nodes1$id | to %in% nodes1$id, .(id = c(from,to)),][,.N, by = "id"][!id %in% nodes1][,.(id, size = N),]
    nodes3<- fread("data/edges.csv")[from %in% nodes2$id | to %in% nodes2$id, .(id = c(from,to)),][,.N, by = "id"][,.(id, size = N),]
    nodes3[id %in% nodes2$id][order(size, decreasing = T)][c(1,4)] %>% unique

  })

  ### visualize ####
  Visualize <- reactive({
    req(graph$graph)

    output$vis <- visNetwork::renderVisNetwork({
      nodes <- IncreaseRadius()
      edges <- fread("data/edges.csv")[from %in% nodes$id][to %in% nodes$id][,.(from,to),]
      layout <- "layout_with_fr"
      arrows <- input$arrows
      smooth_edges <- input$smooth_edges
      #path_length <- input$path
      hover <- FALSE
      width <- "100%"
      height <- "100vh"
      GraphToVisualization( nodes, edges, layout, arrows, smooth_edges, path, hover, width, height )}) })}

# Run the application ----
shinyApp( ui = ui, server = server )
