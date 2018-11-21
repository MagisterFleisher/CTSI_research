load_status <- F
keybinding <- '$(document).on("keypress", function (e) { if(e.keycode == 13) { Shiny.onInputChange("submit_enter", e.which);}});'
skin_colors <- c("blue", "black", "purple", "green", "red", "yellow")
app_title <- "CTSI Research"
membership_choices <- c("Departments" = "department", "Structual Groups" = "louvain")
metric_choices <- c("Brokerage" = "betweenness", "Co-authors" = "degree")
layout_choices <- c( "small Network" = "fr", "large Network" = "kk", "tree" = "tree", "circle" = "circle")
palatte_choices <- c("BottleRocket1", "BottleRocket2", "Rushmore1", "Royal1", "Royal2",  "Zissou1", "Darjeeling1", "Darjeeling2", "Chevalier1" , "FantasticFox1" , "Moonrise1", "Moonrise2", "Moonrise3",  "Cavalcanti1", "GrandBudapest1", "GrandBudapest2", "IsleofDogs1", "IsleofDogs2")
size_scale_init <- c(10, 60)
size_scale_min <- 1
size_scale_max <- 100
year_start <- 2014
year_end <- 2017
