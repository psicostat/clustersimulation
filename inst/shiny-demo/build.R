# This script is meant just for internal use. The script
# create a copy of the R/ folder into inst/shiny-demo/ folder
# to use the functions.
# the app within inst/shiny-demo/ is a simplified version of the
# full shiny app inst/shiny. New functions will be used also for 
# this app but the source code is independent.

# move R/* files into the R folder of the shiny app

app_dir <- "inst/shiny-demo"
funs <- list.files("R", pattern = "*.R", full.names = TRUE)

if(!dir.exists(file.path(app_dir, "R"))){
    dir.create(file.path(app_dir, "R"))
}

file.copy(funs, file.path(app_dir, "R"), overwrite = TRUE)

rsconnect::deployApp("inst/shiny-demo", 
                     appName = "clustersimulation-demo",
                     account = "psicostat")
