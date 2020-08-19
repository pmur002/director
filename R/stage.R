
## Set the stage (open and position windows)

setStage <- function(stage, setting) {
    ## Show desktop
    setting$focusWindow()
}


exitStage <- function(locations, setting) {
    apply(locations, 1, setting$closeWindow)
}
