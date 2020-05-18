
## Set the stage (open and position windows)

setStage <- function(stage, setting) {
    ## Show desktop
    setting$focusWindow()
    ## Empty stage (for now)
    cbind(label=character(), windowID=character())
}


exitStage <- function(locations, setting) {
    apply(locations, 1, setting$closeWindow)
}
