
## Set the stage (open and position windows)

setStage <- function(stage, setting) {
    ## Show desktop
    setting$focusWindow()
    set <- stage$set
    windowID <- apply(set, 1, setting$createWindow)
    cbind(label=set[, "label"], windowID)
}


exitStage <- function(locations, setting) {
    apply(locations, 1, setting$closeWindow)
}
