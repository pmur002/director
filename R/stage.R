
## Set the stage (open and position windows)

setStage <- function(stage) {
    showDesktop()
    set <- stage$set
    windowID <- apply(set, 1,
                      function(x) {
                          wid <- openWindow(x["program"])
                          ## Sys.sleep(1)
                          removeWindowState(wid, "maximized_horz")
                          removeWindowState(wid, "maximized_vert")
                          positionWindow(wid, x["x"], x["y"], x["w"], x["h"])
                          wid
                      })
    cbind(label=set[, "label"], windowID)
}


exitStage <- function(locations) {
    apply(locations, 1,
          function(x) {
              closeWindow(x["windowID"])
          })
}
