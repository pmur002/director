
## A <setting> is an expression that generates a "DirectorSetting" object

## A "DirectorSetting" is a list of (required) functions to
## - create window
## - focus window
## - close window
## - send key events to (current) window
## - send pointer events to (current) window

createSetting <- function(create, focus, close, key, pointer) {
    setting <- list(createWindow = create,
                    focusWindow = focus,
                    closeWindow = close,
                    keyAction = key,
                    pointerAction = pointer)
    class(setting) <- "DirectorSetting"
    setting
}

## Just do everything on local machine
## (and assume that required programs are present)
localLinuxSetting <- function() {
    create <- function(location) {
        wid <- openWindow(location["command"])
        ## Sys.sleep(1)
        removeWindowState(wid, "maximized_horz")
        removeWindowState(wid, "maximized_vert")
        positionWindow(wid, location["x"], location["y"],
                       location["w"], location["h"])
        wid
    }
    focus <- function(which = NULL) {
        if (is.null(which)) {
            ## Show the desktop
            showDesktop()
        } else {
            focusWindow(which)
        }
    }
    close <- function(location) {
        closeWindow(location["windowID"])
    }
    key <- function(keys, delay) {
        typestring(keys, delay=delay)
    }
    mouse <- function() {
    }
    createSetting(create, focus, close, key, mouse)
}

## Do everything in docker container
## (and assume that the container has required software)
dockerSetting <- function(image) {
}
