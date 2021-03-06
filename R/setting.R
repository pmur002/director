
## A <setting> is an expression that generates a "DirectorSetting" object

## A "DirectorSetting" is a list of (required) functions to
## - create window (open window by running code in shell)
## - capture window (find window created by code typed into other window)
## - focus window
## - close window
## - send key events to (current) window
## - send pointer events to (current) window

setting <- function(create, capture, focus, close, key, pointer) {
    setting <- list(createWindow = create,
                    captureWindow = capture,
                    focusWindow = focus,
                    closeWindow = close,
                    keyAction = key,
                    pointerAction = pointer)
    class(setting) <- "DirectorSetting"
    setting
}

## Just do everything on local machine
## (and assume that required programs are present)
localLinux <- function() {
    create <- function(command, location) {
        wid <- wmctrl::openWindow(command)
        ## Sys.sleep(1)
        wmctrl::removeWindowState(wid, "maximized_horz")
        wmctrl::removeWindowState(wid, "maximized_vert")
        wmctrl::positionWindow(wid, location["x"], location["y"],
                               location["w"], location["h"])
        wid
    }
    capture <- function(location) {
        wid <- xdotool::windowWithFocus()
        wmctrl::positionWindow(wid, location["x"], location["y"],
                               location["w"], location["h"])
        wid
    }
    focus <- function(which = NULL) {
        if (is.null(which)) {
            ## Show the desktop
            wmctrl::showDesktop()
        } else {
            wmctrl::focusWindow(which)
        }
    }
    close <- function(location) {
        wmctrl::closeWindow(location["windowID"])
    }
    key <- function(keys, delay) {
        xdotool::typestring(keys, delay=delay)
    }
    mouse <- function() {
    }
    setting(create, capture, focus, close, key, mouse)
}

## Do everything in docker container
## (and assume that the container has required software)
dockerSetting <- function(image) {
}

localWindows <- function() {
    create <- function(command, location) {
        wid <- autohotkey::openWindow(command)
        ## Sys.sleep(1)
        autohotkey::positionWindow(wid, location["x"], location["y"],
                                   location["w"], location["h"])
        wid
    }
    capture <- function(location) {
        stop("Not yet implemented :(")
    }
    focus <- function(which = NULL) {
        if (is.null(which)) {
            ## Show the desktop
            autohotkey::showDesktop()
        } else {
            autohotkey::focusWindow(which)
        }
    }
    close <- function(location) {
        autohotkey::closeWindow(location["windowID"])
    }
    key <- function(keys, delay) {
        autohotkey::typeString(keys)
    }
    mouse <- function() {
    }
    setting(create, capture, focus, close, key, mouse)    
}
