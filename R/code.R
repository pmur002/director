
lineDuration <- function(line, delay) {
    nspaces <- sum(gregexpr("\\s", line, perl=TRUE)[[1]] > -1)
    (nchar(line) - nspaces)*delay/1000
}

chunkDuration <- function(chunk, keydelay, linedelay) {
    lines <- strsplit(chunk, "\n")[[1]]
    if (length(lines)) {
        sum(sapply(lines, lineDuration, keydelay)) +
            linedelay/1000*(length(lines) - 1)
    } else {
        0
    }
}

codeDuration <- function(script) {
    shots <- script$shots
    record <- as.logical(shots[, "record"])
    code <- shots[, "code"]
    keydelays <- as.numeric(shots[, "keydelay"])
    linedelays <- as.numeric(shots[, "linedelay"])
    durations <- mapply(chunkDuration, code, keydelays, linedelays)
    ifelse(record, durations, 0)
}

getLocationWindow <- function(loc, locations) {
    index <- locations[, "label"] == loc
    if (!any(index)) {
        NA
    } else {
        locations[index, "windowID"]
    }
}

recordAction <- function(script, durations, setting, wd) {

    ## Create video files
    shots <- script$shots
    outfiles <- file.path(wd,
                          paste0("shot-", shots[, "shotLabel"],
                                 "-code-video.webm"))

    for (i in 1:nrow(shots)) {
        ## Start recording
        w <- script$stage$width
        h <- script$stage$height
        ffmpeg(screenInput(w=as.numeric(w), h=as.numeric(h),
                           duration=durations[i]), 
               fileOutput(outfiles[i], vcodec=VP8()),
               overwrite=TRUE, wait=FALSE)

        ## Record time shot starts
        start <- proc.time()[3]
        
        ## Focus relevant window
        loc <- shots[i, "location"]
        backstage <- is.na(loc) || loc == "backstage"
        if (!backstage) {
            setting$focusWindow(getLocationWindow(loc, script$stage$set))
        }
        
        ## "type" code in window
        code <- shots[i, "code"]
        if (backstage) {
            locnID <- shots[i, "creates"]
            if (!is.na(locnID)) {
                whichLocn <- script$stage$set[,"label"] == locnID
                locn <- script$stage$set[whichLocn,]
                windowID <- setting$createWindow(code, locn)
                ## Record windowID
                script$stage$set[whichLocn, "windowID"] <- windowID
            } else {
                system(code)
            }
        } else {
            lines <- strsplit(code, "\n")[[1]]
            for (j in seq_along(lines)) {
                setting$keyAction(paste0(lines[j], "\n"),
                                  delay=as.numeric(shots[i, "keydelay"]))
                Sys.sleep(as.numeric(shots[i, "linedelay"])/1000)
            }
            locnID <- shots[i, "creates"]
            if (!is.na(locnID)) {
                whichLocn <- script$stage$set[,"label"] == locnID
                locn <- script$stage$set[whichLocn,]
                windowID <- setting$captureWindow(locn)
                ## Record windowID
                script$stage$set[whichLocn, "windowID"] <- windowID
            }
        }
    
        ## Pause if necessary until end of shot
        while (proc.time()[3] - start < durations[i]) {
            Sys.sleep(.1)
        }
    }

    ## Return updated script (e.g., windowIDs filled in)
    ## AND paths to video files
    list(script=script, videoFiles=outfiles)
}
