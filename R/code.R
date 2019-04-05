
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
    echo <- as.logical(shots[, "echo"])
    code <- shots[, "code"]
    keydelays <- as.numeric(shots[, "keydelay"])
    linedelays <- as.numeric(shots[, "linedelay"])
    durations <- mapply(chunkDuration, code, keydelays, linedelays)
    ifelse(echo, durations, 0)
}

getLocationWindow <- function(loc, locations) {
    index <- locations[, "label"] == loc
    if (!any(index)) {
        NA
    } else {
        locations[index, "windowID"]
    }
}

recordAction <- function(script, locations, durations, wd) {

    ## Create video files
    shots <- script$shots
    outfiles <- file.path(wd,
                          paste0("shot-", shots[, "shotLabel"],
                                 "-code-video.webm"))

    for (i in 1:nrow(shots)) {
        ## Start recording
        w <- shots[i, "width"]
        if (is.na(w)) {
            w <- script$stage$width
        }
        h <- shots[i, "height"]
        if (is.na(h)) {
            h <- script$stage$height
        }
        ffmpeg(screenInput(w=as.numeric(w), h=as.numeric(h),
                           duration=durations[i]), 
               fileOutput(outfiles[i], vcodec=VP8()),
               overwrite=TRUE, wait=FALSE)

        ## Record time shot starts
        start <- proc.time()[3]
        
        ## Focus relevant window
        loc <- shots[i, "location"]
        if (!is.na(loc)) {
            focusWindow(getLocationWindow(loc, locations))
        }
        
        ## "type" code in window
        if (as.logical(shots[i, "echo"])) {
            lines <- strsplit(shots[i, "code"], "\n")[[1]]
            for (j in seq_along(lines)) {
                typestring(paste0(lines[j], "\n"),
                           delay=as.numeric(shots[i, "keydelay"]))
                Sys.sleep(as.numeric(shots[i, "linedelay"])/1000)
            }
        } else {
            ## (or just evaluate it if echo is FALSE)
            source(textConnection(shots[i, "code"]), new.env())
        }

        ## Pause if necessary until end of shot
        while (proc.time()[3] - start < durations[i]) {
            Sys.sleep(.1)
        }
    }

    ## Return paths to video files
    outfiles
}
