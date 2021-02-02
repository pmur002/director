
getAttrs <- function(x, ...) {
    UseMethod("getAttrs")
}

getAttrs.xml_node <- function(x, attr, nomatch=1:length(x)) {
    attrs <- xml_attr(x, attr)
    if (is.na(attrs)) {
        if (!is.na(nomatch) && nomatch == "fail") {
            stop(paste0("Missing required attribute '", attr, "'"))
        } else {
            nomatch
        }
    } else {
        attrs
    }
}

getAttrs.xml_nodeset <- function(x, attr, nomatch=1:length(x)) {
    attrs <- xml_attr(x, attr)
    nas <- is.na(attrs)
    if (any(nas)) {
        if (!is.na(nomatch) && nomatch == "fail") {
            stop(paste0("Missing required attribute '", attr, "'"))
        }
        if (length(nomatch) < length(x)) {
            nomatch <- rep(nomatch, length.out=length(x))
        }
    }
    attrs[nas] <- nomatch[nas]
    attrs
}

readStage <- function(stage, locations) {
    if (inherits(stage, "xml_missing")) {
        stop("No stage upon which to play")
    }
    stagex <- xml_attr(stage, "x")
    stagey <- xml_attr(stage, "y")
    stagew <- xml_attr(stage, "width")
    stageh <- xml_attr(stage, "height")
    ## Allow for all locations
    label <- getAttrs(locations, "id")
    x <- getAttrs(locations, "x", nomatch=0)
    y <- getAttrs(locations, "y", nomatch=0)
    w <- getAttrs(locations, "width", nomatch=600)
    h <- getAttrs(locations, "height", nomatch=400)
    if (is.na(stagex)) {
        stagex <- min(as.numeric(x))
    }
    if (is.na(stagey)) {
        stagey <- min(as.numeric(y))
    }
    if (is.na(stagew)) {
        stagew <- max(as.numeric(x) + as.numeric(w))
    }
    if (is.na(stageh)) {
        stageh <- max(as.numeric(y) + as.numeric(h))
    }
    list(x=stagex, y=stagey, width=stagew, height=stageh,
         set=cbind(label, x, y, w, h, windowID=NA))
}

readCode <- function(action) {
    code <- sapply(action, xml_text)
    code[is.na(code)] <- ""
    ## Shrink all blank lines to nothing
    sapply(strsplit(code, "\n"),
           function(x) {
               paste(gsub("^\\s+$", "", x, perl=TRUE), collapse="\n")
           })
}

readScenes <- function(scenes, TTS) {
    shots <- xml_find_all(scenes, "shot")
    NS <- length(shots)
    dialogue <- sapply(lapply(shots, xml_find_first, "dialogue"), TTS$read)
    dialogue[is.na(dialogue)] <- ""
    keyaction <- xml_find_first(shots, "keyaction")
    code <- readCode(keyaction)
    ## TODO:  do something with <pointeraction> elements
    shotLabel <- getAttrs(shots, "id")
    location <- getAttrs(shots, "location", nomatch=NA)
    labels <- getAttrs(shots, "id")
    duration <- getAttrs(shots, "duration", nomatch=NA)
    record <- sapply(shots,
                     function(x) {
                         parentScene <- xml_find_all(x, "ancestor::scene")
                         getAttrs(x, "record",
                                  nomatch=getAttrs(parentScene, "record",
                                                   nomatch="TRUE"))
                     })
    keydelay <- getAttrs(keyaction, "keydelay", nomatch=100)
    linedelay <- getAttrs(keyaction, "linedelay", nomatch=100)
    sceneLabel <- unlist(mapply(
        function(x, i) {
            s <- xml_find_all(x, "shot")
            ns <- length(s)
            id <- xml_attr(x, "id")
            if (is.na(id)) id <- i
            rep(id, ns)
        },
        scenes, 1:length(scenes)))
    ## New locations
    creates <- sapply(shots,
                      function(x) {
                          location <- xml_find_first(x, "location")
                          if (length(location)) {
                              xml_attr(location, "id")
                          } else {
                              NA
                          }
                      })
    cbind(sceneLabel, shotLabel, code, dialogue,
          location, duration, record, keydelay, linedelay, 
          creates)
}

readSetting <- function(setting) {
    if (length(setting)) {
        code <- xml_text(setting)
        result <- eval(parse(text=code))
        if (!inherits(result, "DirectorSetting"))
            stop(paste("Invalid setting:\n", code))
        result
    } else {
        localLinux()
    }
}

scriptVersion <- "1.0"

## Check that <shot>s refer to existing <location>, etc
validateScript <- function(script) {
    version <- xml_attr(xml_root(script), "version")
    if (scriptVersion != version) {
        stop("Script version mismatch")
    }
    locations <- xml_attr(xml_find_all(script, "//location"), "id")
    shotLocns <- xml_attr(xml_find_all(script, "//shot"), "location")
    if (!all((shotLocns[!is.na(shotLocns)] == "backstage") ||
             (shotLocns[!is.na(shotLocns)] %in% locations))) {
        stop("Shot refers to non-existent location")
    }
}

readScript <- function(filename, TTS=espeakTTS(),
                       label=gsub("[.]xml", "", filename), 
                       validate=TRUE) {
    xml <- read_xml(filename, options=if (validate) "DTDVALID" else "")
    validateScript(xml)
    setting <- readSetting(xml_find_first(xml, "/script/setting"))
    stage <- readStage(xml_find_first(xml, "/script/stage"),
                       xml_find_all(xml, "//location"))
    shots <- readScenes(xml_find_all(xml, "/script/scene"), TTS)
    list(label=label, setting=setting, stage=stage, shots=shots)
}
