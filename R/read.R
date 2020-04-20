
## Assumes 'x' is one or more xml_nodes
getAttrs <- function(x, attr, nomatch=1:length(x)) {
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

readStage <- function(stage) {
    if (inherits(stage, "xml_missing")) {
        stop("No stage upon which to play")
    }
    stagex <- xml_attr(stage, "x")
    stagey <- xml_attr(stage, "y")
    stagew <- xml_attr(stage, "width")
    stageh <- xml_attr(stage, "height")
    locations <- xml_find_all(stage, "location")
    label <- getAttrs(locations, "id")
    program <- getAttrs(locations, "program", nomatch="fail")
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
         set=cbind(label, program, x, y, w, h, windowID=NA))
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

readScenes <- function(scenes) {
    shots <- xml_find_all(scenes, "shot")
    NS <- length(shots)
    dialogue <- sapply(lapply(shots, xml_find_first, "dialogue"), xml_text)
    dialogue[is.na(dialogue)] <- ""
    keyaction <- xml_find_first(shots, "keyaction")
    code <- readCode(keyaction)
    ## TODO:  do something with <pointeraction> elements
    shotLabel <- getAttrs(shots, "id")
    width <- getAttrs(shots, "width", nomatch=NA)
    height <- getAttrs(shots, "height", nomatch=NA)
    location <- getAttrs(shots, "location", nomatch=NA)
    echo <- getAttrs(keyaction, "echo", nomatch="TRUE")
    labels <- getAttrs(shots, "id")
    duration <- getAttrs(shots, "duration", nomatch=NA)
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
    cbind(sceneLabel, shotLabel, code, dialogue,
          location, width, height, duration, keydelay, linedelay, echo)
}

readScript <- function(filename, label=gsub("[.]xml", "", filename),
                       validate=TRUE) {
    xml <- read_xml(filename, options=if (validate) "DTDVALID" else "")
    stage <- readStage(xml_find_first(xml, "/script/stage"))
    shots <- readScenes(xml_find_all(xml, "/script/scene"))
    list(label=label, stage=stage, shots=shots)
}
