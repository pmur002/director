
calculateTiming <- function(script, audioDuration, codeDuration) {
    duration <- as.numeric(script$shots[, "duration"])
    ifelse(is.na(duration),
           pmax(audioDuration, codeDuration),
           duration)
}
