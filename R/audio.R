
## Record all dialogue
recordDialogue <- function(script, TTS, wd) {

    ## Create audio files
    shots <- script$shots
    infiles <- file.path(wd,
                         paste0("shot-", shots[, "shotLabel"], "-audio.txt"))
    outfiles <- file.path(wd,
                          paste0("shot-", shots[, "shotLabel"], "-audio.wav"))
    for (i in 1:nrow(shots)) {
        writeLines(shots[i, "dialogue"], infiles[i])
        TTS$speak(infiles[i], outfiles[i])
        ## ALL shots will be combined with something else at some point
        ## so prep them now
        wav <- readWave(outfiles[i])
        writeWave(prepComb(wav, where="end"), outfiles[i])
    }

    ## Return paths to audio files
    outfiles
}

audioLength <- function(wav) {
    length(wav@left)/wav@samp.rate
}

audioDuration <- function(audioFiles) {
    sapply(audioFiles,
           function(x) {
               wav <- readWave(x)
               audioLength(wav)
           })
}

padAudio <- function(audioFiles, audioDurations, shotDurations) {
    paddedAudioFiles <- gsub("[.]wav$", "-padded.wav", audioFiles)
    for (i in seq_along(audioFiles)) {
        audioWav <- readWave(audioFiles[i])
        ## NOTE that all audio was prepared for combining
        ## when it was recorded
        if (shotDurations[i] > audioDurations[i]) {
            difference <- shotDurations[i] - audioDurations[i]
            silence <- Wave(rep(0, difference*audioWav@samp.rate), 
                            samp.rate=audioWav@samp.rate,
                            bit=audioWav@bit)
            paddedAudio <- bind(audioWav, silence)
        } else {
            paddedAudio <- audioWav
        }
        writeWave(paddedAudio, paddedAudioFiles[i])
    }
    paddedAudioFiles
}
