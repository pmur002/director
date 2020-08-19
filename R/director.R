
shootVideo <- function(filename,
                       wd=paste0(gsub("[.]xml$", "", filename), "-video"),
                       TTS=espeakTTS(),
                       validate=TRUE,
                       clean=FALSE) {

    if (clean) {
        ## Clear working directory
        if (dir.exists(wd)) {
            unlink(wd, recursive=TRUE, force=TRUE)
        }
    }
    if (!dir.exists(wd)) {
        dir.create(wd)
    }
    
    script <- readScript(filename, TTS=TTS, validate=validate)
    audioFiles <- recordDialogue(script, TTS, wd)
    audioLength <- audioDuration(audioFiles)
    codeLength <- codeDuration(script)
    shotLength <- calculateTiming(script, audioLength, codeLength)
    setStage(script$stage, script$setting)
    paddedAudioFiles <- padAudio(audioFiles, audioLength, shotLength)
    wrap <- recordAction(script, shotLength, script$setting, wd)
    exitStage(wrap$script$stage$set, wrap$script$setting)
    muxAudioVideo(wrap$script, paddedAudioFiles, wrap$videoFiles, wd)
}
