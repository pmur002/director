
shootVideo <- function(filename,
                       wd=paste0(gsub("[.]xml$", "", filename), "-video"),
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
    
    script <- readScript(filename)
    audioFiles <- recordDialogue(script, wd)
    audioLength <- audioDuration(audioFiles)
    codeLength <- codeDuration(script)
    shotLength <- calculateTiming(script, audioLength, codeLength)
    locations <- setStage(script$stage)
    paddedAudioFiles <- padAudio(audioFiles, audioLength, shotLength)
    videoFiles <- recordAction(script, locations, shotLength, wd)
    exitStage(locations)
    muxAudioVideo(script, paddedAudioFiles, videoFiles, wd)
}