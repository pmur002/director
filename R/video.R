
combineSceneAudio <- function(label, audioFiles, wd) {
    wavs <- lapply(audioFiles, readWave)
    outfile <- file.path(wd, paste0("scene-", label, "-audio.wav"))
    writeWave(Reduce(bind, wavs), outfile)
    outfile
}

combineSceneVideo <- function(label, videoFiles, wd) {
    outfile <- file.path(wd, paste0("scene-", label, "-video.webm"))
    ffmpeg(concatInput(videoFiles),
           fileOutput(outfile, vcodec="copy"),
           overwrite=TRUE)
    outfile
}

muxScene <- function(label, audioFiles, videoFiles, wd) {
    audio <- combineSceneAudio(label, audioFiles, wd)
    video <- combineSceneVideo(label, videoFiles, wd)
    inputs <- lapply(c(audio, video), fileInput)
    outfile <- file.path(wd, paste0("scene-", label, "-movie.webm"))
    ffmpeg(inputs,
           fileOutput(outfile, vcodec="copy", acodec="libvorbis"),
           overwrite=TRUE)
    outfile    
}
                         
muxAudioVideo <- function(script, audioFiles, videoFiles, wd) {
    
    ## Mux individual scenes
    sceneLabels <- script$shots[, "shotLabel"]
    sceneNames <- unique(sceneLabels)
    sceneAudio <- split(audioFiles, sceneLabels)[sceneNames]
    sceneVideo <- split(videoFiles, sceneLabels)[sceneNames]
    scenes <- mapply(muxScene, sceneNames, sceneAudio, sceneVideo,
                     MoreArgs=list(wd=wd), SIMPLIFY=TRUE)
    
    ## Combine all scenes into main feature
    feature <- file.path(wd, paste0(script$label, "-movie.webm"))
    ffmpeg(concatInput(scenes),
           fileOutput(feature, vcodec="copy", acodec="copy"),
           overwrite=TRUE)
        
    list(feature=feature, scenes=scenes)
}

