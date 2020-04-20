
## Allow for different TTS systems

## TTS$read() reads <dialogue> and returns text
## TTS$speak() turns text into audio
TTS <- function(read, speak, ...) {
    x <- list(read=read, speak=speak, params=list(...))
    class(x) <- "director.TTS"
    x
}

## Default TTS ignores any SSML markup and uses espeak
defaultRead <- function(dialogue) {
    xml_text(dialogue)
}
defaultSpeak <- function(infile, outfile, ...) {
    system(paste("espeak -s 125 -v en -w", outfile,
                 "-f", infile))
    outfile    
}
defaultTTS <- TTS(read=defaultRead, speak=defaultSpeak)

#######################################
## Amazon Polly

## Of course, user must have AWS account AND have AWS CLI installed
## and configured

## This is designed to only synthesize small amounts of speech (at once)

## Polly TTS keeps XML content (so SSML stays in)
pollyRead <- function(dialogue) {
    as.character(xml_children(dialogue))
}
pollySpeak <- function(infile, outfile, voice="Matthew") {
    dialogue <- paste(readLines(infile), collapse="\n")
    ## Try to determine whether this is SSML
    if (grepl("^<speak>", gsub("^ *", "", dialogue))) {
        textType <- "ssml"
    } else {
        textType <- "text"
    }
    mp3file <- tempfile()
    system(paste0("aws polly synthesize-speech ",
                  "--output-format mp3 ",
                  "--voice-id ", voice, " ",
                  "--text '", dialogue, "' ",
                  "--text-type ", textType, " ",
                  mp3file))
    ## mp3 to WAV
    writeWave(prepComb(readMP3(mp3file), where="end"), outfile)
}
pollyTTS <- TTS(read=pollyRead, speak=pollySpeak, voice="Matthew")
                
