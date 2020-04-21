
## Allow for different TTS systems

## TTS$read() reads <dialogue> and returns text
## TTS$speak() turns text into audio
TTS <- function(read, speak, ...) {
    x <- list(read=read, speak=speak, params=list(...))
    class(x) <- "director.TTS"
    x
}

#######################################
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

pollySpeak <- function(infile, outfile,
                       voice="Matthew",
                       engine="standard",
                       format="mp3",
                       sampleRate="",
                       language=NULL,
                       lexicons=NULL) {
    dialogue <- paste(readLines(infile), collapse="\n")
    ## Try to determine whether this is SSML
    if (grepl("^<speak>", gsub("^ *", "", dialogue))) {
        textType <- "ssml"
    } else {
        textType <- "text"
    }
    audiofile <- tempfile()
    if (is.null(lexicons)) {
        lexiconArgs <- ""
    } else {

    }
    if (is.null(language)) {
        languageArg <- ""
    } else {
        languageArg <- paste0("--language-code ", language, " ")
    }
    if (nchar(sampleRate)) {
        sampleRateArg <- paste0("--sample-rate ", sampleRate, " ")
    } else {
        sampleRateArg <- ""
    }
    system(paste0("aws polly synthesize-speech ",
                  "--engine ", engine, " ",
                  "--output-format ", format, " ",
                  "--voice-id ", voice, " ",
                  sampleRateArg,
                  languageArg,
                  "--text '", dialogue, "' ",
                  "--text-type ", textType, " ",
                  audiofile))
    if (format == "mp3") {
        wav <- readMP3(audiofile)
    } else {
        pcm <- read_audio_bin(audiofile)
        wav <- Wave(pcm, samp.rate=attr(pcm, "sample_rate"), pcm=TRUE)
    }
    writeWave(prepComb(wav, where="end"), outfile)
}

## Does not allow format="json" OR speechMarks, but those COULD
## be added to allow further processing on speech, such as
## dynamic word highlighting on subtitles ???
pollyTTS <- function(voice="Matthew",
                     engine=c("standard", "neural"),
                     format=c("mp3", "ogg_vorbis", "pcm"),
                     sampleRate="default",
                     ## Only relevant for bilingual voices 
                     language=NULL,
                     ## Special pronunciations
                     lexicons=NULL) {
    engine <- match.arg(engine)
    format <- match.arg(format)
    if (sampleRate == "default") {
        sampleRate <- switch(format,
                             mp3=,
                             ogg_vorbis=switch(engine,
                                               standard="22050",
                                               neural="24000"),
                             pcm="16000",
                             "22050")
    } else {
        validSample <- switch(format,
                              mp3=,
                              ogg_vorbis=c("8000", "16000", "22050", "24000"),
                              pcm=c("8000", "16000"))
        if (format == "json") {
            sampleRate <- ""
        } else {
            sampleRate <- as.character(sampleRate)
            if (!(sampleRate %in% validSample))
                stop("Invalid sample rate")
        }
    }
    TTS(read=pollyRead, speak=pollySpeak,
        voice=voice, engine=engine, format=format,
        sampleRate=sampleRate,
        language=language, lexicons=lexicons)
}            
