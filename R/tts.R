
## Allow for different TTS systems

## TTS$read() reads <dialogue> and returns text
## TTS$speak() turns text into audio
TTS <- function(read, speak, ...) {
    x <- list(read=read, speak=speak, params=list(...))
    class(x) <- "DirectorTTS"
    x
}

#######################################
## Default TTS ignores any SSML markup and uses espeak
defaultRead <- function(dialogue) {
    xml_text(dialogue)
}

eSpeak <- function(infile, outfile, ...) {
    if (.Platform$OS.type == "windows") {
        cmd <- paste(shortPathName(getOption("director.espeakPath")),
                     "-s 125 -v en -w", outfile,
                     "-f", infile)
        shell(cmd)
    } else {
        cmd <- paste(getOption("director.espeakPath"),
                     "-s 125 -v en -w", outfile,
                     "-f", infile)
        system(cmd)
    }
    outfile    
}

espeakTTS <- function() {
    if (is.null(getOption("director.espeakPath"))) {
        path <- Sys.which("espeak")
        if (path == "") {
            stop(paste("Unable to find 'espeak'",
                       "(try setting 'director.espeakPath' option)"))
        } else {
            options("director.espeakPath"=path)
        }
    }
    TTS(read=defaultRead, speak=eSpeak)
}

#######################################
## Amazon Polly

## Of course, user must have AWS account AND have AWS CLI installed
## and configured

## This is designed to only synthesize small amounts of speech (at once)

## Polly TTS keeps XML content (so SSML stays in)
pollyRead <- function(dialogue) {
    content <- xml_children(dialogue)
    if (length(content)) {
        as.character(content)
    } else {
        ""
    }
}

pollySpeak <- function(infile, outfile,
                       voice="Matthew",
                       engine="standard",
                       format="mp3",
                       sampleRate="",
                       language=NULL,
                       lexicons=NULL,
                       docker=FALSE) {
    dialogue <- paste(readLines(infile), collapse="\n")
    ## Try to determine whether this is SSML
    if (grepl("^<speak>", gsub("^ *", "", dialogue))) {
        textType <- "ssml"
    } else {
        textType <- "text"
    }
    ## Save result in same dir as final .wav
    outdir <- dirname(outfile)
    audiofile <- tempfile(tmpdir=outdir, fileext=paste0(".", format))
    if (is.null(lexicons)) {
        lexiconArgs <- ""
    } else {

    }
    if (is.null(language)) {
        languageArg <- NULL
    } else {
        languageArg <- c("--language-code", language)
    }
    if (nchar(sampleRate)) {
        sampleRateArg <- c("--sample-rate", sampleRate)
    } else {
        sampleRateArg <- NULL
    }
    pollycmd <- c("polly", "synthesize-speech",
                  "--engine", engine, 
                  "--output-format", format,
                  "--voice-id", voice, 
                  sampleRateArg,
                  languageArg,
                  "--text",
                  if (docker) {
                      ## Do NOT want the quotes around dialog when using
                      ## Polly via (stevedore interface to) docker
                      dialogue
                  } else {
                      paste0("'", dialogue, "'")
                  },
                  "--text-type", textType, 
                  audiofile)
    if (docker) {
        if (!requireNamespace("stevedore", quietly = TRUE)) {
            stop("The 'stevedore' package must be installed")
        }
        if (!stevedore::docker_available()) {
            stop("Docker must be (correctly) installed")
        }
        ## Run Polly command in a Docker container (based on AWS CLI image)
        docker <- stevedore::docker_client()
        docker$container$run("amazon/aws-cli",
                             pollycmd,
                             ## Mount local AWS credentials and config
                             ## AND mount movie working dir as location
                             ## where Polly result gets generated (/aws)
                             volumes=c(paste0(normalizePath("~/.aws"),
                                              ":/root/.aws"),
                                       paste0(normalizePath(getwd()),
                                              ":/aws")),
                             rm=TRUE)
    } else {
        awscmd <- paste("aws", paste(pollycmd, collapse=" "))
        system(awscmd)
    }
    if (format == "mp3") {
        wav <- readMP3(audiofile)
    } else {
        pcm <- read_audio_bin(audiofile)
        wav <- Wave(pcm, samp.rate=attr(pcm, "sample_rate"), pcm=TRUE)
    }
    ## If there is no dialogue, the recording will just be zeroes anyway
    if (all(wav@left == 0)) {
        prepped <- wav
    } else {
        prepped <- prepComb(wav, where="end")
    }
    writeWave(prepped, outfile)
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
                     lexicons=NULL,
                     ## Use official AWS CLI docker image?
                     docker=FALSE) {
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
        language=language, lexicons=lexicons,
        docker=docker)
}            
