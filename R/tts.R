
## Allow for different TTS systems

## TTS$read() reads <dialogue> and returns text
## TTS$speak() turns text into audio
TTS <- function(read, speak) {
    x <- list(read=read, speak=speak)
    class(x) <- "director.TTS"
    x
}

## Default TTS ignores any SSML markup and uses espeak
defaultTTS <- TTS(read=function(dialogue) xml_text(dialogue),
                  speak=function(infile, outfile) {
                      system(paste("espeak -s 125 -v en -w", outfile,
                                   "-f", infile))
                      outfile
                  })

