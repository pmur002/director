\name{NEWS}
\title{NEWS file for the director package}
\encoding{UTF-8}

\section{Changes in version 0.2-0}{
  \itemize{
    \item Add DTD for script XML files and validate against it by
    default.

    \item Add \code{TTS} argument to  \code{shootVideo} to allow user
    to select text-to-speech engine, plus add
    \code{TTS()} function for defining TTS engine, plus
    provide \code{espeakTTS()} engine and \code{pollyTTS()}
    (Amazon Polly) engine.

    \item Add \code{<setting>} element to script XML to allow script
    writer to specify the context within which the video should
    be shot, plus add \code{setting()} function to allow user to
    define a setting, plus add \code{localLinuxSetting()} function
    to define a local Linux context.

    \item Change \code{shootVideo} to \code{makeMovie}.

    \item Add a \code{world} argument to \code{makeMovie} to describe
    the world within which the movie will be shot.  Default is
    \code{realWorld} (localhost desktop), but \code{dockerWorld()} can
    be used to shoot video within Docker container.

    \item Add \code{docker} (logical) argument to \code{pollyTTS()}.
    If \code{TRUE}, then use offfical Amazon AWS CLI docker image
    to run Polly speech-to-text.
  }
}
