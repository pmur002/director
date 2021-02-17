
## Support for shooting a video in a Docker "world"

dockerWorld <- function(image, volumes=NULL, ...) {
    directorWorld(image=image, volumes=volumes,
                  class="DirectorDockerWorld")
}

shootVideo.DirectorDockerWorld <- function(world,
                                           filename, wd, TTS, validate, clean) {
    if (!requireNamespace("stevedore", quietly = TRUE)) {
        stop("The 'stevedore' package must be installed")
    }
    if (!stevedore::docker_available()) {
        stop("Docker must be (correctly) installed")
    }
    ## Start a Docker container
    ## (based on world$image)
    docker <- stevedore::docker_client()
    container <-
        docker$container$create(world$image,
                                ## Keep container open
                                "/bin/bash", tty=TRUE,
                                ## Mount local output directory
                                ## AND local docker engine socket 
                                ## (so docker containers created within this
                                ##  container will be "sibling" containers)
                                ## AND any world$volumes specified by the user
                                volumes=c(world$volumes,
                                          paste0("/var/run/docker.sock",
                                                 ":/var/run/docker.sock"),
                                          paste0(normalizePath(getwd()),
                                                 ":/home/director")),
                                working_dir="/home/director")
    container$start()
    ## Start Xvfb
    cmd <- c("Xvfb", ":1", "-screen", "0", "1920x1200x24")
    container$exec(cmd, detach=TRUE)
    ## give Xvfb a chance to come up
    Sys.sleep(1)
    ## Start WM
    cmd <- c("metacity", "--display=:1", "--replace")
    container$exec(cmd, detach=TRUE)
    ## give WM a chance to come up
    Sys.sleep(1)

    ## Prepare serialised version of arguments for Docker container
    saveRDS(list(filename=filename,
                 wd=wd,
                 TTS=TTS,
                 validate=validate,
                 clean=clean),
            "makeMovie-args.rds")
    ## Run R function in Docker container that loads serialised
    ## arguments and calls director::makeMovie()
    cmd <- c("Rscript", "-e", "do.call(director::makeMovie, readRDS(\"makeMovie-args.rds\"))")
    container$exec(cmd)
    
    container$stop()
    container$remove()
}

