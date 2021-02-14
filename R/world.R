
## Define a "world" to shoot a video within

directorWorld <- function(..., class) {
    world <- list(...)
    class(world) <- c(class, "DirectorWorld")
    world
}

realWorld <- directorWorld(class="DirectorRealWorld")


