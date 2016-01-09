#' force/episode data
#' @export
force_episode <- function() {
  read.csv(system.file("extdata", "force_episode.csv",
                       package="forceaccounted"),
           stringsAsFactors=FALSE)
}

#' force/character data
#' @export
force_character <- function() {
  read.csv(system.file("extdata", "force_character.csv",
                       package="forceaccounted"),
           stringsAsFactors=FALSE)
}

#' force/type data
#' @export
force_type <- function() {
  read.csv(system.file("extdata", "force_type.csv",
                       package="forceaccounted"),
           stringsAsFactors=FALSE)
}

#' mentions data
#' @export
mentions <- function() {
  read.csv(system.file("extdata", "mentions.csv",
                       package="forceaccounted"),
           stringsAsFactors=FALSE)
}

#' timeline data
#' @export
timeline <- function() {
  read.csv(system.file("extdata", "timeline.csv",
                       package="forceaccounted"),
           stringsAsFactors=FALSE)
}

min_to_hhmm <- function(mins) { sprintf("  %02d:%02d  ", round(mins/60), (mins %% 60)) }
