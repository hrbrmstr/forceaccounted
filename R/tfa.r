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

#' good vs evil
#' @export
good_vs_evil <- function() {

  fe <- mutate(force_episode(), episode=factor(episode, levels=c(rev(episode), "ep0")))
  fe$rn <- c("I", "II", "III", "IV", "V", "VI")

  gg <- ggplot(fe)
  gg <- gg + stat_identity(aes(x=episode, y=-total_light),
                           geom="bar", fill="#78dcff", width=0.75)
  gg <- gg + geom_text(aes(x=episode, y=-total_light, label=min_to_hhmm(total_light)),
                       color="#78dcff", hjust=1, family="Helvetica")
  gg <- gg + geom_text(aes(x="ep0", y=-300, label="Light", size=24, fontface="bold"),
                       color="#78dcff", hjust=0, family="Helvetica")
  gg <- gg + stat_identity(aes(x=episode, y=total_dark),
                           geom="bar", fill="#f60700", width=0.75)
  gg <- gg + geom_text(aes(x=episode, y=total_dark, label=min_to_hhmm(total_dark)),
                       color="#f60700", hjust=0, family="Helvetica")
  gg <- gg + geom_text(aes(x="ep0", y=300, label="Dark", size=24, fontface="bold"),
                       color="#f60700", hjust=0, family="Helvetica")
  gg <- gg + geom_text(aes(x=episode, y=0, label=rn),
                       color="white", hjust=0.5, family="Helvetica", size=6, fontface="bold")
  gg <- gg + geom_text(aes(x="ep0", y=0, label="Episode"),
                       color="white", hjust=0.5, family="Helvetica", size=6, fontface="bold")
  gg <- gg + scale_x_discrete(expand=c(0,0.2), drop=FALSE)
  gg <- gg + scale_y_continuous(expand=c(0,0), limits=c(-600, 600))
  gg <- gg + coord_flip()
  gg <- gg + labs(x=NULL, y=NULL, title="Good vs. Evil\n")
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(axis.text=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(plot.background=element_rect(fill="black"))
  gg <- gg + theme(panel.background=element_rect(fill="black"))
  gg <- gg + theme(plot.title=element_text(color="white", hjust=0, size=36,
                                           family="Helvetica", face="bold"))
  gg

}

#' strong with which one
#' @export
strong_with_which_one <- function() {

  fc <- force_character()
  fc$ge <- c("good", "good", "good", "good", "good", "good", "good", "evil", "evil", "evil", "evil")

  gg <- ggplot(fc)
  gg <- gg + stat_identity(aes(x=reorder(character, total_force), y=total_force, fill=ge),
                           geom="bar", width=0.75)
  gg <- gg + geom_text(aes(x=reorder(character, total_force), y=-1,
                           label=sprintf("%s  ", character), color=ge), size=6, hjust=1)
  gg <- gg + geom_text(aes(x=reorder(character, total_force), y=total_force,
                           label=min_to_hhmm(total_force), color=ge), size=5, hjust=0)
  gg <- gg + scale_x_discrete(expand=c(0,0))
  gg <- gg + scale_y_continuous(expand=c(0,0), limits=c(-200, 900))
  gg <- gg + scale_color_manual(values=c(good="#78dcff", evil="#f60700"))
  gg <- gg + scale_fill_manual(values=c(good="#78dcff", evil="#f60700"))
  gg <- gg + coord_flip()
  gg <- gg + labs(x=NULL, y=NULL, title="Strong With Which One?\n")
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(axis.text=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(plot.background=element_rect(fill="black"))
  gg <- gg + theme(panel.background=element_rect(fill="black"))
  gg <- gg + theme(plot.title=element_text(color="white", hjust=0, size=36,
                                           family="Helvetica", face="bold"))
  gg


}
