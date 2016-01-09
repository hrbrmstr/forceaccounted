#' strong with which one
#' @export
strong_with_which_one <- function() {

  fc <- force_character()
  fc$ge <- c("good", "good", "good", "good", "good", "good", "good",
             "evil", "evil", "evil", "evil")

  gg <- ggplot(fc)
  gg <- gg + stat_identity(aes(x=reorder(character, total_force),
                               y=total_force, fill=ge),
                           geom="bar", width=0.75)
  gg <- gg + geom_text(aes(x=reorder(character, total_force), y=-1,
                           label=sprintf("%s  ", character), color=ge),
                       size=6, hjust=1)
  gg <- gg + geom_text(aes(x=reorder(character, total_force), y=total_force,
                           label=min_to_hhmm(total_force), color=ge),
                       size=5, hjust=0)
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
