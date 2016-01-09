#' be mentioned
#' @export
be_mentioned <- function() {

  count(filter(gather(setNames(select(mentions(), ends_with("char")),
                               c("I", "II", "III", "IV", "V", "VI")),
                      episode, character),
               character != ""), episode, character) -> ment

  char_col <- c("Jar"="gray", "Mace"="#78dcff", "Mundi"="#78dcff",
                "Qui-Gon"="#78dcff", "Yoda"="#78dcff", "Anakin"="#78dcff",
                "Dooku"="#f60700", "Obi-Wan"="#78dcff", "Palpatine"="#f60700",
                "Dodonna"="gray", "Han"="gray", "Luke"="#78dcff",
                "Vader"="#f60700", "Ackbar"="gray",
                "white"="white", "black"="black")

  ment <- mutate(ment,
                 lab_col=ifelse(character %in% c("Vader", "Palpatine", "Dooku"),
                                "white",
                                "black"))

  gg <- ggplot(ment)
  gg <- gg + geom_hline(yintercept=1, color="white", size=0.15)
  gg <- gg + stat_identity(aes(x=character, y=1, size=n, fill=character),
                           geom="point", color="white", shape=21)
  gg <- gg + geom_text(aes(x=character, y=1, label=n, color=lab_col),
                       size=4)
  gg <- gg + geom_text(aes(x=character, y=1, label=character),
                       color="white", size=2.5, vjust=-4.7)
  gg <- gg + facet_wrap(~episode, ncol=1)
  gg <- gg + scale_x_discrete(expand=c(0,0.5))
  gg <- gg + scale_y_continuous(expand=c(0,1))
  gg <- gg + scale_color_manual(values=char_col)
  gg <- gg + scale_fill_manual(values=char_col)
  gg <- gg + scale_size(range=c(5, 18))
  gg <- gg + labs(x=NULL, y=NULL, title="May the Force Be Mentioned\n")
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(axis.text.x=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(strip.text=element_text(color="white", hjust=0, size=10))
  gg <- gg + theme(plot.background=element_rect(fill="black"))
  gg <- gg + theme(panel.background=element_rect(fill="black"))
  gg <- gg + theme(plot.title=element_text(color="white", hjust=0, size=36,
                                           family="Helvetica", face="bold"))
  gg


}
