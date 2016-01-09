#' applied force
#' @export
applied_force <- function() {

  ge <- c("Anakin"="light", "Luke"="light", "Leia"="light",
          "Obi.Wan"="light", "Qui.Gon"="light", "Yoda"="light",
          "Mace"="light", "Palpatine"="dark", "Maul"="dark",
          "Dooku"="dark", "Vader"="dark")

  ft <- mutate(gather(force_type(), who, count, -type),
               side=unname(ge[who]),
               side=factor(side, levels=c("light", "dark")),
               type=factor(type, levels=unique(type)))

  fts <- c("Force Leap", "Sense", "Telekinesis", "Force Push",
           "Force Lightning", "Jedi Mind Trick", "Force Spirit",
           "Telepathy", "Force Choke", "Burst of Speed")

  plts <- lapply(fts, ft_gg, ft)

  marrangeGrob(plts, nrow=length(fts), ncol=1,
               padding = unit(0, "null"), top=NULL)

}

ft_gg <- function(force_type, ft) {

  ft <- filter(ft, type==force_type)
  ft <- mutate(ft,
               type=factor(type, c(force_type, "zzz")),
               lab_col=ifelse(side=="dark", "black", "white"))

  ft_tot <- count(ft, type, side, wt=count)

  gg <- ggplot(ft)
  gg <- gg + stat_identity(aes(x=type, y=count,
                               group=type, fill=side),
                           geom="bar", position="stack",
                           color="black", width=0.75)
  gg <- gg + geom_text(aes(x="zzz", y=0, label=force_type,
                           color=lab_col),
                           size=8, hjust=0)
  gg <- gg + geom_text(data=ft_tot,
                       aes(x=type, y=n, label=sprintf(" %d", n),
                           color=side),
                       hjust=0, size=6)
  gg <- gg + scale_y_continuous(expand=c(0,0), limits=c(0,150))
  gg <- gg + scale_x_discrete(expand=c(0,0.5), drop=FALSE)
  gg <- gg + scale_color_manual(values=c(light="#78dcff",
                                        dark="#f60700",
                                        black="black",
                                        white="white"))
  gg <- gg + scale_fill_manual(values=c(light="#78dcff",
                                        dark="#f60700",
                                        black="black",
                                        white="white"))
  gg <- gg + coord_flip()
  gg <- gg + facet_wrap(type~side, ncol=2)
  gg <- gg + labs(x=NULL, y=NULL, title=NULL)
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(axis.text=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(strip.text=element_blank())
  gg <- gg + theme(strip.background=element_blank())
  gg <- gg + theme(panel.margin=unit(c(0,0,0,0), "null"))
  gg <- gg + theme(panel.background=element_rect(color="black",
                                                 fill="black"))
  gg <- gg + theme(plot.background=element_rect(color="black",
                                                fill="black"))
  gg <- gg + theme(plot.title=element_text(color="white",
                                           hjust=0, size=36,
                                           family="Helvetica",
                                           face="bold"))
  gg
}
