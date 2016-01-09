#' ebb and flow
#' @export
ebb_and_flow <- function() {

  ep <- c("X1"="I", "X2"="II", "X3"="III", "X4"="IV", "X5"="V", "X6"="VI")
  cols <- c("start", "end", "character", "ability", "episode")

  tl <- timeline()

  I <- select(tl, starts_with("X1"))
  II <- select(tl, starts_with("X2"))
  III <- select(tl, starts_with("X3"))
  IV <- select(tl, starts_with("X4"))
  V <- select(tl, starts_with("X5"))
  VI <- select(tl, starts_with("X6"))

  bind_rows(
    setNames(mutate(I[complete.cases(I),], episode="I"), cols),
    setNames(mutate(II[complete.cases(II),], episode="II"), cols),
    setNames(mutate(III[complete.cases(III),], episode="III"), cols),
    setNames(mutate(IV[complete.cases(IV),], episode="IV"), cols),
    setNames(mutate(V[complete.cases(V),], episode="V"), cols),
    setNames(mutate(VI[complete.cases(VI),], episode="VI"), cols)
  ) -> flow

  lt <- c("Obi-Wan Kenobi", "Qui-Gon Jinn", "Anakin Skywalker",
          "Other \\(Jedi\\)", "Yoda", "Other \\(Younglings\\)",
          "Other \\(Youngling\\)", "Mace Windu", "Luke Skywalker",
          "Princess Leia Organa")
  dk <- c("Darth Maul", "Count Dooku", "Palpatine", "Darth Vader")

  flow$character <- gsub(sprintf("(%s)", paste(lt, collapse="|")),
                         "light", flow$character)
  flow$character <- gsub(sprintf("(%s)", paste(dk, collapse="|")),
                         "dark", flow$character)
  flow$character <- sapply(str_split(flow$character, "\\|"), unique)

  ch <- c("light"="#78dcff", "dark"="#f60700")

  flow$ymax <- ifelse(flow$character == "dark", -1, 1)

  gg <- ggplot(flow)
  gg <- gg + geom_rect(aes(xmin=start, xmax=end, ymin=0, ymax=ymax,
                           fill=character), color="#00000000", size=0.1)
  gg <- gg + scale_y_continuous(expand=c(0,0))
  gg <- gg + scale_fill_manual(values=ch)
  gg <- gg + facet_wrap(~episode, ncol=1)
  gg <- gg + labs(x=NULL, y=NULL, title="May the Force Be Mentioned\n")
  gg <- gg + theme_tufte(base_family="Helvetica")
  gg <- gg + theme(axis.text.x=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(strip.text=element_text(color="white", hjust=0, size=10))
  gg <- gg + theme(plot.background=element_rect(fill="black"))
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.background=element_rect(color="#252525",
                                                 fill="#252525"))
  gg <- gg + theme(plot.title=element_text(color="white", hjust=0, size=36,
                                           family="Helvetica", face="bold"))
  gg

}