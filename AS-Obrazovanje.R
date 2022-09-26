require(openxlsx)
read.xlsx("AS-Obrazovanje.xlsx")
ASobrazovanje <- read.csv("AS-Obrazovanje.csv")
ASobrazovanje$Pol <- as.factor(ASobrazovanje$Pol)
levels(ASobrazovanje$Pol) <- c("muškarci", "žene")

ASobrazovanje$Obrazovanje <- as.factor(ASobrazovanje$Obrazovanje)
#ASobrazovanje$Godina <- as.factor(ASobrazovanje$Godina)
ASobrazovanje$Tip.naselja <- as.factor(ASobrazovanje$Tip.naselja)
levels(ASobrazovanje$Tip.naselja) <-
  c("Gradska naselja", "Ostala naselja")

ASgg <- ggplot(data = ASobrazovanje, aes(
  x = Godina,
  y = Vrednost,
  text = sprintf(
    "%s. god. je <b>%s%%</b> %s iz %s\n%s ",
    Godina,
    format(Vrednost, decimal.mark=",",digits=2),
    ifelse(as.character(Pol) == "muškarci", "mušakraca", "žena"),
    ifelse(
      as.character(Tip.naselja) == "Gradska naselja",
      "gradskih naselja",
      "ostalih naselja"
    ),
    ifelse(
      as.character(Obrazovanje) == "Bez školske spreme",
      "bilo bez školske spreme",
      ifelse(
        as.character(Obrazovanje) == "Niže od osnovnog",
        "imalo niže obrazovanje od osnovnog",
        ifelse(
          as.character(Obrazovanje) == "Osnovno obrazovanje",
          "imalo osnovno obrazovanje",
          ifelse(
            as.character(Obrazovanje) == "Srednje obrazovanje",
            "imalo srednje obrazovanje",
            "imalo tercijarno obrazovanje"
          )
        )
      )
    )
  )
)) +
  geom_line(aes(group = Pol, col = Pol)) +
  facet_grid(Tip.naselja ~ Obrazovanje) +
  scale_color_manual(values = c("#377eb8", "#e41a1c")) +
  scale_y_continuous(
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_x_continuous(breaks = c(1981, 1991, 2002, 2011)) +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL)

ASplotly <- ggplotly(ASgg,tooltip = "text")
ASplotly
