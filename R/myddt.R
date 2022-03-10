#' @title MyDDT
#'
#' @description Project 1: myddt function
#'
#' @param df the data frame
#' @param SPECIES desired species
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 ggplot geom_point geom_smooth ggtitle aes_string
#'
#' @return a plot, a csv file, and 3 command-line outputs
#' @export
#'
#' @examples
#' \dontrun{myddt(df = ddt, species = "CCATFISH")}
#'

myddt <- function(df, SPECIES){

  print(df)
  tab = table(df$RIVER) / length(df$RIVER)
  print(tab)
  df1 <- df %>% filter(SPECIES == {{SPECIES}})
  print(df1)
  if ({{SPECIES}} == "CCATFISH"){
    write.csv(x = df1, "LvsWforCCATFISH.csv", row.names = FALSE)
  }
  if ({{SPECIES}} == "SMBUFFALO"){
    write.csv(x = df1, "LvsWforSMBUFFALO.csv", row.names = FALSE)
  }
  if ({{SPECIES}} == "LMBASS"){
    write.csv(x = df1, "LvsWforLMBASS.csv", row.names = FALSE)
  }
  g <- ggplot(df1, aes_string(x = "WEIGHT", y = "LENGTH")) +
    geom_point(aes_string(color = "RIVER")) +
    geom_smooth(formula = y~x + I(x^2), method = "lm") +
    ggtitle("Gavin Short")
  print(g)
}
