#' Création d'un réseau de placettes permanentes
#' @description La fonction construit un réseau de placettes permanentes en fonction
#' du périmètre de la zone d'étude, du nombre de placettes souhaité et de la forme du réseau
#' @return La fonction construit le shape Maille.shp dans un dossier choisi par l'utilisateur.
#' @param Nb = nombre souhaité de placettes
#' @param Type = type de maille souhaitée. Les options possibles sont : random, regular
#' (option par défaut), stratified, nonaligned, hexagonal
#' @param Objet = FALSE par défaut. Si True la fonction retourne la grille de points
#' @import rgdal
#' @import tools
#' @import tcltk
#' @import sp
#' @author Bruciamacchie Max
#' @export
#'
CreateNetPP <- function(Nb, Type="regular", Objet=F) {
  shp <- tk_choose.files(caption = "Choix du périmètre",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  perim <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)))
  Plac <- spsample(perim, Nb, Type)
  tab <- data.frame(Num=1:dim(Plac@coords)[1])
  SPDF <- SpatialPointsDataFrame(Plac, data=tab)
  writeOGR(SPDF, dirname(shp), "Maille", driver="ESRI Shapefile", overwrite_layer=T)
  if (Objet) return(SPDF)
}
