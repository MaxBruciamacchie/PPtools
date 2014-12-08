#' Création d'un périmètre autour des points de placettes
#'
#' @description La fonction un SpatialPolygoneDataFrame contenant les périmètres des forêts.
#' Ils sont construits à l'aide de la fonction gConvexHull du package. Le fichier en entrée est un
#' SpatialPointsDataFrame dont la table attributaire contient au moins 2 champs. Le premier permet de distinguer
#' les dispositifs, le second les placettes
#'
#' @return La fonction renvoie un tableau des types et numéros de tarif Schaeffer par essence.
#'
#' @param TailleBuffer = Taille du buffer en mètre (50m par défaut)
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du fichier au format .shp.
#' Par défaut enreg=TRUE.
#'
#' @import tcltk
#' @import sp
#' @import rgdal
#' @import tools
#' @import rgeos
#' @import gstat
#'
#' @author Bruciamacchie Max
#'
#' @export

CreatePerim <- function(TailleBuffer=50, enreg=T) {
  shp <- tk_choose.files(caption = "Choix du fichier placettes",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  maille <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)))
  maille@data <- maille@data[,1:2]
  ListDisp <- unique(maille@data$NumDisp)
  myPerim <- subset(maille, NumDisp==ListDisp[[1]])
  myPerim <- gConvexHull(myPerim)
  myPerim <- gBuffer(myPerim, width=TailleBuffer)
  myPerim@polygons[[1]]@ID <- as.character(ListDisp[[1]])
  for (i in 2:length(ListDisp)) {
    SP <- subset(maille, NumDisp==ListDisp[[i]])
    SP <- gConvexHull(SP)
    SP <- gBuffer(SP, width=TailleBuffer)
    SP@polygons[[1]]@ID <- as.character(ListDisp[[i]])
    myPerim <- rbind(myPerim, SP)
  }
  tab <- data.frame(NumDisp=ListDisp)
  row.names(tab) <- as.character(ListDisp)
  SPDF <- SpatialPolygonsDataFrame(Sr = myPerim, data = tab)
  writeOGR(SPDF, dirname(shp), "PerimAut", driver="ESRI Shapefile", overwrite_layer=T)
}
