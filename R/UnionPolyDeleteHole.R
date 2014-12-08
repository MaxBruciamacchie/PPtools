#' Union des différents polygones d'un shape et suppression des trous.
#' @description La fonction utilise en entrée un shape de type surfacique.
#' Elle aggrège tous les polygones en un seul, et supprime les éventuels trous.
#' @return La fonction construit le shape PerimCor.shp dans le même dossier.
#' @import rgeos
#' @import rgdal
#' @import tools
#' @import tcltk
#' @import sp
#' @author Bruciamacchie Max
#' @export
#'
UnionPolyDeleteHole <-function() {
  shp <- tk_choose.files(caption = "Choix du périmètre",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  perim <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)))
  SP <- gUnaryUnion(perim)
  trous <- sapply(SP@polygons[[1]]@Polygons, function(x) x@hole)
  Poly <- list()
  for(i in 1:length(trous)){
    if(!trous[[i]]) Poly <- c(Poly, SP@polygons[[1]]@Polygons[[i]])
  }
  SP1 <- SpatialPolygons(Srl = list(Polygons(srl = Poly, ID="global")))
  tab <- data.frame(Var="Perim")
  row.names(tab) <- "global"
  SPDF <- SpatialPolygonsDataFrame(Sr = SP1, data = tab)
  SPDF@proj4string <- perim@proj4string
  writeOGR(SPDF, dirname(shp), "PerimCor", driver="ESRI Shapefile", overwrite_layer=T)
}

DeleteHole <-function() {
  shp <- tk_choose.files(caption = "Choix du périmètre",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  Polys <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)))
  ListPoly <- slot(Polys, "polygons")
  holes <- lapply(ListPoly, function(x) sapply(slot(x, "Polygons"), slot, "hole"))
  res <- lapply(1:length(ListPoly), function(i) slot(ListPoly[[i]], "Polygons")[!holes[[i]]])
  IDs <- row.names(Polys@data)
  SansTrou <- SpatialPolygons(lapply(1:length(res), function(i)
   Polygons(res[[i]], ID=IDs[i])), proj4string=CRS(proj4string(Polys)))
}
