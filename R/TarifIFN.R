#' Tarif de cubage volume géométrique bois fort
#'
#' @description La fonction renvoie un tableau des types et numéros de tarif Schaeffer par essence
#' en utilisant la base de donées arbres de l'IFN. Il s'agit d'un volume géométrique bois fort tige.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu : forêt, massif,
#' sylvoécorégion, etc.
#'
#' @return La fonction renvoie un tableau des types et numéros de tarif Schaeffer par essence.
#'
#' @param TailleBuffer = Taille du buffer en mètre.
#' @param SeuilCircf = seuil minimal de circonférence en cm en dessous duquel l'arbre n'est pas retenu
#' pour le calcul du tarif.
#' @param SeuilNb = seuil minimal du nombre d'arbres dans la base IFN pour qu'une essence soit retenue.
#' @param res = résolution (par défaut = 1000 m)
#' @param UseSer = argument permettant de choisir si le calcul des numéros de tarif doit se faire
#' au sein d'une même sylvoécorégion. Par défaut, UseSer=TRUE.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .xlsx.
#' Par défaut enreg=False.
#'
#' @import doBy
#' @import rgdal
#' @import tools
#' @import tcltk
#' @import sp
#' @import rgeos
#' @import raster
#' @import gstat
#' @import openxlsx
#' @import raster
#'
#' @author Bruciamacchie Max
#'
#' @export

TarifIFN <- function(TailleBuffer=30000, SeuilCircf=50, SeuilNb=10, res=1000, UseSer=T, enreg=F) {
#   data(IFNBaseArbres)
#   Tarifs <- IFNBaseArbres
  shp <- tk_choose.files(caption = "Choix du périmètre",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  perimetre <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)))
  perimetre <- spTransform(perimetre, CRS("+init=epsg:2154"))
  zone <- gBuffer(perimetre, width=TailleBuffer)
  if (UseSer) {
    zone <- gIntersection(zone, ser, byid = T)
    List <- gIntersects(perimetre, zone, byid = T)
    zone <- zone[List[,1],]
  }
  List <- gIntersects(zone, IFNBaseArbres, byid = T)
  Tarifs <- IFNBaseArbres[List[,1],]
  print("Extraction dans la base IFN")
  t <- Tarifs@data[which(Tarifs@data$c13 >=SeuilCircf), "espar"]
  df <- data.frame(table(t))
  df <- df[which(df$Freq > SeuilNb),]
  df <- merge(df,CodesEssIFN, by.x="t", by.y="code", all.x=T)
  df <- df[order(-df$Freq),]
  names(df)[1] <- "Code"
  print("Liste des essences")
  Tab <- data.frame()
        # -------------# Création grid #--------------
  BB <- bbox(zone)
  BB <- res*(round(BB/res)) ## Arrondi
  x.range <- as.integer(range(BB[1,]))
  y.range <- as.integer(range(BB[2,]))
  grd <- expand.grid(x=seq(from=x.range[1]-2*res, to=x.range[2]+2*res, by=res),
                     y=seq(from=y.range[1]-2*res, to=y.range[2]+2*res, by=res))
  coordinates(grd) <- ~ x+y
  gridded(grd) <- TRUE
  grd@proj4string <- CRS("+init=epsg:2154")
  print("grid créé")
  for (i in 1:dim(df)[1]) {
    # -------------# Extraction essence #--------------
    print(paste0("Traitement de l'essence : ",df[i,3]))
    b <- subset(Tarifs, espar==paste(df[i,1],"",sep="") & c13 >=SeuilCircf,
                select=c("numSchR","numSchI","numSchL","numSchTL","c13"))
    if (length(unique(b@coords))<3) {
      t1 <- data.frame(Moyenne = apply(b@data[,1:4], 2, mean, na.rm=T),
                       Variance = apply(b@data[,1:4], 2, sd, na.rm=T))
      row.names(t1) <- c("SchR","SchI","SchL","SchTL")
      t2 <- data.frame(Code = df[i,1], Nbre = df[i,2], Essence = df[i,3],
                       Type = rownames(t1)[which.min(t1[,2])],
                       Num  = floor(t1[which.min(t1[,2]),1]/0.5+0.5)*0.5)
      Tab <- rbind(Tab,t2)
    } else {
      # -------------# moyenne sur un pas de 500m #--------------
      t <- cbind(b@data,b@coords)
      t$x <- floor(t$xl93/500+0.5)*500
      t$y <- floor(t$yl93/500+0.5)*500
      t <- summaryBy(numSchR + numSchI + numSchL + numSchTL ~ x + y, data=t, FUN=mean, keep.names=T)
      coordinates(t) <- ~ x + y
      t@proj4string <- CRS("+init=epsg:2154")
      # -------------# krigeage pour chacun des tarifs #--------------
      for (j in 1:4) {
        v <- variogram(t@data[,j] ~ 1, data=t, cutoff = TailleBuffer, width = 1000)
        vm <- vgm(psill = 3, model = "Sph", range = 10000, nugget = 1)
        vmf <- fit.variogram(v, vm)
        plot(v, pl = T, model = vmf)
        k <- krige(t@data[,j] ~ 1, locations = t, newdata = grd, model = vmf)
        if (j==1) {
          List1 <- list(raster(k[,1]))
          List2 <- list(raster(k[,2]))
        } else {
          List1 <- c(List1, list(raster(k[,1])))
          List2 <- c(List2, list(raster(k[,2])))
        }
      }
      b1 <- brick(List1)
      names(b1) <- c("SchR","SchI","SchL","SchTL")
      b2 <- brick(List2)
      names(b2) <- c("SchR","SchI","SchL","SchTL")
      # -------------# Extraction par essence #--------------
      v1 <- mask(b1, perimetre)
      v2 <- mask(b2, perimetre)
      t1 <- data.frame(Moyenne = apply(v1@data@values, 2, mean, na.rm=T),
                       Variance = apply(v2@data@values, 2, sd, na.rm=T))

      t2 <- data.frame(Code = df[i,1], Nbre = df[i,2], Essence = df[i,3],
                       Type = rownames(t1)[which.min(t1[,2])],
                       Num  = floor(t1[which.min(t1[,2]),1]/0.5+0.5)*0.5)
      Tab <- rbind(Tab,t2)
    }
  }
  if (enreg) {
    wb <- createWorkbook()
    addWorksheet(wb, "data")
    writeData(wb, "data", Tab)
    dir.create("Out", showWarnings = F)
    saveWorkbook(wb, "tarifIFN.xlsx", overwrite = TRUE)
  }
  return(Tab)
}


