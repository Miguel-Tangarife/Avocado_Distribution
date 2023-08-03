#Modelo con los pesos que asigna el maxent
#Modelo con los pesos de la literatura




install.packages("dplyr")
install.packages("rasterVis")
library(dplyr)
library(rasterVis)
library(raster)
library(readr)
library(ggplot2)
Persea_americana_ocurrences <- read_excel("Persea_americana_ocurrences.xlsx")
data <- Persea_americana_ocurrences[, c("species", "publishi_1", "decimalLat", "decimalLon")]
write.csv(data, file = "Persea_americana_ocurrences_filtered.csv", row.names = FALSE)

Persea_americana_ocurrences_filtered <- read_csv("Persea_americana_ocurrences_filtered.csv")
class(Persea_americana_ocurrences_filtered)
colnames(Persea_americana_ocurrences_filtered) <- Persea_americana_ocurrences_filtered[1,]
Persea_americana_ocurrences_filtered$publishi_1 <- gsub("\"", "", Persea_americana_ocurrences_filtered$publishi_1)

dataframe_filtrado <- subset(Persea_americana_ocurrences_filtered, publishi_1 == "CO")
write.csv(dataframe_filtrado, file = "Persea_americana_ocurrences_COfiltered.csv", row.names = FALSE)

MXN_data <- dataframe_filtrado[,-2]
colnames(MXN_data) <- c("Especie", "Lat", "Lon")
MXN_data[,2] <- as.numeric(MXN_data[,2])
MXN_data[,3] <- as.numeric(MXN_data[,3])
write.csv(MXN_data, file = "MXN_data.csv", row.names = FALSE)

#Data Mexico
datamexi <- ocurrences_mexi[, c("species", "publishi_1", "decimalLat", "decimalLon")]
datamexi$publishi_1 <- gsub("\"", "", datamexi$publishi_1)
datamexi <- subset(datamexi, publishi_1 == "MX")

as.matrix(MXN_datamexi)
class(MXN_datamexi)
MXN_datamexi <- datamexi[,-2]
colnames(MXN_datamexi) <- c("Especie", "Lat", "Lon")
MXN_datamexi[,2] <- as.numeric(MXN_datamexi[,2])
MXN_datamexi[,3] <- as.numeric(MXN_datamexi[,3])
write.csv(MXN_datamexi, file = "MXN_datamexi.csv", row.names = FALSE)

#Cargar rasters de variables

meantemp <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\wc2.1_30s_bio\\wc2.1_30s_bio_1.tif")
meanprec <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\wc2.1_30s_bio\\wc2.1_30s_bio_12.tif")
elev <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\wc2.1_30s_elev\\wc2.1_30s_elev.tif") 
drange <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\wc2.1_30s_bio\\wc2.1_30s_bio_2.tif")
slope <- terrain(elev, opt="slope", unit="degrees")
wokb <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\sq7.asc")

shape_AP_CO <- shapefile("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\Parques_Nacionales_Naturales_de_Colombia\\Parques_Nacionales_Naturales_de_Colombia.shp")
shape_AP_CO <- rasterize(shape_AP_CO, meantemp, field = 1)
shape_AP_MX <- shapefile("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\capas\\Shapefile - Censo 2010 (Estatal)\\inegi_refcenesta_2010.shp")
shape_AP_MX <- rasterize(shape_AP_MX, meantemp, field = 1)
plot(shape_AP_CO)
#Recortar los raster colombia
colombia_extent <- extent(-79.024626, -66.876325, -4.225, 12.448347) # Colombia: Oeste, Este, Sur, Norte
plot(colombia_extent)
meantemp_colombia <- crop(meantemp, colombia_extent)
meanprec_colombia <- crop(meanprec, colombia_extent)
elev_colombia <- crop(elev, colombia_extent)
drange_colombia <- crop(drange, colombia_extent)
slope_colombia <- crop(slope, colombia_extent)
wokb_colombia <- crop(wokb, colombia_extent)
wokb_colombia <- resample(wokb_colombia, meantemp_colombia, method = "bilinear")
plot(wokb_colombia)
#cambiar formato para la lectura de MXNT colombia (formato ascii)
writeRaster(meantemp_colombia, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\resahpe_data\\meantemp_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(meanprec_colombia, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\resahpe_data\\meanprec_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(elev_colombia, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\resahpe_data\\elev_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(drange_colombia, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\resahpe_data\\drange_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(slope_colombia, filename = "slope_colombia.asc", format = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\resahpe_data\\slope_new.asc", overwrite=TRUE)
writeRaster(wokb_colombia, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\resahpe_data\\workb_new.asc", format = "ascii", overwrite=TRUE)



#Crop de los raster para agilizar calculo del mask mexico
Mex_extent <- extent(-118.0, -80, 10, 33.0) # Mexico: Oeste, Este, Sur, Norte
meantemp_crop <- crop(meantemp, Mex_extent)
plot(meantemp_crop)
meanprec_crop <- crop(meanprec, Mex_extent)
elev_crop <- crop(elev, Mex_extent)
drange_crop <- crop(drange, Mex_extent)
slope_crop <- crop(slope, Mex_extent)
workb_crop <- crop(wokb, Mex_extent)
workb_crop <- resample(workb_crop, meantemp_crop, method="ngb")

#Recortar los raster mexico (mask)
mexico_shp <- shapefile("C:\\Users\\Juan G\\Downloads\\Shapefile - Censo 2010 (Estatal)\\inegi_refcenesta_2010.shp")
mexico_raster <- raster(meantemp_crop) # Crear un raster vacío con la misma 
mexico_raster[] <- NA                 #extensión y resolución que el raster a recortar
mexico_raster <- rasterize(mexico_shp, mexico_raster, field = 1) # Rasterizar el shapefile de México, asignando valor 1 a las celdas dentro de los límites
mexico_raster[mexico_raster != 1] <- NA # Convertir los valores distintos de 1 a NA, dejando solo el área de México con valores 1

# Aplicar la función mask para recortar los raster
meantemp_mexi <- mask(meantemp_crop, mexico_raster)
meanprec_mexi <- mask(meanprec_crop, mexico_raster)
elev_mexi <- mask(elev_crop, mexico_raster)
drange_mexi <- mask(drange_crop, mexico_raster)
slope_mexi <- mask(slope_crop, mexico_raster)
workb_mexi <- mask(workb_crop, mexico_raster)

#cambiar formato para la lectura de MXNT (formato ascii)
writeRaster(meantemp_mexi, filename = "meantemp_mexi.asc", format = "ascii", overwrite=TRUE)
writeRaster(meanprec_mexi, filename = "meanprec_mexi.asc", format = "ascii", overwrite=TRUE)
writeRaster(elev_mexi, filename = "elev_mexi.asc", format = "ascii", overwrite=TRUE)
writeRaster(drange_mexi, filename = "drange_mexi.asc", format = "ascii", overwrite=TRUE)
writeRaster(slope_mexi, filename = "slope_mexi.asc", format = "ascii", overwrite=TRUE)
writeRaster(workb_mexi, filename = "workb_mexi.asc", format = "ascii", overwrite=TRUE)

#recortar los raster para adaptarse a ambos paises
CO_MX_extent <- extent(-118.0, -60, -4.23, 33.0)  # Colombia: Oeste, Este, Sur, Norte
meantempCOMX <- crop(meantemp, CO_MX_extent)
plot(meantempCOMX)
meanprecCOMX <- crop(meanprec, CO_MX_extent)
elevCOMX <- crop(elev, CO_MX_extent)
drangeCOMX <- crop(drange, CO_MX_extent)
slopeCOMX <- crop(slope, CO_MX_extent)
wokbCOMX <- crop(wokb, CO_MX_extent)
res(meantempCOMX)
res(meanprecCOMX)
res(drangeCOMX)
res(slopeCOMX)

writeRaster(meantempCOMX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\meantemp.asc", format = "ascii", overwrite=TRUE)
writeRaster(meanprecCOMX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\meanprec.asc", format = "ascii", overwrite=TRUE)
writeRaster(elevCOMX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\elev.asc", format = "ascii", overwrite=TRUE)
writeRaster(drangeCOMX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\drange.asc", format = "ascii", overwrite=TRUE)
writeRaster(slopeCOMX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\slope.asc", format = "ascii", overwrite=TRUE)
writeRaster(wokbCOMX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\workb.asc", format = "ascii", overwrite=TRUE)

meantemp <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\meantemp.asc")
meanprec <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\meanprec.asc")
elev <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\elev.asc") 
drange <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\drange.asc")
slope <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\slope.asc")
wokb <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\workb.asc")
wokb <- resample(wokb, drange, method = "bilinear")
help("raster:resample")
plot(meantemp)
res(drange)
res(wokb)

writeRaster(meantemp, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\meantemp_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(meanprec, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\meanprec_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(elev, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\elev_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(drange, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\drange_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(slope, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\slope_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(wokb, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\workb_new.asc", format = "ascii", overwrite=TRUE)
writeRaster(shape_AP_CO, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\parques_col.asc", format = "ascii", overwrite=TRUE)
writeRaster(shape_AP_MX, filename = "C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\CO_MX_data\\parques_mex.asc", format = "ascii", overwrite=TRUE)


#carreteras
shape_ways <- shapefile("C:\\Users\\Juan G\\Downloads\\889463674641_s\\conjunto_de_datos\\red_vial.shp")
shape_AP_CO <- rasterize(shape_AP_CO, meantemp, field = 1)

#plot del raster 
# Convertir el raster a un data frame
datos <- raster("C:\\Users\\Juan G\\Desktop\\Proyecto_SIG\\MXNT_data\\run2\\Persea_americana.asc")

# Plotear el raster con levelplot
levelplot(datos, 
          col.regions = viridis::viridis(100),  # Puedes cambiar la paleta de colores aquí
          main = "Título del gráfico",
          xlab = "Etiqueta eje X",
          ylab = "Etiqueta eje Y")






