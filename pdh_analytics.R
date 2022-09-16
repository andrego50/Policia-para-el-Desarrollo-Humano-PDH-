library(sf)
library(ks)
library(mapview)
library(dplyr)
library(readr)

municipios <- st_read("mun_13_02_2018/Municipios.shp")
municipios <- st_as_sf(municipios, crs = 4326)
unidad <- municipios[municipios$NOM_DEPART == 'TOLIMA',]
unidad <- unidad[unidad$NOM_MUNICI %in% c('IBAGUÉ', 'CAJAMARCA',
                                       'PIEDRAS', 'ALVARADO'),]
unidad <- st_transform(unidad, 4326) 

grid <- st_make_grid(unidad, cellsize = 0.01/5)
grid_kde <- do.call(rbind, st_centroid(grid)) %>% 
  as_tibble()

delitos <- read_csv("~/Downloads/delitos.csv", 
                    locale = locale(decimal_mark = ","))

delitos <- delitos[delitos$LONGITUD < -60 & delitos$LATITUD > 4,]

unique(delitos$DESCRIPCION_CONDUCTA)
View(table(delitos$DESCRIPCION_CONDUCTA))

vida_homicidio <- c("ARTÍCULO 103. HOMICIDIO", "ARTÍCULO 104A. FEMINICIDIO")

vida_lesiones <- c("ARTÍCULO 111. LESIONES PERSONALES", "ARTICULO 120 LESIONES CULPOSAS", "ARTÍCULO 119. LESIONES PERSONALES ( CIRCUNSTANCIAS DE AGRAVACIÓN)",
                   "ARTÍCULO 113. DEFORMIDAD", "ARTÍCULO 116. PÉRDIDA ANATÓMICA O FUNCIONAL DE UN ÓRGANO O MIEMBRO", "ARTÍCULO 229. VIOLENCIA INTRAFAMILIAR")

vida_sexual <- c("ARTÍCULO 206. ACTO SEXUAL VIOLENTO", "ARTÍCULO 205. ACCESO CARNAL VIOLENTO", "ARTÍCULO 210. ACCESO CARNAL O ACTO SEXUAL ABUSIVO CON INCAPAZ DE RESISTIR",
                 "ARTÍCULO 210 A. ACOSO SEXUAL", "ARTÍCULO 207. ACCESO CARNAL O ACTO SEXUAL EN PERSONA PUESTA EN INCAPACIDAD DE RESISTIR", "ARTÍCULO 213. INDUCCIÓN A LA PROSTITUCIÓN",
                 "ARTÍCULO 214. CONSTREÑIMIENTO A LA PROSTITUCIÓN")

vida_sexual_nna <- c("ARTÍCULO 208. ACCESO CARNAL ABUSIVO CON MENOR DE 14 AÑOS", "ARTÍCULO 209. ACTOS SEXUALES CON MENOR DE 14 AÑOS",
              "ARTÍCULO 218. PORNOGRAFÍA CON MENORES", "ARTÍCULO 219 A. UTILIZACIÓN O FACILITACIÓN DE MEDIOS DE COMUNICACIÓN PARA OFRECER SERVICIOS SEXUALES DE MENORES",
              "ARTÍCULO 217 A. DEMANDA DE EXPLOTACION SEXUAL COMERCIAL DE PERSONA MENOR DE 18 AÑOS DE EDAD", "ARTÍCULO 213 A. PROXENETISMO CON MENOR DE EDAD",
              "ARTÍCULO 126. LESIONE CULPOSAS AL FETO", "ARTÍCULO 217. ESTÍMULO A LA PROSTITUCIÓN DE MENORES")

ingreso_extorsion <- c("ARTÍCULO 244. EXTORSIÓN")

ingreso_hurto <- c("ARTÍCULO 239. HURTO PERSONAS", "ARTÍCULO 239. HURTO ENTIDADES COMERCIALES", "ARTÍCULO 239. HURTO RESIDENCIAS",
             "ARTÍCULO 239. HURTO AUTOMOTORES", "ARTÍCULO 239. HURTO PIRATERÍA TERRESTRE", "ARTÍCULO 239. HURTO ENTIDADES FINANCIERAS", 
             "ARTÍCULO 243. ABIGEATO")

dimension <- ingreso_hurto
  
multicrimen_ubano <- subset(delitos, 
                      delitos$DESCRIPCION_CONDUCTA %in% dimension &
                        delitos$ZONA == 'URBANA')
multicrimen_rural <- subset(delitos, 
                      delitos$DESCRIPCION_CONDUCTA %in% dimension &
                        delitos$ZONA == 'RURAL')

coord_urbana <- multicrimen_ubano[,c('LONGITUD','LATITUD')]
coord_rural  <- multicrimen_rural[,c('LONGITUD','LATITUD')]

kd_urbana <- kde(coord_urbana, Hpi(coord_urbana, pilot="dscalar"), eval.points = grid_kde)
kd_rural <- kde(coord_rural, Hpi(coord_rural, pilot="dscalar"), eval.points = grid_kde)

kde_urbana <- data.frame(x = kd_urbana[["eval.points"]][[1]], 
                  y = kd_urbana[["eval.points"]][[2]],
                  densidad = kd_urbana[["estimate"]])

kde_rural <- data.frame(x = kd_rural[["eval.points"]][[1]], 
                  y = kd_rural[["eval.points"]][[2]],
                  densidad = kd_rural[["estimate"]])

percentil_urbana <- quantile(kde_urbana$densidad, probs = 0.999)[[1]]
percentil_rural <- quantile(kde_rural$densidad, probs = 0.999)[[1]]

microterritorio_urbano <- kde_urbana[kde_urbana$densidad > percentil_urbana,]
microterritorio_rural <- kde_rural[kde_rural$densidad > percentil_rural,]

microterritorio_urbano <- st_as_sf(microterritorio_urbano,
                                  coords = c('x','y'), 
                                  crs = 4326)
microterritorio_rural <- st_as_sf(microterritorio_rural,
                             coords = c('x','y'), 
                             crs = 4326)

mapview(list(microterritorio_urbano, microterritorio_rural),
        legend = F, color = 'blue') 
  