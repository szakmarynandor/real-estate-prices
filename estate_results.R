### Ingatlan területi panel modell (Panel SAR with fixed effects)
# Készítette: Szakmáry Nándor

#csomagok betöltése
library(plm)
library(splm)
library(car)
library(tidyverse)
library(readxl)
library(ggcorrplot)
library(spdep)
library(sp)

#adatok importálása
panel_data <- read_xlsx("Lakásár_területi_panel.xlsx",
                                sheet = "panel_data", col_names = TRUE)
panel_data$jaras <- as.factor(panel_data$jaras)

Coords <- read_xlsx("Lakásár_területi_panel.xlsx",
                            sheet = "koordináták", col_names = TRUE)

df_2016 <- read_xlsx("Lakásár_területi_panel.xlsx",
                     sheet = "2016", col_names = TRUE)

df_2017 <- read_xlsx("Lakásár_területi_panel.xlsx",
                     sheet = "2017", col_names = TRUE)

df_2018 <- read_xlsx("Lakásár_területi_panel.xlsx",
                     sheet = "2018", col_names = TRUE)

df_2019 <- read_xlsx("Lakásár_területi_panel.xlsx",
                     sheet = "2019", col_names = TRUE)

df_2020 <- read_xlsx("Lakásár_területi_panel.xlsx",
                     sheet = "2020", col_names = TRUE)

panel_data$jaras <- as.factor(panel_data$jaras)

#leíró statisztikák
psych::describe(panel_data)
summary(panel_data)

psych::describe(df_2016[,3:5])

mean(df_2020$tranz_hasznalt)
median(df_2016$tranz_hasznalt)
sd(df_2020$tranz_hasznalt)

#változók eloszlásainak és kapcsolatainak ábrázolása
GGally::ggpairs(panel_data[,c(3:4,7:17, 19)],
                lower=list(continuous="points", mapping=aes(color="red")),
                diag=list(continuous="densityDiag",mapping=aes(color="red")))


is.pbalanced(panel_data) #kiegyensúlyozott panelstruktúrájú az adatfájl

#korrelációk
seged <- panel_data[,c(3:4, 7:9, 14:17, 19)]
mn_corr <- cor(seged)

ggcorrplot( corr = mn_corr,
            colors = c( "#4472C4", "white", "#B44137" ),
            lab = T, lab_size = 2.5,
            p.mat = cor_pmat( seged ),
            insig = "blank" )+
  theme(text=element_text(family="serif"))

#szja és ingatlanár kapcsolata

p1 <- ggplot(panel_data, aes(x=szja_alap_eft, y=atlagar_hasznalt/1000000))+
  geom_point(size=2.5)+
  labs(y="Átlagár (millió Ft)", x="SZJA-alapot képező jövedelem (ezer Ft)")+
  theme_bw()+
  theme(legend.title=element_blank(), legend.position = "top",
        text = element_text(size = 24, family="serif", colour = "black"))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50), limits = c(0,55))

p2 <- ggplot(panel_data, aes(x=szja_alap_eft, y=medianar_hasznalt/1000000))+
  geom_point(size=2.5, shape=17)+
  labs(y="Medián ár (millió Ft)", x="SZJA-alapot képező jövedelem (ezer Ft)")+
  theme_bw()+
  theme(legend.title=element_blank(), legend.position = "top", 
        text = element_text(size = 24, family="serif", colour = "black"))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50), limits = c(0,55))

gridExtra::grid.arrange(p1, p2, padding = unit(1, "line"), ncol=2,
                        top=grid::textGrob("Az egy főre jutó jövedelem és a lakásárak kapcsolata a vidéki járásokban\n (2016-2020)",
                                     gp=grid::gpar(fontfamily = "serif", fontsize=28)))
                        
hist(log(panel_data$atlagar_hasznalt))
hist(log(panel_data$medianar_hasznalt))
hist(log(panel_data$szja_alap_eft))

#### Modellépítés ####

### Pooled panel model
pooled <- plm(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                vendeglatohely + szelektiv_szazalek + internet_elofiz_efo,
              data=panel_data, model="pooling")
summary(pooled)

pooled_m <- plm(log(medianar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                  log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                  mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                  vendeglatohely + szelektiv_szazalek + internet_elofiz_efo,
              data=panel_data, model="pooling")
summary(pooled_m) #szinte minden szignifikáns

stargazer::stargazer(pooled, pooled_m, type="text")

### Fixed effect panel model
fixef_model_1 <- plm(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                       log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                       mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                       vendeglatohely + szelektiv_szazalek + internet_elofiz_efo,
                     data=panel_data, model="within", index=c("jaras","ev"))
summary(fixef_model_1)
fixef(fixef_model_1)

fixef_model_1_m <- plm(log(medianar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                         log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                         mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                         vendeglatohely + szelektiv_szazalek + internet_elofiz_efo,
                       data=panel_data, model="within", index=c("jaras","ev"))
summary(fixef_model_1_m)
fixef(fixef_model_1_m)

stargazer::stargazer(fixef_model_1, fixef_model_1_m, type="text")

#döntés pooled és fix modellek között
pooltest(pooled, fixef_model_1)
pooltest(pooled_m, fixef_model_1_m)
pFtest(fixef_model_1, pooled) #fix kell
pFtest(fixef_model_1_m, pooled_m) #fix kell

fixef_model_2 <- plm(log(atlagar_hasznalt) ~ allaskeresok_szazalek +
                       log(szja_alap_eft) + 
                        mukodo_vall_efo + orvosrajuto_lakos +
                       log(kisker_uzlet_10efo) + vendeglatohely , data=panel_data,
                     model="within", index=c("jaras","ev"))
summary(fixef_model_2)
fixef(fixef_model_2)

fixef_model_3 <- plm(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                       log(szja_alap_eft) +
                       szennyviz_szazalek + log(mukodo_vall_efo) + log(orvosrajuto_lakos) +
                       log(vendeglatohely), data=panel_data,
                     model="within", index=c("jaras","ev"))
summary(fixef_model_3)
fixef(fixef_model_3)

fixef_model_3_m <- plm(log(medianar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                       log(szja_alap_eft) +
                       szennyviz_szazalek + log(mukodo_vall_efo) + log(orvosrajuto_lakos) +
                        log(vendeglatohely), data=panel_data,
                     model="within", index=c("jaras","ev"))
summary(fixef_model_3_m)
summary(fixef(fixef_model_3_m))


# Function: Calculates AIC based on an lm or plm object

AIC_adj <- function(mod){
  # Number of observations
  n.N   <- nrow(mod$model)
  # Residuals vector
  u.hat <- residuals(mod)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  length(coef(mod)) + 1
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}



###Random-hatású modell
# random_model_1 <- plm(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
#                         log(szja_alap_eft) +
#                         szennyviz_szazalek + mukodo_vall_efo + orvosrajuto_lakos +
#                         kisker_uzlet_10efo + vendeglatohely + auto_efo, data=panel_data,
#                      model="random", index=c("jaras","ev"))
# 
# summary(random_model_1)

# Hausman teszt és térbeli autokorrelációra robusztus Hausman (döntsünk a fix és a 
# random modell között. Ha H0-t elvetjük, akkor lesz fix modell)
hausman_panel <- phtest(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                          log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                          mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                          vendeglatohely + szelektiv_szazalek + internet_elofiz_efo,
                        data = panel_data, index = c("jaras","ev"))
hausman_panel #A fix kell

hausman_panel_m <- phtest(log(medianar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                            log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                            mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                            vendeglatohely + szelektiv_szazalek + internet_elofiz_efo,
                        data = panel_data, index = c("jaras","ev"))
hausman_panel_m #A fix kell


library(stats)

panel_data <- panel_data %>% arrange(ev)

#Térbeli súlymátrix létrehozása
v_age_dist <- as.matrix( dist( cbind( Coords$hossz, Coords$szel ) ) )
v_age_dists_inv <- 1 / v_age_dist
diag( v_age_dists_inv ) <- 0
v_age_dist[ 1:10, 1:10]

#koordináták kimentése
Coords_1 <- cbind( Coords$hossz, Coords$szel )

#Szomszédság definiálása
neib <- dnearneigh( coordinates( Coords_1 ), 0, 36, longlat = TRUE)
listw <- nb2listw( neib )


### Térbeli autokorreláció struktúrájára irnyuló tesztek

slmtest(fixef_model_1, listw=listw, test="lml") #p-value < 2.2e-16
slmtest(fixef_model_1_m, listw=listw, test="lml") #p-value = 9.38e-09

slmtest(fixef_model_1, listw=listw, test="rlml") #p-value = 1.222e-08
slmtest(fixef_model_1_m, listw=listw, test="rlml") #p-value = 0.0001229

slmtest(fixef_model_1, listw=listw, test="lme") #p-value = 1.484e-12
slmtest(fixef_model_1_m, listw=listw, test="lme") #p-value = 3.468e-06

slmtest(fixef_model_1, listw=listw, test="rlme") #p-value = 0.06529 -> a SAR jó lesz
slmtest(fixef_model_1_m, listw=listw, test="rlme")#p-value = 0.06841 -> a SAR jó lesz
#térbelii késlelteté1 esetén a hibatagban nincs autokorreláció 5%-on

# Moran és Geary évenként

### 2016
# Moran I
moran( df_2016$atlagar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2016$atlagar_hasznalt, listw, 175, 174, Szero( listw ) )
# Moran I
moran( df_2016$medianar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2016$medianar_hasznalt, listw, 175, 174, Szero( listw ) )

### 2017
# Moran I
moran( df_2017$atlagar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2017$atlagar_hasznalt, listw, 175, 174, Szero( listw ) )
# Moran I
moran( df_2017$medianar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2017$medianar_hasznalt, listw, 175, 174, Szero( listw ) )

### 2018
# Moran I
moran( df_2018$atlagar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2018$atlagar_hasznalt, listw, 175, 174, Szero( listw ) )
# Moran I
moran( df_2018$medianar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2018$medianar_hasznalt, listw, 175, 174, Szero( listw ) )


### 2019
# Moran I
moran( df_2019$atlagar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2019$atlagar_hasznalt, listw, 175, 174, Szero( listw ) )
# Moran I
moran( df_2019$medianar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2019$medianar_hasznalt, listw, 175, 174, Szero( listw ) )

### 2020
# Moran I
moran( df_2020$atlagar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2020$atlagar_hasznalt, listw, 175, 174, Szero( listw ) )
# Moran I
moran( df_2020$medianar_hasznalt, listw, 175, 174, zero.policy = NULL, NAOK = FALSE )
# Geary C
geary( df_2020$medianar_hasznalt, listw, 175, 174, Szero( listw ) )


# Térbeli függőség tesztelése 
slmtest(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
          log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
          mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
          vendeglatohely + szelektiv_szazalek + internet_elofiz_efo, data = panel_data,
        listw = listw, test = "lme", model = "within", index = c("jaras","ev"))

slmtest(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
          log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
          mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
          vendeglatohely + szelektiv_szazalek + internet_elofiz_efo, data = panel_data,
        listw = listw, test = "lml", model = "within", index = c("jaras","ev"))

slmtest(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
          log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
          mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
          vendeglatohely + szelektiv_szazalek + internet_elofiz_efo, data = panel_data,
        listw = listw, test = "rlml", model = "within", index = c("jaras","ev"))

slmtest(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
          log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
          mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
          vendeglatohely + szelektiv_szazalek + internet_elofiz_efo, data = panel_data,
        listw = listw, test = "rlme", model = "within", index = c("jaras","ev"))
# Egyértelműen a panel SAR modell kell


### Térbeli panel modellek

# Fixed-effect SAR átlagárra
SAR_panel_1 <- spml(log(atlagar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                      log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                      mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                      vendeglatohely + szelektiv_szazalek + internet_elofiz_efo, data = panel_data,
                      listw = listw, model = "within", index = c("jaras","ev"), lag = T, effect="individual", 
                      spatial.error = "none")
summary(SAR_panel_1)

#standard reziduumok és becsült értékek kimentése, majd ábrázolása
res_1 <- scale(residuals(SAR_panel_1))
y_kalap <- SAR_panel_1$fitted.values
plot(y_kalap,res_1)

BIC_SAR_1 <- -2*SAR_panel_1$logLik + (length(SAR_panel_1$coefficients)+1)*log(870)
BIC_SAR_1

SAR_panel_2 <- spml(log(atlagar_hasznalt) ~ allaskeresok_szazalek +
                      log(szja_alap_eft) + orvosrajuto_lakos +
                      mukodo_vall_efo + kisker_uzlet_10efo +
                      vendeglatohely, data = panel_data,
                    listw = listw, model = "within", index = c("jaras","ev"), lag = T, effect="individual", 
                    spatial.error = "none")
summary(SAR_panel_2)

BIC_SAR_2 <- -2*SAR_panel_2$logLik + (length(SAR_panel_2$coefficients)+1)*log(870)
BIC_SAR_2

res_2 <- scale(residuals(SAR_panel_2))
y_kalap_2 <- SAR_panel_2$fitted.values
plot(y_kalap_2, res_2)

# Fixed-effect SAR medián árra

SAR_panel_1_m <- spml(log(medianar_hasznalt) ~ telepulesarany_5000 + allaskeresok_szazalek +
                        log(szja_alap_eft) + vezetekesgaz_szazalek + orvosrajuto_lakos +
                        mukodo_vall_efo + kisker_uzlet_10efo + gyogyszertar_10efo +
                        vendeglatohely + szelektiv_szazalek + internet_elofiz_efo, data = panel_data,
                      listw = listw, model = "within", index = c("jaras","ev"), lag = T, effect="individual", 
                      spatial.error = "none")
summary(SAR_panel_1_m)

BIC_SAR_1_m <- -2*SAR_panel_1_m$logLik + (length(SAR_panel_1_m$coefficients)+1)*log(870)
BIC_SAR_1_m

res_1_m <- scale(residuals(SAR_panel_1_m))
y_kalap_1_m <- SAR_panel_1_m$fitted.values

plot(y_kalap_1_m, res_1_m)

SAR_panel_2_m <- spml(log(medianar_hasznalt) ~ allaskeresok_szazalek +
                        log(szja_alap_eft) + orvosrajuto_lakos +
                        mukodo_vall_efo + kisker_uzlet_10efo, data = panel_data,
                    listw = listw, model = "within", index = c("jaras","ev"), lag = T, effect="individual", 
                    spatial.error = "none")
summary(SAR_panel_2_m)

BIC_SAR_2_m <- -2*SAR_panel_2_m$logLik + (length(SAR_panel_2_m$coefficients)+1)*log(870)
BIC_SAR_2_m

res_2_m <- scale(residuals(SAR_panel_2_m))
y_kalap_2_m <- SAR_panel_2_m$fitted.values

plot(y_kalap_2_m, res_2_m)
