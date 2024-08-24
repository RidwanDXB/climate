rm(list=ls())
library(tidyverse)
library(writexl)
library(R.utils)
library(data.table)



bankersround <- function(number, precision = 1, eps = 2^-52, tolerance = TRUE) {
  if(tolerance == TRUE) {
    number <- signif(number, 15)   # eerst 
  }
  return (sign(number) * round((abs(number) * 10 ^ precision + eps) - eps, 0) / 10 ^ precision)
}

setwd("/Users/parisienne/Documents/Climat 2.0/brondata")

ftp_folder <- "https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/MENS/"

filename <- c("MENSQ_75_previous-1950-2022.csv.gz", "MENSQ_75_latest-2023-2024.csv.gz")

for (i in 1:2) {
  download.file(paste0(ftp_folder, filename[i]), filename[i])
  gunzip(filename[i], overwrite=TRUE, remove=FALSE)
  assign(paste0('origineel', i),
         read.csv(str_sub(filename[i], 1, -4), sep=";", dec="."))
}

origineel <- bind_rows(origineel1, origineel2)   # Bij meerdere bestanden moeten we iets beters bedenken.

# CSV-bestand inlezen.
# We doen dit met Base R.
# origineel <- read.csv(filename_csv, sep = ";", dec=".")   # Gebruik fread om een data.table in te lezen
# origineel <- fread(filename_csv, sep = ";", dec=".")   # Gebruik fread om een data.table in te lezen

# We zien rechtsboven in Environment dat onze dataset 16499 waarnemingen bevat bestaande uit 162 variabelen.
# We selecteren de waarnemingen die betrekking hebben op PARIS-MONTSOURIS. Daarmee wordt de dataset gereduceerd tot 876 waarnemingen.

# paris <- origineel[origineel$NOM_USUEL == 'PARIS-MONTSOURIS', ]

#   7. RR              : cumul mensuel des hauteurs de précipitation (en mm et 1/10)
#  14. NBJRR1          : nombre de jours avec RR ≥ 1.0 mm
#  15. NBJRR5          : nombre de jours avec RR ≥ 5.0 mm
#  26. TX              : moyenne mensuelle des températures maximales (TX) quotidiennes (en °C et 1/10)
#  37. NBJTX25         : nombre de jours avec TX ≥ 25°C
#  38. NBJTX30         : nombre de jours avec TX ≥ 30°C
#  43. TN              : moyenne mensuelle des températures minimales (TN) quotidiennes (en °C et 1/10)
#  60. NBJGELEE        : nombre de jours avec gelée
#  67. TM              : moyenne mensuelle des (TN+TX)/2 quotidiennes (en °C et 1/10)
#  70. TMM             : moyenne mensuelle des températures moyennes (TM) quotidiennes (en °C et 1/10)
# 102. NBJFF16         : nombre de jours avec FXI ≥ 16 m/s
# 123. FFM             : moyenne mensuelle de la force moyenne quotidienne du vent moyenné sur 10 mn (FFM), à 10 m (en m/s et 1/10)
# 126. INST            : cumul mensuel des durées totales d’insolation quotidiennes (en mn)
# 129. NBSIGMA0        : nombre de jours avec SIGMA = 0% (SIGMA est la fraction d'insolation par rapport à la durée du jour)
# 147. NBJNEIG         : nombre de jours avec précipitation de neige
# 162. NBJBROU         : nombre de jours présents avec brouillard

# QRR, QTX, QTN, QTM, QTMM, QFFM, QINST, NBINST

setDT(origineel)
paris <- origineel[NOM_USUEL == 'PARIS-MONTSOURIS', .(NOM_USUEL, AAAAMM,
                                                      RR, NBJRR1, NBJRR5,
                                                      TX, NBJTX25, NBJTX30,
                                                      TN, NBJGELEE,
                                                      TM,
                                                      NBJFF16, FFM,
                                                      INST, NBSIGMA0,
                                                      NBJNEIG, NBJBROU,
                                                      NBFXI
                                                      )]

# 1. De waarde ontbreekt
# 2. De waarde is niet berekend op basis van alle dagen in de betreffende maand.
# 3. De kwaliteit van de statistiek is onvoldoende.

# We gaan eerst de tabel iets overzichtelijker maken.
sim1 <- as_tibble(paris) %>%
  mutate(annee = AAAAMM %/% 100,
  mois = AAAAMM %% 100) %>%
  rename(station = NOM_USUEL) %>%
  select(station, annee, mois, RR:NBJBROU, NBFXI)

write_xlsx(sim1, "parisdata.xlsx")

sim2 <- sim1 %>%
  mutate(NBJFF16_ratio = NBJFF16 / NBFXI)

view(sim2)


# Bereken een aantal klimaatgemiddelden
gemiddelden <- sim1 %>%
  filter(annee %in% 1991:2020) %>%
  group_by(mois) %>%
  summarise_at(vars(RR:NBJBROU), ~bankersround(mean(.x, na.rm = TRUE), 1))

view(gemiddelden)

# Hier selecteren we de variabelen TN, TM en TX.
sim2 <- as.tibble(sim1) %>%
  select(station, annee, mois, TN, TM, TX) %>%
  rename(T_min = TN,
         T_moyenne = TM,
         T_max = TX)

# Voor de LOESS
test <- sim2 %>%
  filter(mois %in% c(6, 7, 8)) %>%
  mutate(aantal_dagen = ifelse(mois == 6, 30, 31)) %>%
  group_by(annee) %>%
  summarise(TNM = sum(T_min * aantal_dagen) / 92,
            TMM = sum(T_moyenne * aantal_dagen) / 92,
            TXM = sum(T_max * aantal_dagen) / 92)
write_csv2(test, 'paris_loess.csv')

            

# Definieer de maanden van de maanden.
# Ignorez les signes diacritiques:
# accent aigu          é
# accent grave         à è ù
# accent circonflexe   â ê î ô û
# tréma                ë ï ü ÿ

les_mois <- c('janvier', 'fevrier', 'mars', 'avril', 'mai', 'juin', 'juillet', 'aout', 'septembre', 'octobre', 'novembre', 'decembre')
sim2$mois_char <- les_mois[sim2$mois]

# Maak een mooie tabel met de maanden als kolomnamen.
sim3 <- sim2 %>%
  select(-mois, -T_min, -T_moyenne) %>%
  pivot_wider(names_from = mois_char,
              values_from = T_max)

# Of zo:
sim3a <- sim2 %>%
  select(-mois) %>%
  pivot_wider(names_from = mois_char,
              values_from = c(T_min, T_moyenne, T_max))

sim3b <- sim3a %>%
  pivot_longer(cols = T_min_janvier:T_max_decembre,
               names_pattern = "(T_min_|T_moyenne_|T_max_)(.*)",
               names_to = c("variable", ".value")) %>%
  mutate(variable = str_sub(variable, end = -2))


# Bereken nu van elke variabele het gemiddelde
sim4 <- sim3b %>%
  group_by(variable) %>%
  summarise_at(vars(janvier:decembre), ~bankersround(mean(.x, na.rm = TRUE), 1))

view(sim4)

# LOESS
zomer <- sim3 %>%
  select(annee, juin, juillet, aout) %>%
  filter(annee <= 2023) %>%
  mutate(ete = (juin * 30 + juillet * 31 + aout * 31) / 92)

climatrend <- function(t, y, startyear, endyear, p = 0.05, ybounds, drawplot) {
  
  #  t = parisdata$jaar
  #  y = parisdata$juli
  #  endyear = 2022
  #  p = 0.05
  #  ybounds = c(20, 30)
  #  drawplot = T
  
  # fixed parameters()
  width <- 42
  
  # dimensions etc.
  
  dt <- diff(t[1:2])
  n <- length(y)   # Number of y values
  ig <- ((!is.na(y)) & t <= endyear)
  
  yg <- y[ig == TRUE]
  tg <- t[ig == TRUE]
  y <- yg
  t <- tg
  ng <- sum(ig)    # Number of valid y values
  
  if (any(diff(tg) != dt)) {
    error("NA may only occur in y at beginning and/or end of time-series")
  }
  
  
  # 30 year average computation
  avt <- avy <- avysd <- NULL
  
  if (ng > 29) {
    avt <- tg + dt / 2
    avy <- stats::filter(yg, rep(1, 30) / 30, method = "convolution")
    avy2 <- stats::filter(yg^2, rep(1, 30) / 30, method = "convolution")
    avysd <- sqrt(avy2 - avy^2)
    ind <- 15:(ng-15)
    avt <- avt[ind]
    avy <- avy[ind]   # 30 year moving average
    avysd <- avysd[ind]
  }
  
  # linear LOESS trendline computation
  control <- loess.control(surface = "direct", statistics = "exact", iterations = 1)
  ng <- max(tg) - min(tg) + 1
  span <- width / ng
  
  # y = temperature
  # t = time
  mdl <- loess(y ~ t,
               data = data.frame(t = tg, y = yg),
               span = span,
               degree = 1,
               control = control)
  
  pre <- predict(mdl, newdata = data.frame(t = t), se = TRUE)
  trend <- pre$fit
  trendsd <- pre$se.fit
  
  # confidence limits of the estimation (normal approximation)
  p <- 0.05
  trendub <- trend + trendsd * qnorm(1 - (1 - p) / 2)
  trendlb <- trend - trendsd * qnorm(1 - (1 - p) / 2)
  
  # apply bounds (for the graph)
  trend   <- pmax(pmin(trend  , ybounds[2]), ybounds[1])
  trendub <- pmax(pmin(trendub, ybounds[2]), ybounds[1])
  trendlb <- pmax(pmin(trendlb, ybounds[2]), ybounds[1])
  
  # plotting
  if (!(drawplot == FALSE)) {
    climate <- data.frame(x = tg, y = yg, y_trend = trend)
    normal <- data.frame(x = avt, y = avy)
    
    eindpunt <- climate %>%
      filter(x == max(x) | x == 1995)
    
    # De eenvoudigste grafiek
    ggplot(data = climate, aes(x = x)) +
      geom_line(aes(y = y)) +
      geom_line(aes(y = y_trend), color = "blue") +
      geom_line(data = normal, aes(x = x, y = y), color = "purple", linewidth = 1) +
      geom_smooth(aes(y = y), span = span) +
      geom_point(data = eindpunt, mapping = aes(x = x, y = y_trend), color = "red") +
      geom_label(data = eindpunt, aes(x = x, y = y_trend, label = round(y_trend, 2)),
                 nudge_y = 0.3,
                 size = 2.5) +
      scale_x_continuous(limits = c(startyear, endyear),
                         breaks = seq(startyear, 2020, by = 10)) +
      scale_y_continuous(limits = c(ybounds[1], ybounds[2]),
                         breaks = seq(ybounds[1], ybounds[2], by = 1),
                         minor_breaks = seq(ybounds[1], ybounds[2], by = 0.5)
      ) +
      ggtitle("Moyenne des températures maximales en été à Paris") +
      xlab("Année") + ylab("Température")
  }
}

climatrend(t = zomer$annee, y = zomer$ete, startyear = 1950, endyear = 2023, ybounds = c(20, 31), drawplot = T)

mean(zomer$ete[1961<=zomer$annee & zomer$annee <= 1990])