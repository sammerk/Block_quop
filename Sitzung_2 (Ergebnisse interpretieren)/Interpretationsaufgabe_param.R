library(dplyr)
library(tidyverse)

# Definiton der Parameter ######################################################
## AV #####
AV <- sample(c("Lesegenauigkeit",
               "Lesegeschwindigkeit",
               "Leseverständnis"),
             1)
AV_Einheit <- case_when(AV == "Lesegenauigkeit" ~ 
                            "Anzahl richtig gelesener Wörter",
                        AV == "Lesegeschwindigkeit" ~ 
                            "Wörter pro Minute",
                        AV == "Leseverständnis" ~ 
                            "Anzahl korrekt beantworteter Fragen")



## Intervention ##### 
Intervention <- sample(c("Intervention wurde durchgeführt",
                         "Keine Intervention"),
                       1)

## Messzeitpunkt ####
if(Intervention == "Keine Intervention")
    Messzeitpunkt <- 1:sample(5:9,1)
if(Intervention != "Keine Intervention")
    Messzeitpunkt <- 1:sample(9:12,1)

## Interventionsdauer ##### 
Interventionsdauer <- sample(3:(max(Messzeitpunkt) - 1), 1)
Interventionsstart <- sample(2:(max(Messzeitpunkt) - 1 - Interventionsdauer), 1)



## Normalbereichsband ##### 
Normalbereich_entwicklung_qual <- sample(c("langsam",
                                           "mittel",
                                           "schnell"), 1)
Normalbereich_entwicklung_quant <- 
    case_when(Normalbereich_entwicklung_qual == "langsam" ~ .5,
              Normalbereich_entwicklung_qual == "mittel" ~ 2.5,
              Normalbereich_entwicklung_qual == "schnell" ~ 5)

Normalbereich_weite_qual <- sample(c("mittel",
                                     "weit"), 1)
Normalbereich_weite_quant <-
    case_when(
        Normalbereich_weite_qual == "mittel" ~ 2,
        Normalbereich_weite_qual == "weit" ~ 3
    )

## Individuum ####
Start_ind <- sample(c("unterdurchschnittlich außerhalb",
                      "unterdurchschnittlich innerhalb",
                      "überdurchschnittlich innerhalb",
                      "überdurchschnittlich außerhalb"))
Entwicklung_ind <- 
    sample(c("Artefakte/Messfehler",
             "zuerst durchschnittlich dann überdurchschnittlich",
             "zuerst durchschnittlich dann durchschnittlich",
             "zuerst durchschnittlich dann unterdurchschnittlich",
             "zuerst überdurchschnittlich dann überdurchschnittlich",
             "zuerst überdurchschnittlich dann durchschnittlich",
             "zuerst überdurchschnittlich dann unterdurchschnittlich",
             "zuerst unterdurchschnittlich dann überdurchschnittlich",
             "zuerst unterdurchschnittlich dann durchschnittlich",
             "zuerst unterdurchschnittlich dann unterdurchschnittlich"), 1)

# Daten generieren #############################################################
## Normalbereich ####
# Vektor initialisieren
Normalbereich_lowerlimit <- numeric(max(Messzeitpunkt))

# Vektor füllen
for(i in 1:max(Messzeitpunkt)){
 if(i == 1){
     Normalbereich_lowerlimit[1] <- 
      case_when(AV == "Lesegenauigkeit" ~ 
                    sample(4:18, 1),
                AV == "Lesegeschwindigkeit" ~ 
                    sample(10:70, 1),
                AV == "Leseverständnis" ~ 
                    sample(4:8, 1))
 }else{
     Normalbereich_lowerlimit[i] <- 
         # erhöhen oder stagnieren
         Normalbereich_lowerlimit[i-1] + 
         Normalbereich_lowerlimit[1]*Normalbereich_entwicklung_quant*runif(1,0,.3)
 }
}


Data_Normalbereich <- 
    tibble(
        Messzeitpunkt = c(Messzeitpunkt, rev(Messzeitpunkt)),
        AV = c(Normalbereich_lowerlimit,
                                 rev(Normalbereich_lowerlimit + 
                                 min(Normalbereich_lowerlimit) * 
                                 Normalbereich_weite_quant * 1)))

Weite_Normalbereich_empirisch <- 
    Data_Normalbereich$AV[max(Messzeitpunkt)*2] -    
    Data_Normalbereich$AV[1]
    
# Daten Individuum generieren ####
Daten_Individuum <- 
    tibble(
        Messzeitpunkt = Messzeitpunkt,
        AV = numeric(length(Messzeitpunkt))
    )

# Verlauf innerhalb Normalbereich
for(i in 1:max(Messzeitpunkt)){
    Daten_Individuum$AV[i] <- 
        Data_Normalbereich %>% 
          filter(Messzeitpunkt == i) %>% 
          pull(AV) %>% 
          min() + 
          (i-1)/(max(Messzeitpunkt) - 1)*Weite_Normalbereich_empirisch +
        # plus etwas Rauschen
          + sample(-Weite_Normalbereich_empirisch:Weite_Normalbereich_empirisch/8)
}



# Daten Plotten ####

ggplot() +
    geom_polygon(data = Data_Normalbereich, 
                 aes(Messzeitpunkt, AV),
                 fill = "#11111130") +
    geom_point(data = Daten_Individuum, aes(Messzeitpunkt, AV)) +
    labs(caption = paste("Entw.:", Normalbereich_entwicklung_qual, "Weite:", Normalbereich_weite_qual)) +
    expand_limits(y=0)
