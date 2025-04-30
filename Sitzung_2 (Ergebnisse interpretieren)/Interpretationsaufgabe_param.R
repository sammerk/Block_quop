library(dplyr)
library(tidyverse)

# Definiton der Parameter ######################################################
## AV #####
AV_Label <- sample(c("Lesegenauigkeit",
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
Messzeitpunkt <- 1:sample(c(6,8,10),1)

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
Normalbereich_lowerlimit <- 
  numeric(max(Messzeitpunkt)) # generates empty numeric vec of length = max(x)

# Vektor füllen
for(i in 1:max(Messzeitpunkt)){
 if(i == 1){
     Normalbereich_lowerlimit[1] <- 
      case_when(AV_Label == "Lesegenauigkeit" ~ 
                    sample(4:18, 1),
                AV_Label == "Lesegeschwindigkeit" ~ 
                    sample(10:70, 1),
                AV_Label == "Leseverständnis" ~ 
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
                                 min(Normalbereich_lowerlimit)))) %>%
    mutate(Limit = c(rep("lower limit", n()/2),
                     rep("upper limit", n()/2)))



Weite_Normalbereich_empirisch <- 
    Data_Normalbereich$AV[max(Messzeitpunkt)*2] -    
    Data_Normalbereich$AV[1]
    
# Daten Individuum generieren ##################################################
Daten_Individuum <- 
    tibble(
        Messzeitpunkt = Messzeitpunkt,
        AV1 = numeric(length(Messzeitpunkt)), # initialize empty vectors
        AV2 = numeric(length(Messzeitpunkt)), 
        AV3 = numeric(length(Messzeitpunkt)), 
        AV4 = numeric(length(Messzeitpunkt)), 
    )

### Konstante Geschw; Verlauf innerhalb Normalbereich ##########################
for(i in 1:max(Messzeitpunkt)){
    Daten_Individuum$AV1[i] <- 
        Data_Normalbereich %>% 
          filter(Messzeitpunkt == i) %>% 
          pull(AV) %>% 
          min() + 
          (i-1)/(max(Messzeitpunkt) - 1)*Weite_Normalbereich_empirisch +
        # plus etwas Rauschen
          + sample(-Weite_Normalbereich_empirisch:Weite_Normalbereich_empirisch/8)
}

# Korrektur erster/letzter Messzeitpunkt in Normalbereich
Daten_Individuum[Daten_Individuum$Messzeitpunkt == 1,]$AV1 <- 
    min(Data_Normalbereich[Data_Normalbereich$Messzeitpunkt == 1,]$AV) +
    Weite_Normalbereich_empirisch*.05
Daten_Individuum[Daten_Individuum$Messzeitpunkt == max(Messzeitpunkt),]$AV1 <- 
    max(Data_Normalbereich[Data_Normalbereich$Messzeitpunkt == max(Messzeitpunkt),]$AV) -
    Weite_Normalbereich_empirisch*.05


# Daten Plotten ####

ggplot() +
    geom_polygon(data = Data_Normalbereich, 
                 aes(Messzeitpunkt, AV),
                 fill = "#11111130") +
    geom_point(data = Daten_Individuum, aes(Messzeitpunkt, AV1)) +
    labs(caption = paste("Norm_entw:", Normalbereich_entwicklung_qual,
                         "Ind_entw:", Entwicklung_ind)) +
    expand_limits(y=0)
