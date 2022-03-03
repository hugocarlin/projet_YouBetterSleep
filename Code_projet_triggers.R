########## PRÉPARATION DU CODE

### Effacer les données de l'environnement
rm(list = ls())


### Libraries
library('readr')
library('stringr')
library('purrr')
library('dplyr')
library('here')
library('ggplot2')
library('scales')
library('reshape2')
library('modeltime')
library('tidymodels')
library('tidyverse')
library('timetk')
library('lubridate')





########## PRÉPARATION DES DONNÉES

### Chemin du dossier contenant les data
setwd("data")


### Importation des données
# Nom des données fichiers à importer
files_names <- list.files(here("extract_audit_data"))
files_names <- files_names[-c(16:17)] # Suppression des bases qu'on ne veut pas (23 et 24)
# Import en tibble
lst_tib <- map(.x = files_names, 
               .f = function (x) read_csv(paste(here("extract_audit_data"), x, sep = "/")))
# On renomme les tibble
names(lst_tib) <- files_names
# Union des données pour obtenir une seule base
data <- reduce(.x = lst_tib, .f = bind_rows)


### Gestion des valeurs abérrantes et NA
summary(data)
# Valeurs négatives dans la variable sell_out
va <- which(data$sell_out<0)
data <- data[-c(va),]


### On garde les nature = WeMissYou et Welcome
data <- data %>% filter(nature=="We_Miss_You" | nature=="Welcome")


### Gestion du format des données
summary(data)
data$brand_code <- factor(data$brand_code)
data$country_code <- factor(data$country_code)
data$nature <- factor(data$nature)
data$contact_type <- factor(data$contact_type)





########## STATS DESCRIPTIVES

### Répartition des Welcome/WeMissYou (campagne)
# Création de la base de données
camp_welc_wmy <- data.frame(table(data$nature))
colnames(camp_welc_wmy) <- c("nature","nb")
camp_welc_wmy <- camp_welc_wmy %>% 
  arrange(desc(nature)) %>%
  mutate(prop = nb / sum(nb) *100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
# Représentation graphique
ggplot(camp_welc_wmy, aes(x="", y=nb, fill=nature)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(prop,2),'%\n(',nb,')',sep='')), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Type de trigger") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Paired")


### Moyenne de mail par campagne
sum(data$nb_targets)/nrow(data)


### Répartition des Welcome/WeMissYou (nb_target)
# Création de la base de données
targ_welc_wmy <- data %>% group_by(nature) %>%
  summarise(nb=sum(nb_targets))
targ_welc_wmy <- targ_welc_wmy %>% 
  arrange(desc(nature)) %>%
  mutate(prop = nb / sum(nb) *100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
# Représentation graphique
ggplot(targ_welc_wmy, aes(x="", y=nb, fill=nature)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(prop,2),'%\n(',nb,')',sep='')), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Type de trigger") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Paired")


### Repartiton des types de triggers selon les pays
# Création de la base de données
camp_pays <- data.frame(table(data$country_code,data$nature))
colnames(camp_pays) <- c('pays','nature','eff')
camp_pays <- camp_pays %>% group_by(pays) %>% 
  mutate(prop = round(prop.table(eff),3)) %>%
  arrange(pays,desc(nature)) %>%
  mutate(ypos=cumsum(prop)-0.5*prop+0.1)
camp_pays <- camp_pays[-c(8),]
# Représentation graphique
ggplot(camp_pays, aes(x=pays, y=eff, fill=nature)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label=paste(prop*100,'%','\n(',eff,')',sep=''), y=ypos), vjust=1.6, color="black", size=4) +
  labs(title = "", x = "Pays", y = "Pourcentage de répartition", fill="Type de trigger")


### Repartition du nombre de mail selon les pays
trig_pays <- data %>% group_by(country_code) %>%
  summarise(nb=sum(nb_targets))


### Repartiton des types de type de trigger selon les marques
# Création de la base de données
camp_marque <- data.frame(table(data$brand_code,data$nature))
colnames(camp_marque) <- c('marque','nature','eff')
camp_marque <- camp_marque %>% group_by(marque) %>% 
  mutate(prop = round(prop.table(eff),3)) %>%
  arrange(marque,desc(nature)) %>%
  mutate(ypos=cumsum(prop)-0.5*prop+0.09)
camp_marque <- camp_marque[-c(10),]
# Représentation graphique
ggplot(camp_marque, aes(x=marque, y=eff, fill=nature)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label=paste(prop*100,'%','\n(',eff,')',sep=''), y=ypos), vjust=1.6, color="black", size=4) +
  labs(title = "",x = "Pays", y = "Pourcentage de répartition", fill="Type de contact")


### Repartition du nombre de mail selon les marques
trig_marque <- data %>% group_by(brand_code) %>%
  summarise(nb=sum(nb_targets))


### Répartition des Welcome/WeMissYou (campagnes) selon les types de contact
# Création de la base de données``
camp_nat <- data.frame(table(data$contact_type,data$nature))
colnames(camp_nat) <- c('contact','nature','eff')
camp_nat <- camp_nat %>% group_by(nature) %>% 
  mutate(prop = round(prop.table(eff),3)) %>%
  arrange(nature,desc(contact)) %>%
  mutate(ypos=cumsum(prop)-0.5*prop+0.09)
# Représentation graphique
ggplot(camp_nat, aes(x=nature, y=eff, fill=contact)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label=paste(prop*100,'%',' (',eff,')',sep=''), y=ypos-0.05),
            vjust=1.6, color="black", size=4) +
  labs(title = "", x = "Nature des trigger", y = "Pourcentage de répartition", fill="Type de contact")


### Répartition des Welcome/WeMissYou (nb_target) selon les types de contact
# Création de la base de données``
trag_nat <- data %>% group_by(nature,contact_type) %>%
  summarise(nb=sum(nb_targets))
trag_nat <- trag_nat %>%
  mutate(prop = round(prop.table(nb),3)) %>%
  arrange(nature,desc(contact_type)) %>%
  mutate(ypos=cumsum(prop)-0.5*prop+0.04)
# Représentation graphique
ggplot(trag_nat, aes(x=nature, y=prop, fill=contact_type)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label=paste(prop*100,'%',' (',nb,')',sep=''), y=ypos),
            vjust=1.6, color="black", size=4) +
  labs(title = "", x = "Nature des trigger", y = "Pourcentage de répartition", fill="Type de contact")












########## SÉRIES TEMPORELLES

### Représentation graphique des séries temporelles
# nb_open de We_miss_you par mois
data %>% filter(nature=="We_Miss_You") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(nb_open_month = sum(nb_open)) %>%
  plot_time_series(month, nb_open_month)
# nb_open de Welcome par mois
data %>% filter(nature=="Welcome") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(nb_open_month = sum(nb_open)) %>%
  plot_time_series(month, nb_open_month)
# nb_targets de We_miss_you par mois
data %>% filter(nature=="We_Miss_You") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(nb_targets_month = sum(nb_targets)) %>%
  plot_time_series(month, nb_targets_month)
# nb_targets de Welcome par mois
data %>% filter(nature=="Welcome") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(nb_targets_month = sum(nb_targets)) %>%
  plot_time_series(month, nb_targets_month)
# nb_buyers de We_miss_you par mois
data %>% filter(nature=="We_Miss_You") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(nb_buyers_month = sum(nb_buyers)) %>%
  plot_time_series(month, nb_buyers_month)
# nb_buyers de Welcome par mois
data %>% filter(nature=="Welcome") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(nb_buyers_month = sum(nb_buyers)) %>%
  plot_time_series(month, nb_buyers_month)
# sell_out de We_Miss_You par mois
data %>% filter(nature=="We_Miss_You") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(sell_out = sum(sell_out)) %>%
  plot_time_series(month, sell_out)
# sell_out de Welcome par mois
data %>% filter(nature=="Welcome") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(sell_out = sum(sell_out)) %>%
  plot_time_series(month, sell_out)

data$panier_moyen <- data$sell_out / data$nb_buyers

data <- data %>% filter(brand_code == "Kiehls") %>%
    filter(country_code == "IT")


data$panier_moyen[is.nan(data$panier_moyen)] <- 0

data %>% filter(nature=="Welcome") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(panier_moyen_month = mean(panier_moyen)) %>%
  plot_time_series(month, panier_moyen_month)


data %>% filter(nature=="We_Miss_You") %>%
  group_by(month = lubridate::floor_date(delivery_first_contact_date, "month")) %>%
  summarize(panier_moyen_month = mean(panier_moyen)) %>%
  plot_time_series(month, panier_moyen_month)

data <- data %>% arrange(data,delivery_first_contact_date)

data$week <- week(data$delivery_first_contact_date)
data$year <- year(data$delivery_first_contact_date)

data$wy <- paste(data$week,data$year, sep = "-")

tmp <- 0
tmp1 <- data$wy[1]
for (i in 1:nrow(data)){
  if (data$wy[i] != tmp1){
    tmp1 <- data$wy[i]
    tmp <- tmp + 1
  }
  data$wy[i] <- tmp
}

data$wy <- as.numeric(data$wy)

data %>% filter(nature=="Welcome") %>%
  group_by(week = wy) %>%
  summarize(panier_moyen_week = mean(panier_moyen)) %>%
  plot_time_series(week, panier_moyen_week)

data %>% filter(nature=="We_Miss_You") %>%
  group_by(week = wy) %>%
  summarize(panier_moyen_week = mean(panier_moyen)) %>%
  plot_time_series(week, panier_moyen_week)












