train<-read.csv("training.csv", header=T)
test<-read.csv("test.csv", header=T)

skimr::skim(train)
skimr::skim(test)

#Analisi del training
#Ho 8000 osservazioni di 17 variabili, la variabile risposta è selling price
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)

#Prima cosa: ho una variabile problematica che è other features perche ho tutte caratteristiche in stringhe
#Le trasformo in una colonna per ogni feature 

train$other_features  <- gsub("pvc(?=exposure)", "pvc | ",train$other_features , perl = TRUE)
train$other_features  <- gsub("pvc(?=double)", "pvc | ",train$other_features , perl = TRUE)
train$other_features <- gsub("\\s*\\|\\s*", "|", train$other_features)

# Separare le caratteristiche
df_bin <- train %>%
  mutate(riga_id = row_number()) %>%
  separate_rows(other_features, sep = "\\s*\\|\\s*") %>%
  mutate(other_features = trimws(other_features)) %>%
  mutate(presente = 1) %>%
  pivot_wider(names_from = other_features, values_from = presente, values_fill = 0)

#togli colonna "NA", ID e riga ID
df_bin <- df_bin %>%
  select(-all_of(c("NA", "ID", "riga_id")))

colnames(df_bin)

#provare a unire colonne con triple double single
df_bin <- df_bin %>%
  mutate(single_glass = if_else(
    rowSums(select(., starts_with("window frames in glass"))) > 0, 1, 0
  ))%>%
  select(-starts_with("window frames in glass"))
df_bin <- df_bin %>%
  mutate(double_glass = if_else(
    rowSums(select(., starts_with("window frames in double glass"))) > 0, 1, 0
  ))%>%
  select(-starts_with("window frames in double glass"))
df_bin <- df_bin %>%
  mutate(triple_glass = if_else(
    rowSums(select(., starts_with("window frames in triple glass"))) > 0, 1, 0
  ))%>%
  select(-starts_with("window frames in triple glass"))  

#provare a unire exposure 
colonne_da_unire <- c("exposure south", "exposure east", "exposure west","exposure north" )

df_bin <- df_bin %>%
  mutate(single_exposure = if_else(
    rowSums(select(., all_of(colonne_da_unire))) > 0, 1, 0
  )) %>%
  select(-all_of(colonne_da_unire))

colonne_da_unire <- c("double exposure", "exposure north, south", "exposure east, west", "exposure south, east", "exposure north, west", 
                      "exposure south, west", "exposure north, east")
df_bin <- df_bin %>%
  mutate(double_exposure = if_else(
    rowSums(select(., all_of(colonne_da_unire))) > 0, 1, 0
  )) %>%
  select(-all_of(colonne_da_unire))

#ho 5 osservazioni che hanno esposizione da 4 parti, li metto in esposizione tripla
colonne_da_unire <- c("exposure north, south, east", "exposure south, east, west", "exposure north, east, west", "exposure north, south, west", "exposure north, south, east, west")
df_bin <- df_bin %>%
  mutate(triple_exposure = if_else(
    rowSums(select(., all_of(colonne_da_unire))) > 0, 1, 0
  )) %>%
  select(-all_of(colonne_da_unire))

#unisco quelle che hanno i balconi
df_bin[df_bin$`6 balconies`==1,"balcony"]<-1
df_bin[df_bin$`8 balconies`==1,"balcony"]<-1
df_bin<-df_bin%>%select(-all_of(c("6 balconies", "8 balconies")))

skimr::skim(df_bin)

df_bin[df_bin$balcony==1, "balcony"]<-1
df_bin<-df_bin%>%select(-all_of(c("property land1 balcony")))

#unisco half day e full day concierge
df_bin[df_bin$`full day concierge`==1, "half-day concierge"]<-1
df_bin<-df_bin%>%select(-all_of(c("full day concierge")))
df_bin<-df_bin%>%rename("concierge"="half-day concierge" )

#unisco privato e condiviso giardino in condiviso
table(df_bin$`private and shared garden`)
df_bin[df_bin$`private and shared garden`==1, "private garden"]<-1
df_bin[df_bin$`private and shared garden`==1, "shared garden"]<-1
df_bin<-df_bin%>%select(-all_of(c("private and shared garden")))

p<-skimr::skim(df_bin)         
which(p$numeric.mean<(10/8000))
#identifico le caratteristiche che hanno pochissime case e le elimino
#l'unica è quella di avere l'accesso per i disabili, la tolgo
df_bin[df_bin$`disabled access`==1, "selling_price"]
df_bin[c(df_bin$`disabled access`==0 & df_bin$square_meters==100), "selling_price"]

#disabled acces ha solo 3 osservazioni 
df_bin<-df_bin%>%select(-all_of(c("disabled access")))

#------inizio analisi delle colonne

#variabile risposta
summary(df_bin$selling_price)
hist(df_bin$selling_price)
hist(log(df_bin$selling_price))

#Square meters
plot(df_bin[df_bin$selling_price>=2000000,]$square_meters, df_bin[df_bin$selling_price>=2000000,]$rooms_number)
df_bin[df_bin$selling_price>=2000000,"square_meters"]
df_bin[df_bin$square_meters==1,] #problema
#ci sono 5 case che hanno un metro quadro però hanno un numero di stanze superiore a 5
#o 4 o 1 camera ma con tutti 3 o più bagni e hanno un selling_price molto elevato quindi si deduce che
#i metri quadri sono sbagliati

#guardo le case che hanno come metri quadri un numero inferiore a 20
df_bin[df_bin$square_meters<20,] 
#noto che anche una casa con 12 metri quadri ha in realtà più di 5 stanze
#per questo motivo modifico i metri quadrati di queste case
#imputo come metri quadri la media dei metri quadrati in base al numero di staze che hanno:

df_bin<-as.data.frame(df_bin)
#media dei metri quadrati di edifici con più di 5 stanze:
mean(df_bin[df_bin$rooms_number>5, "square_meters"]) # 244.231
df_bin$square_meters[df_bin$square_meters<=12 & df_bin$rooms_number>5 ]<-245

mean(df_bin[df_bin$rooms_number==4, "square_meters"]) #142.0236
df_bin$square_meters[df_bin$square_meters<=12 & df_bin$rooms_number==4]<-  142

#mi rimane quella con 5 metri quadri
mean(df_bin[df_bin$rooms_number==5, "square_meters"]) #186.1692
df_bin$square_meters[df_bin$square_meters<=12 & df_bin$rooms_number==5]<-  186

#mi rimane quella con 1 metro quadro e 3 bagni
df_bin%>%filter(bathrooms_number==3)%>%summarize(mean=mean(square_meters))
df_bin[2571,"square_meters"]<-192

#Bathrooms number

# Ho 25 valori mancanti, se la casa ha meno di 4 stanze viene imputato
#un bagno se no 2 bagni per la moda
table(df_bin$bathrooms_number, useNA = "always")
table(df_bin$bathrooms_number, df_bin$rooms_number, useNA = "always")
df_bin$bathrooms_number[is.na(df_bin$bathrooms_number) & df_bin$rooms_number<4 ]<- "1"
df_bin$bathrooms_number[is.na(df_bin$bathrooms_number) & df_bin$rooms_number>=4 ]<- "2"

#Lift

table(df_bin$lift, useNA = "always")
table(df_bin$total_floors_in_building, useNA = "always")
table(df_bin$total_floors_in_building, df_bin$lift, useNA = "always")

#le oss che hanno valore mancante in ascensore e hanno meno di 3 piani nell'edificio
#le imputo come "no" nell'ascensore ( ho seguito la maggioranza)
df_bin$lift[is.na(df_bin$lift) & df_bin$total_floors_in_building < 3 ]<- "no"

#imputo "yes" alla var ascensore per quelle osservazioni che hanno più di tre piani nell'edificio
df_bin$lift[is.na(df_bin$lift) & df_bin$total_floors_in_building>=3]<-"yes"
#ci sono 3 osservazioni che hanno mancante sia il numero di piani dell'edificio che l'ascensore
#le imputo con ascensore "yes" seguendo la maggioranza
table(df_bin$lift, useNA = "always")
df_bin$lift[is.na(df_bin$lift)]<-"yes"
df_bin$lift<-as.factor(df_bin$lift)


#Rooms number
table(df_bin$rooms_number, useNA = "always") #non ho NA
df_bin$rooms_number<-as.factor(df_bin$rooms_number)

#numero piani nell'edificio
table(df_bin$total_floors_in_building, useNA = "always")
table(df_bin$floor, useNA = "always")
table(df_bin$total_floors_in_building, df_bin$floor, useNA = "always")
# quelli che sono a piano terra che hanno NA come floor of building e
#non hanno tasse condominiali li metto con totali piano pari a 1
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="ground floor" & df_bin$condominium_fees=="No condominium fees" ] <- "1 floor"    
#le restanti 8 osservazioni che sono al ground floor e hanno le tasse condominiali le imputo con un numero di piani pari a 4
#che è la maggioranza
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="ground floor" ] <-"4"

#Guardo anche in questo caso se hanno no condominuim fees, pero hanno tutte il lift 
#(originariamente, non imptutato) percui imputo come se fossero in un condominio
#Per farlo uso la moda
df_bin[is.na(df_bin$total_floors_in_building) & df_bin$floor=="2" & df_bin$condominium_fees=="No condominium fees" ,] #ha l'ascensore quindi ok mettere che ha piu piani
df_bin[is.na(df_bin$total_floors_in_building) & df_bin$floor=="3" & df_bin$condominium_fees=="No condominium fees" ,] 
df_bin[is.na(df_bin$total_floors_in_building) & df_bin$floor=="4" & df_bin$condominium_fees=="No condominium fees",] 

df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="1"] <- "4"
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="2"  ] <- "4"
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="3" ] <- "4"
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="4" ] <- "4"

df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="mezzanine" ] <- "4"

df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="5" ] <- "5"
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="6" ] <- "6"
df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="7" ] <- "8"

df_bin$total_floors_in_building[is.na(df_bin$total_floors_in_building) & df_bin$floor=="semi-basement" ] <- "5"
#Trasformo total floor in numerico
table(df_bin$total_floors_in_building)
df_bin$total_floors_in_building[df_bin$total_floors_in_building=="1 floor"]<-1
df_bin$total_floors_in_building<-as.numeric(df_bin$total_floors_in_building)
sum(df_bin$total_floors_in_building) #Ho 183 appartamenti che hanno piu di 10 piani 
# per ora la alscio numerica ma si pootrebbe unire tutti quelli che hanno piu di 10
# e metterle in una categoria e farla diventare factor

#Floor
table(df_bin$floor, useNA="always")
#metto ground floor in piano 1
df_bin$floor[df_bin$floor=="ground floor"]<-1
df_bin$floor[df_bin$floor=="mezzanine"]<-0.5
df_bin$floor[df_bin$floor=="semi-basement"]<- -1
df_bin$floor<-as.numeric(df_bin$floor)


#Parcheggio auto 

#raggruppo osservazioni che hanno solo il box, quelle che non hanno niente, quelle che hanno sia
#box che parcheggio esterno e quelle che hanno sia box che parcheggio esterno
#in modo da creare 4 categorie

table(df_bin$car_parking, useNA = "always")
df_bin$car_parking[df_bin$car_parking=="1 in garage/box"]<-"box"
df_bin$car_parking[df_bin$car_parking=="2 in garage/box"]<-"box"
df_bin$car_parking[df_bin$car_parking=="1 in garage/box, 1 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="1 in garage/box, 2 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="1 in garage/box, 3 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="1 in garage/box, 5 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="2 in garage/box, 1 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="2 in garage/box, 2 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="2 in garage/box, 3 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="2 in garage/box, 6 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="2 in garage/box, 7 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="7 in garage/box, 3 in shared parking"]<-"box, shared"
df_bin$car_parking[df_bin$car_parking=="1 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="2 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="7 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="20 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="3 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="4 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="5 in shared parking"]<-"shared"
df_bin$car_parking[df_bin$car_parking=="9 in shared parking"]<-"shared"
#Volevo cambiare per categorie diverse ma le frequenze sono troppo poche, meglio lasciare cosi

#Disponibilità casa 

#Imputo gli NA come se non sono disponibili e poi metto tutti gli altri come disponbili
table(df_bin$availability, useNA = "always")
df_bin[is.na(df_bin$availability),]$availability<- "No"
df_bin[grepl("^available", df_bin$availability), ]$availability<-"Yes"

#Condizioni della casa
table(df_bin$conditions, useNA = "always")
table(df_bin$conditions, df_bin$energy_efficiency_class, useNA = "always")
table(df_bin$conditions, df_bin$availability, useNA = "always")
table(df_bin$conditions, df_bin$year_of_construction, useNA = "always")

#Assegno a tutti gli NA in condizioni la condizione eccellente che è la classe maggioritaria
#tranne a quelle con classe a che hanno come maggioritaria "new"
#Classe a 
df_bin$conditions[df_bin$energy_efficiency_class=="a" & is.na(df_bin$conditions)]<- "new / under construction"

#per le restanti la moda è excellent
df_bin$conditions[is.na(df_bin$conditions)]<- "excellent / refurbished"

#Riscaldamento (uguale a gio)
table(df_bin$heating_centralized, useNA = "always")
df_bin[is.na(df_bin$heating_centralized),]
#imputo a quelli che hanno le tasse condominiali "central", mentre alle osservazioni che non hanno tasse condominiali "indipendent"
df_bin$heating_centralized[is.na(df_bin$heating_centralized) & df_bin$condominium_fees=="No condominium fees"] <- "independent"
df_bin$heating_centralized[is.na(df_bin$heating_centralized)]<- "central"

#CLASSE ENERGETICA (leggermente diverso)
table(df_bin$energy_efficiency_class, useNA = "always")
table(df_bin$energy_efficiency_class, df_bin$conditions, useNA = "always")
#Applico la moda usando le condizioni della casa: noto che tutte hanno mda classe g
#tranne quelle nuove a cui imputo classe a
df_bin$energy_efficiency_class<-as.factor(df_bin$energy_efficiency_class)
df_bin%>%filter(!energy_efficiency_class %in% c("a", "b", "c", "d", "e", "f", "g") & ! is.na(energy_efficiency_class))
df_bin[!df_bin$energy_efficiency_class %in% c("a", "b", "c", "d", "e", "f", "g") & ! is.na(df_bin$energy_efficiency_class),"energy_efficiency_class"]<- NA

df_bin$energy_efficiency_class[is.na(df_bin$energy_efficiency_class) & df_bin$conditions=="new / under construction"]<- "a"
df_bin$energy_efficiency_class[is.na(df_bin$energy_efficiency_class)]<-"g"
levels(df_bin$energy_efficiency_class)
#tolgo il livello vuoto della virgola
df_bin$energy_efficiency_class <- droplevels(df_bin$energy_efficiency_class)

#ANNO DI COSTRUZIONE (rf)
hist(df_bin$year_of_construction)
sum(is.na(df_bin$year_of_construction))
table( df_bin$conditions, df_bin$year_of_construction, useNA = "always")
table(df_bin$year_of_construction, useNA = "always")
range(na.omit(df_bin)$year_of_construction)
df_bin<-as.data.frame(df_bin)
df_bin[df_bin$year_of_construction==2026,]

library(randomForest)
mod2<-randomForest(year_of_construction ~ conditions+energy_efficiency_class, data=na.omit(df_bin))
df_bin[is.na(df_bin$year_of_construction), "year_of_construction"]<-round(predict(mod2, df_bin[is.na(df_bin$year_of_construction),]))

#TASSE CONDOMINIALI imputo con RF
table(df_bin$condominium_fees, useNA = "always")
range(df_bin$condominium_fees)
df_bin$condominium_fees[is.na(df_bin$condominium_fees)]<-"NA"
df_bin[df_bin$condominium_fees=="No condominium fees","condominium_fees"]<-0
df_bin$condominium_fees<-as.numeric(df_bin$condominium_fees)

#imputo con random forest utilizzando come previsori: lift, total, car parking, floor, conidtions, heating_cnetr
#concierge, shared garde, receprion, 
cdfee<-df_bin[!is.na(df_bin$condominium_fees), ]%>% select(c("condominium_fees", "lift", "total_floors_in_building", "floor", "car_parking", 
                                                             "heating_centralized", "conditions", "reception", "concierge"))
cdnf<-df_bin[is.na(df_bin$condominium_fees), ]%>% select(c("condominium_fees", "lift", "total_floors_in_building", "floor", "car_parking", 
                                                           "heating_centralized", "conditions", "reception", "concierge"))
cdfee<-na.omit(cdfee)
cdfee<-as.data.frame(cdfee)
mod<-randomForest(condominium_fees ~ ., data=cdfee)

df_bin$condominium_fees[is.na(df_bin$condominium_fees)]<-predict(mod,df_bin[is.na(df_bin$condominium_fees), 
                                                                            c("condominium_fees", "lift", "total_floors_in_building", "floor", "car_parking",
                                                                              "heating_centralized", "conditions", "reception", "concierge")]) 
df_bin$condominium_fees<-round(df_bin$condominium_fees) 
df_bin<-df_bin%>%filter(condominium_fees<20000)

#ZONE, HO 1 SOLO NA
#Divo per frequenza, con 10 
df_bin$zone<-as.character(df_bin$zone)

df_bin10<-df_bin
freq_neigh <- table(df_bin10$zone)
freq_neigh
df_bin10$zone[df_bin10$zone %in% names(freq_neigh)[freq_neigh < 10]] <- "zone piccole"
df_bin10$zone[is.na(df_bin10$zone)] <- "zone piccole"
table(df_bin10$zone, useNA="always")

df_bin10<-df_bin10 %>%
  mutate(across(where(is.character), as.factor))

#quanti missing mi rimangono
freq_missing <- apply(df_bin10, 2, function(x) sum(is.na(x))) # Number of missing values
freq_missing[freq_missing > 0]

write.csv(data.frame(df_bin10), "preproc_zonepiccole10.csv", row.names = FALSE)

#Stesso con frequenza pari a 5
df_bin5<-df_bin
df_bin5$zone[df_bin5$zone %in% names(freq_neigh)[freq_neigh < 5]] <- "zone piccole"
df_bin5$zone[is.na(df_bin5$zone)] <- "zone piccole"
table(df_bin5$zone)

df_bin5<-df_bin5 %>%
  mutate(across(where(is.character), as.factor))

#quanti missing mi rimangono
freq_missing <- apply(df_bin5, 2, function(x) sum(is.na(x))) # Number of missing values
freq_missing[freq_missing > 0]

write.csv(data.frame(df_bin5), "preproc_zonepiccole5.csv", row.names = FALSE)

#Altra prova su zone: invece ceh toglier e le piccole faccio table, assegno le piu piccole a quelle piu vicine
table(train$zone)
levels(as.factor(train$zone))

tab<-table(train$zone)
livelli<-names(which(tab<=10))
#   "brera" e   brera eLANZA               
#   "cadorna - castello"  sempione   
#  "cascina gobba" crescenzago
#   "figino"         quinto romano        
#  "lanza"     brera eLANZA  
#   "parco lambro"   cimiano        
#  "qt8"      PORTELLO
#   "quadrilatero della moda" duomo
#  "rogoredo"       santa giulia 
#   "san babila"      DUOMO       
#  "sant'ambrogio"   famagosta
#   "scala - manzoni"   duomo     
#  "via calizzano"  comasina
#   "via canelli"   lamvrate         
# "via fra' cristoforo" famagosta

rilivellamento <- c(
  "brera" = "brera e lanza",
  "cadorna - castello" = "sempione",
  "cascina gobba" = "crescenzago",
  "figino" = "quinto romano",
  "lanza" = "brera e lanza",
  "parco lambro" = "cimiano",
  "qt8" = "portello - parco vittoria",
  "quadrilatero della moda" = "duomo",
  "rogoredo" = "santa giulia",
  "san babila" = "duomo",
  "sant'ambrogio" = "famagosta",
  "scala - manzoni" = "duomo",
  "via calizzano" = "comasina",
  "via canelli" = "lambrate",
  "via fra' cristoforo" = "famagosta"
)

# Applichiamo la sostituzione
zone_rilivellata <- ifelse(train$zone %in% names(rilivellamento), rilivellamento[train$zone], train$zone)
levels(as.factor(zone_rilivellata))




write.csv(data.frame(df_bin), "prep_mancazone.csv", row.names = FALSE)

#---------------------fine pre processing

#-------------------INIZIO ANALISI
library(tidyverse)

dati<-read.csv("preproc_zonepiccole10.csv", header=T)
dati<-read.csv("preproc_zonepiccole5.csv", header=T)
dati<-read.csv("prep_mancazone.csv", header=T)
dati<-read.csv("prenozone_sirilivel.csv", header=T)

str(dati)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))
dati[15:49]<-lapply(dati[15:49], as.factor)

skimr::skim(dati)

set.seed(123)
index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)
train<-dati[index, ]
val<-dati[-index,]


# Some initial plots ------------------------------------------------------------------------------------

par(mfrow = c(1, 1))
plot(train$square_meters, train$selling_price,
     xlab = "Ground living area", ylab = "Sale Price", pch = 16, cex = 0.8, col="blue"
)

plot(train$condominium_fees, train$selling_price, xlab = "Cond fees", ylab = "Sale Price", pch = 16, cex = 0.8)

plot(train$year_of_construction, train$selling_price,
     xlab = "Anno", ylab = "Sale Price", pch = 16, cex = 0.8)

boxplot(selling_price ~ bathrooms_number, data = train)
boxplot(log(selling_price) ~ lift, data = train)
boxplot(log(selling_price) ~ rooms_number, data = train)
boxplot(log(selling_price) ~ car_parking, data = train) #uguale per tutti
boxplot(log(selling_price) ~ conditions, data = train)
boxplot(log(selling_price) ~ zona, data = train) #uguale
boxplot(log(selling_price) ~ floor, data = train) #ugualo
boxplot(log(selling_price) ~ energy_efficiency_class, data = train)
boxplot(log(selling_price) ~ total_floors_in_building, data = train)
boxplot(log(selling_price) ~ optic.fiber, data = train)
boxplot(log(selling_price) ~ security.door, data = train) #un pochino
boxplot(log(selling_price) ~ balcony, data = train)
boxplot(log(selling_price) ~ cellar, data = train)
boxplot(log(selling_price) ~ video.entryphone, data = train)
boxplot(log(selling_price) ~ alarm.system, data = train) #uh pochino
boxplot(log(selling_price) ~ external.exposure , data = train) #boh sembra che 1 lo diminuisca
boxplot(log(selling_price) ~ closet, data = train)
boxplot(log(selling_price) ~ concierge, data = train) #un po cambia
boxplot(log(selling_price) ~ centralized.tv.system, data = train)

boxplot(log(selling_price) ~ partially.furnished, data = train)
boxplot(log(selling_price) ~ electric.gate, data = train)
boxplot(log(selling_price) ~ internal.exposure , data = train)
boxplot(log(selling_price) ~ furnished, data = train)
boxplot(log(selling_price) ~ shared.garden, data = train)
boxplot(log(selling_price) ~ terrace, data = train) #Si

boxplot(log(selling_price) ~ hydromassage, data = train) #si
boxplot(log(selling_price) ~ fireplace, data = train) #si
boxplot(log(selling_price) ~ kitchen , data = train) #no
boxplot(log(selling_price) ~ single.tv.system, data = train) #no
boxplot(log(selling_price) ~ tavern, data = train) #un po
boxplot(log(selling_price) ~ private.garden, data = train)#non troppoo

boxplot(log(selling_price) ~ only.kitchen.furnished, data = train) #un oicino
boxplot(log(selling_price) ~ attic, data = train)
boxplot(log(selling_price) ~ pool , data = train)#si
boxplot(log(selling_price) ~ tennis.court, data = train) #si
boxplot(log(selling_price) ~ tv.system.with.satellite.dish, data = train)
boxplot(log(selling_price) ~ reception, data = train) #si

boxplot(log(selling_price) ~ single_glass, data = train)
boxplot(log(selling_price) ~ double_glass, data = train)
boxplot(log(selling_price) ~ triple_glass , data = train)#si
boxplot(log(selling_price) ~ single_exposure, data = train)
boxplot(log(selling_price) ~ double_exposure, data = train) #si
boxplot(log(selling_price) ~ triple_exposure, data = train) #si


# Setting a benchmark ---------------------------------------------------------------------

MAE <- function(y, y_fit) {
  mean(abs(y - y_fit))
}

MSE <- function(y, y_fit) {
  mean((y - y_fit)^2)
}

MSLE <- function(y, y_fit) {
  mean((log(y) - log(y_fit))^2)
}

y_hat_median <- rep(median(train$selling_price), nrow(val)) # Prediction

round(MAE(val$selling_price, y_hat_median), 4) #256452.3
round(MSLE(val$selling_price, y_hat_median), 4) #0.3868

# A first simple model --------------------------------------------------------------------------
m_simple <- lm(selling_price ~ conditions + square_meters + year_of_construction + bathrooms_number, data = train)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = val)
summary(y_hat_simple)
# Perform a small correction:
y_hat_simple <- pmax(y_hat_simple, 0)

round(MAE(val$selling_price, y_hat_simple), 4) #164980.9
round(MSLE(val$selling_price, y_hat_simple), 4)

# Taking the log scale -----------------------------------------------------------------------------------
m_simple <- lm(log(selling_price) ~ conditions + log(square_meters) + log(year_of_construction) + bathrooms_number, data = train)
summary(m_simple)

# Re-obtain the original scale
y_hat_simple <- exp(predict(m_simple, newdata = val))

round(MAE(val$selling_price, y_hat_simple), 4) #168730.5
round(MSLE(val$selling_price, y_hat_simple), 4) #0.1431

#Modello con tutte le covariate
# How many variables are involved?
dim(model.matrix(log(selling_price) ~ ., data = train)[, -1])

# Linear regression model with all the covariates (some of them are going to be redundant!)
m_full <- lm(log(selling_price) ~. , data = train)
summary(m_full)

# 4 collinearities are due to "no basement", 3 collinearities are due to "no garage"
#ottengo Na quando ho collinearità

# Predictions for the full model. This command, due to collinearity, will produced warnings!
y_hat_full <- exp(predict(m_full, newdata = val))

round(MAE(val$selling_price, y_hat_full), 5) #137893 #104301 #
round(MSLE(val$selling_price, y_hat_full), 5) #0.08


# Forward and backward regression ----------------------------------------------------

library(leaps)

# Maximum number of covariates included in the list ()
dim(model.matrix(log(selling_price) ~ ., data = train)[, -1])
p_max <- 216 

# There are some collinear variables, therefore this will produce warnings!
m_forward <- regsubsets(log(selling_price) ~ .,
                        data = train, method = "forward", nbest = 1, nvmax = p_max, really.big = TRUE
)
sum_forward <- summary(m_forward)

m_backward <- regsubsets(log(selling_price) ~ .,
                         data = train, method = "backward", nbest = 1, nvmax = p_max
)
sum_backward <- summary(m_backward)

library(broom)
library(dplyr)
m_forward_summary <- m_forward %>%
  tidy() %>%
  rowwise() 

m_backward_summary <- m_backward %>%
  tidy() %>%
  rowwise()

# The official version of this function is bugged - fixed with an inefficient (!) tweak;
# Hopefully the original function coef.regsubsets will be fixed soon.
coef.regsubsets <- function(object, id, data){
  form <- as.formula(object[["call"]][[2]])
  s <- summary(object)
  y <- model.response(model.frame(form, data))
  X <- model.matrix(form, data)
  xvars <- names(which(s$which[id, ]))
  Xvars <- X[, xvars]
  beta_hat <- c(solve(crossprod(Xvars), crossprod(Xvars, y)))
  names(beta_hat) <- xvars
  beta_hat
}

# Let us see what happens at the lowest levels
which(sum_backward$which[1, ]) # Model with one covariate
which(sum_backward$which[2, ]) # Model with two covariates
which(sum_backward$which[3, ]) # Model with three covariates
which(sum_backward$which[4, ]) # Model with four covariates

round(coef(m_backward, 1, train), 6)
round(coef(m_backward, 2, train), 6)
round(coef(m_backward, 3, train), 6)
round(coef(m_backward, 4, train), 6)

# Coding time. Regsubsets does not have a "predict" method, we need to do it ourselves
predict.regsubsets <- function(object, data, newdata, id, ...) {
  form <- as.formula(object[["call"]][[2]])
  
  # Compute the design matrix
  X <- model.matrix(form, newdata)
  # Identify the correct beta coefficients
  beta_hat <- coef(object, id = id, data)
  xvars <- names(beta_hat)
  
  # Making the predictions
  pred_mat <- X[, xvars] %*% beta_hat
  
  # Housekeeping
  pred <- as.numeric(pred_mat)
  names(pred) <- rownames(X)
  pred
}

# Let see out it works
head(exp(predict(m_backward, data = train, newdata = train, id = 2)))

# Validation set - selection of p and performance comparisons ----------------------------------------
resid_back <- matrix(0, nrow(val), p_max + 1)
resid_log_back <- matrix(0, nrow(val), p_max + 1)

# We first comput the null model
resid_back[, 1] <- val$selling_price - exp(predict(lm(log(selling_price) ~ 1, data = train), newdata = val))
resid_log_back[, 1] <- log(val$selling_price) - predict(lm(log(selling_price) ~ 1, data = train), newdata = val)
#lo apllico a tutti i modelli
for (j in 2:(p_max)) {
  y_hat <- exp(predict(m_backward, data = train, newdata = val, j - 1))
  resid_back[, j] <- val$selling_price - y_hat
  #resid_log_back[, j] <- log(val$selling_price) - log(y_hat)
}

# Displaying the results
data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_back, 2, function(x) mean(abs(x))),
  #MSLE = apply(resid_log_back^2, 2, function(x) mean(x))
)

p_back_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_back_optimal #194
#ma in realta arivvo gia a p =25 a 150000

par(mfrow = c(1, 1))
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MAE(val$selling_price, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = p_back_optimal, lty = "dashed")
abline(h = MSLE(val$selling_price, y_hat_simple), lty = "dotted")
#in questo caso aumentando le covariate un po si riduce l'errore pero dato ceh si stabilizza
#vediamo dal grafico ceh anche se considero un numero prima di 98 ottengo 
#piu o meno lo stesso errore percui bo

# Optimal model on the validation set
y_hat_back <- exp(predict(m_backward, data = train, newdata = val, id = p_back_optimal))#uso modello ottimo

MAE(val$selling_price, y_hat_back)
MSLE(val$selling_price, y_hat_back)

# Principal components regression ----------------------------------------------------------------------

library(pls)

m_pcr <- pcr(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
             +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = train, center = TRUE, scale = TRUE)
summary(m_pcr)

resid_pcr <- matrix(0, nrow(val), p_max + 1)
resid_log_pcr <- matrix(0, nrow(val), p_max + 1)

# We first comput the null model
resid_pcr[, 1] <- resid_back[, 1]
resid_log_pcr[, 1] <- resid_log_back[, 1]

y_hat_pcr <- exp(predict(m_pcr, newdata = val))
for (j in 2:(p_max + 1)) {
  resid_pcr[, j] <- val$selling_price - y_hat_pcr[, , j - 1]
  resid_log_pcr[, j] <- log(val$selling_price) - log(y_hat_pcr[, , j - 1])
}

data_cv <- data.frame(
  p = 0:p_max,
  MAE = apply(resid_pcr, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_pcr^2, 2, function(x) mean(x))
)

#un modello con una componetne princs
#non va bene perche abbiamo che il modello migliore è l'ultimo modello

p_pcr_optimal <- data_cv$p[which.min(data_cv$MAE)]
p_pcr_optimal<-100


# Plots on the validation set
plot(data_cv$p, data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MAE(val$selling_price, y_hat_simple), lty = "dotted")

plot(data_cv$p, data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = p_pcr_optimal, lty = "dashed")
abline(h = MSLE(val$selling_price, y_hat_simple), lty = "dotted")

# Optimal model on the validation set
MAE(val$selling_price, y_hat_pcr[, , p_pcr_optimal]) #134830.8
#
MSLE(val$selling_price, y_hat_pcr[, , p_pcr_optimal])
#funziona bene quando sono tanto correlate le covariate

# Ridge regression ----------------------------------------------------------------------

library(glmnet)

# The lambda parameter can be then conveniently selected via cross-validation
X_shrinkage <- model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                            +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75)))-condominium_fees, data = train)[, -1]
y_shrinkage <- train$selling_price

# We need to set alpha = 0 to use the ridge
lambda_ridge_grid <- exp(seq(-6, 6, length = 100))
m_ridge <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
#se metto alpha=1 ottengo la LASSO

par(mfrow = c(1, 1))
plot(m_ridge, xvar = "lambda")
#stime dei coef in corrispondenza dei valori di lmabda

# How to select the "optimal" lambda?
resid_ridge <- matrix(0, nrow(val), length(lambda_ridge_grid))
resid_log_ridge <- matrix(0, nrow(val), length(lambda_ridge_grid))

y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                        +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = val)[, -1], s = lambda_ridge_grid))
for (j in 1:length(lambda_ridge_grid)) {
  resid_ridge[, j] <- val$selling_price - y_hat_ridge[, j]
  resid_log_ridge[, j] <- log(val$selling_price) - log(y_hat_ridge[, j])
}

data_cv <- data.frame(
  lambda = lambda_ridge_grid,
  MAE = apply(resid_ridge, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_ridge^2, 2, function(x) mean(x))
)

lambda_ridge_optimal <- lambda_ridge_grid[which.min(data_cv$MSLE)]
lambda_ridge_optimal

#lambda è vicino allo zero isgnifica ch e siamo vicini alle stime OLS
#only a minor penalization is needed

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_ridge_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_ridge_optimal), lty = "dashed")

# Optimal model on the validation set
y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                        +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = val)[, -1], s = lambda_ridge_optimal))

MAE(val$selling_price, y_hat_ridge) #81335.63
MSLE(val$selling_price, y_hat_ridge)

## Cross-validation for ridge regression
ridge_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
par(mfrow = c(1, 1))
plot(ridge_cv)

ridge_cv$lambda.min
ridge_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
ridge_cv$cvm[ridge_cv$index]

## LARS --------------------------------------------------------------------------
library(lars)

m_lar <- lars(X_shrinkage, log(y_shrinkage), type = "lar")

# Order of inclusion delle variabili, in terms o finterpretability works really well
m_lar

# Coefficient path
plot(m_lar, breaks = FALSE)
#prima increase e poi decrease significa che è correlata con le altre super correlata

plot(m_lar$df, m_lar$Cp, type = "b", xlab = "Degrees of freedom", ylab = "Cp of Mallow")
abline(v = which.min(m_lar$Cp))

y_hat_lar <- exp(predict(m_lar, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                    +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = val)[, -1], 
                         s = which.min(m_lar$Cp))$fit)

# Optimal model on the validation set
MAE(val$selling_price, y_hat_lar) #81038.17
MSLE(val$selling_price, y_hat_lar)

# Cross-validation for the lar
lar_cv <- cv.lars(X_shrinkage, log(y_shrinkage), plot.it = TRUE)

## Elastic-net -----------------------------------------------------------------------------------------

# We need to set (for example) alpha = 0.5 to select the elastic-net penalty. Any 0 < alpha < 1 would use an elastic-net penalty.
lambda_en_grid <- exp(seq(-10, 0, length = 100))
m_en <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)

# Coefficient path
plot(m_en, xvar = "lambda")

# How to select the "optimal" lambda?
resid_en <- matrix(0, nrow(val), length(lambda_en_grid))
resid_log_en <- matrix(0, nrow(val), length(lambda_en_grid))

y_hat_en <- exp(predict(m_en, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                  +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = val)[, -1], s = lambda_en_grid))
for (j in 1:length(lambda_en_grid)) {
  resid_en[, j] <- val$selling_price - y_hat_en[, j]
  resid_log_en[, j] <- log(val$selling_price) - log(y_hat_en[, j])
}

data_cv <- data.frame(
  lambda = lambda_en_grid,
  MAE = apply(resid_en, 2, function(x) mean(abs(x))),
  MSLE = apply(resid_log_en^2, 2, function(x) mean(x))
)

lambda_en_optimal <- lambda_en_grid[which.min(data_cv$MSLE)]
lambda_en_optimal

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_en_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_en_optimal), lty = "dashed")

# Optimal model on the validation set
y_hat_en <- exp(predict(m_en, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                  +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = val)[, -1], s = lambda_en_optimal))

MAE(val$selling_price, y_hat_en) #81041.36
MSLE(val$selling_price, y_hat_en)

## Cross-validation for elastic-net
en_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)

par(mfrow = c(1, 1))
plot(en_cv)

en_cv$lambda.min
en_cv$lambda.1se

# MSLE for lambda.min and lambda.1se
en_cv$cvm[en_cv$index]

# Group Lasso ---------------------------------------------------------------------------------------------
library(grpreg)
library(forcats)

#facccio la lasso considerando però un gruppo delle covariate in cui tutte le one hot encoding vengon shrinked to zero 
# insieme 
# Need to create the "groups"
group_times <- function(x){
  if(is.factor(x)){
    group_times <- length(fct_unique(x)) - 1
  } else {
    group_times <- 1
  }
  group_times
}

groups <- NULL
for(j in colnames(subset(train, select =c("zone_rilivellata")))){
  groups <- c(groups, rep(j, times = group_times(train[, j])))
}

# Estimation of the group lasso
lambda_grp_grid <- exp(seq(-11, 0, length = 100))
m_grp_lasso <- grpreg(X = X_shrinkage, y = log(y_shrinkage), group = groups, lambda = lambda_grp_grid)

# Coefficient path
plot(m_grp_lasso, log.l = TRUE)

## Cross-validation for elastic-net
set.seed(123)
grp_lasso_cv <- cv.grpreg(X_shrinkage, log(y_shrinkage), group = groups, lambda = lambda_grp_grid)

par(mfrow = c(1, 1))
# Cross-validation
plot(grp_lasso_cv)

# Optimal lambda value
lambda_grp_optimal <- grp_lasso_cv$lambda.min

# Optimal model on the validation set
y_hat_grp_lasso <- exp(predict(m_grp_lasso, X = model.matrix(selling_price ~ ., data = val)[, -1], 
                               lambda = lambda_grp_optimal))

MAE(val$selling_price, y_hat_grp_lasso)
MSLE(val$selling_price, y_hat_grp_lasso)


# Number of nonzero groups
predict(m_grp_lasso, type="ngroups", lambda=lambda_grp_optimal) 

# List of variables that have been selected
predict(m_grp_lasso, type="groups", lambda=lambda_grp_optimal) 

# Number of non-zero coefficients
predict(m_grp_lasso, type="nvars", lambda=lambda_grp_optimal)

# Comparison between group-lasso and elastic-net
cbind(coef(m_grp_lasso, lambda = lambda_grp_optimal),
      coef(m_en, s = lambda_en_optimal))

plot(coef(m_grp_lasso, lambda = lambda_grp_optimal)[-1],
     coef(m_en, s = lambda_en_optimal)[-1])


## Random forests (spoiler!) ------------------------------------------------------------------------------
library(ranger)
library(randomForest)
m_rf <- randomForest(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
               +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = train, num.trees = 2000, mtry = 10, max.depth = 30)
y_hat_rf <- exp(predict(m_rf, data = val, type = "response")$predictions)

MAE(val$selling_price, y_hat_rf) #116187.3
MSLE(val$selling_price, y_hat_rf)

##################
#Dataset con le zone divise nelle 20 zone fino al 1999 non funziona, quidni uso il dataset normale

#modello mettendo un po di cose a mano
set.seed(123)
mod <- lm(log(selling_price) ~ poly(square_meters,4) + zone + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
          + poly(condominium_fees, 3) + fireplace  + heating_centralized + triple_exposure + poly(floor, 4) +triple_glass + poly(total_floors_in_building, 3), data = train)

summary(mod)

# Predictions for the full model. This command, due to collinearity, will produced warnings!
y_mod<- exp(predict(mod, newdata = val))

round(MAE(val$selling_price, y_mod), 5) #85377.66 con zone <10 80314.43 #con zone <5 77154.61
round(MSLE(val$selling_price, y_mod), 5) #0.03618

#regressione gamma non funzione

#regressione quantili
install.packages("quantreg")
library(quantreg)

model_q<-rq(log(selling_price) ~ poly(square_meters,4) + zone + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
            + poly(condominium_fees, 3) + fireplace + heating_centralized + triple_exposure + poly(floor, 4) +triple_glass + poly(total_floors_in_building, 3), data = train,tau=0.5 )

#gradient boosting

library(gbm)
set.seed(123)

#Cross validation
mae<-c()

for (k in 1: 10){
  index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)  
  train<-dati[index, ]
  val<-dati[-index,]
  mod <- lm(log(selling_price) ~ poly(square_meters,4) + zone + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
            + poly(condominium_fees, 3) + fireplace  + heating_centralized + triple_exposure + poly(floor, 4) +triple_glass + poly(total_floors_in_building, 3), data = train)
  
  y_mod<- exp(predict(mod, newdata = val))
  mae[k]<-round(MAE(val$selling_price, y_mod), 5) 
}
 
#su questo modello : 
# lm(log(selling_price) ~ poly(square_meters,4) + zone + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
#    + poly(condominium_fees, 3) + fireplace  + heating_centralized + triple_exposure + poly(floor, 4) +triple_glass + poly(total_floors_in_building, 3), data = train)
#ottengo in crossvalidation questo MAE
# 77154.61 80263.81 81174.89 84952.80 82832.59 84996.42 78962.51 80225.74 78240.74 83081.75
#con seed 123 al primo colpa ottengo 77

#Ha senso unire le zone che non sono significative epr la previsione?
#magari valutare il modelo in altri modi ma boh contano sole le previsioni

#Provo a fare step backward del modello
p<-step(mod, direction=("backward"))
y_p<-exp(predict(p, newdata = val))
round(MAE(val$selling_price, y_p), 5)
summary(p)
#mi ha tolto fireplace, heating_centrali, hyfromassage, 


#FAccio crosvalidation completo con step all'interno di cv
mae<-c()
for (k in 1: 10){
  index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)  
  train<-dati[index, ]
  val<-dati[-index,]
  mod_full <- lm(log(selling_price) ~ poly(square_meters,4) + zone + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
            + poly(condominium_fees, 3) + fireplace  + heating_centralized + triple_exposure + poly(floor, 4) +triple_glass + poly(total_floors_in_building, 3), data = train)
  p<-step(mod, direction=("backward"))
  y_mod<- exp(predict(p, newdata = val))
  mae[k]<-round(MAE(val$selling_price, y_mod), 5) 
}
# 76780.02 118539.61  80300.32  79591.61  84684.93  83466.66  79427.47 81891.76 101669.56
#no vedi che peggiora se scelgo il modello di step percui io terrei il modello di prima 


#provo effetii random 
install.packages("lme4")
library(lme4)
mae<-c()
dati$square_meters<-scale(dati$square_meters)
dati$condominium_fees<-scale(dati$condominium_fees)
for (k in 1: 10){
  index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)  
  train<-dati[index, ]
  val<-dati[-index,]
  mod <- lmer(log(selling_price) ~ poly(square_meters,4) + zone   + (square_meters|zone) + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
            + poly(condominium_fees, 3) + fireplace  + heating_centralized + triple_exposure + poly(floor, 4) +triple_glass + poly(total_floors_in_building, 3), data = train)
  y_mod<- exp(predict(mod, newdata = val))
  mae[k]<-round(MAE(val$selling_price, y_mod), 5) 
}

#fa cagare

install.packages("mgcv")
library(mgcv)
mae<-c()

for (k in 1: 10){
  index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)  
  train<-dati[index, ]
  val<-dati[-index,]
  mod <- gamm(log(selling_price) ~ s(square_meters)  + pool + conditions + lift + rooms_number+ terrace+ security.door + hydromassage + bathrooms_number + concierge + reception +  car_parking
              + s(condominium_fees) + fireplace  + heating_centralized + triple_exposure + floor +triple_glass + total_floors_in_building, random = list (zone_rilivellata=~1), data = train)
  y_mod<- exp(predict(mod$gam, newdata = val))
  mae[k]<-round(MAE(val$selling_price, y_mod), 5) 
} #fa cagare


#
#Modello con splines al posto di ploy 
library(splines)
dati<-read.csv("preproc_zonepiccole10.csv", header=T)
dati<-read.csv("preproc_zonepiccole5.csv", header=T)
dati<-read.csv("prep_mancazone.csv", header=T)
dati<-read.csv("provepre.csv", header=T)

str(dati)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))
dati[16:51]<-lapply(dati[16:51], as.factor)
skimr::skim(dati)

set.seed(123)

index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)
train<-dati[index, ]
val<-dati[-index,]
mod <- lm(log(selling_price) ~ ns(square_meters,3) +  zone_rilivellata + alarm.system  + optic.fiber+ pool + conditions + lift + rooms_number + terrace + energy_efficiency_class+ security.door + tennis.court +  hydromassage + bathrooms_number + concierge + reception +  car_parking
          + ns(condominium_fees,3) + single_exposure+ double_exposure+ triple_exposure +floor +triple_glass + total_floors_in_building + condominio, data = train)

y_mod<- exp(predict(mod, newdata = val))
round(MAE(val$selling_price, y_mod), 5)  #78356.82

set.seed(123)
mae<-c()
aic<-c()
r<-c()
#aggiungo optic fiber e alarm system e tutte le esposizioni e tennis court e energy
for (k in 1: 10){
  index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)
  train<-dati[index, ]
  val<-dati[-index,]
  mod <- lm(log(selling_price) ~ ns(square_meters,3) +  zone_rilivellata + alarm.system  + optic.fiber+ pool + conditions + lift + rooms_number + terrace + energy_efficiency_class+ security.door + tennis.court +  hydromassage + bathrooms_number + concierge + reception +  car_parking
            + ns(condominium_fees,3) + single_exposure+ double_exposure+ triple_exposure +floor +triple_glass + total_floors_in_building + condominio, data = train)
  
  y_mod<- exp(predict(mod, newdata = val))
  r[k]<-summary(mod)$adj.r.squared
  mae[k]<-round(MAE(val$selling_price, y_mod), 5) 
  aic[k]<-AIC(mod)
}
# 78356.82 80627.49 84217.13 81334.03 83731.35 86584.26 77501.67 82519.63 82823.63 79976.94
#mae con dataset diversi 
#79141.39 80816.70 83182.88 84029.80 83990.30 85526.07 79587.48 80532.06 80753.96 83309.6

#Provo con le variabili della gio
str(dati)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))
dati[16:49]<-lapply(dati[16:49], as.factor)
skimr::skim(dati)

set.seed(123)
index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)
train<-dati[index, ]
val<-dati[-index,]
mod <- lm(log(selling_price) ~ poly(square_meters,4) +  zone   + optic.fiber+ pool + conditions + lift + rooms_number + terrace + energy_efficiency_class+ security.door +  hydromassage + bathrooms_number  
          + poly(condominium_fees,4) + fireplace+ balcony +floor + total_floors_in_building + condominio, data = train)

y_mod<- exp(predict(mod, newdata = val))
round(MAE(val$selling_price, y_mod), 5)
# con seed 123 e mio dataset ma con vvariabili della gio e splines invece di poly  MAE :78324.27 
# con seed 123 e mio dataset ma con vvariabili della gio e poly invece di splines  MAE:79299.62

set.seed(123)
mae<-c()
aic<-c()
r<-c()
for (k in 1: 10){
  index<-sample(1:nrow(dati), 0.7*nrow(dati), replace=F)  
  train<-dati[index, ]
  val<-dati[-index,]
  mod <- mod <- lm(log(selling_price) ~ poly(square_meters,4) +  zone   + optic.fiber+ pool + conditions + lift + rooms_number + terrace + energy_efficiency_class+ security.door +  hydromassage + bathrooms_number  
                   + poly(condominium_fees,4) + fireplace+ balcony +floor + total_floors_in_building + condominio, data = train)
  
  y_mod<- exp(predict(mod, newdata = val))
  r[k]<-summary(mod)$adj.r.squared
  mae[k]<-round(MAE(val$selling_price, y_mod), 5) 
  aic[k]<-AIC(mod)
}
# con seed 123 e mio dataset ma con vvariabili della gio e splines invece di poly  MAE: 
#78324.27 80594.23 84846.16 81453.03 83991.45 86158.17 78050.64 82819.50 82921.84 79962.26

# con seed 123 e mio dataset ma con variabili della gio e poly invece di splines  MAE:
#79299.62 81086.34 82975.49 83939.85 83884.59 85134.96 77912.60 84324.97 83090.34 79907.35



#Previsioni sul test
#Rimando su tutto ill test
dati<-read.csv("provepre.csv", header=T)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))
dati[16:51]<-lapply(dati[16:51], as.factor)

set.seed(123)
mod <- lm(log(selling_price) ~ ns(square_meters,3) +  zone + alarm.system  + optic.fiber+ pool + conditions + lift + rooms_number + terrace + energy_efficiency_class+ security.door + tennis.court +  hydromassage + bathrooms_number + concierge + reception +  car_parking
          + ns(condominium_fees,3) + single_exposure+ double_exposure+ triple_exposure +floor +triple_glass + total_floors_in_building + condominio, data = dati)
#test 
test<-read.csv("testpre.csv", header=T)
test<-test %>%
  mutate(across(where(is.character), as.factor))
colnames(test)
test[15:49]<-lapply(test[15:49], as.factor)

y_mod<- exp(predict(mod, newdata = test))

previsione<-data.frame(
  ID=1:4800,
  prediction=y_mod
)
write.csv(previsione, "previsioni1.csv", row.names=F)

#PRevisione 2
dati<-read.csv("provepre.csv", header=T)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))
dati[16:51]<-lapply(dati[16:51], as.factor)

set.seed(123)
mod <- lm(log(selling_price) ~ ns(square_meters,3) +  zone_rilivellata + alarm.system  + optic.fiber+ pool + conditions + lift + rooms_number + terrace + energy_efficiency_class+ security.door + tennis.court +  hydromassage + bathrooms_number + concierge +  car_parking
          + ns(condominium_fees,3) + single_exposure+ double_exposure+ triple_exposure +floor +triple_glass  + condominio, data = dati)
#test 
test<-read.csv("testpre.csv", header=T)
test<-test %>%
  mutate(across(where(is.character), as.factor))
colnames(test)
test[15:50]<-lapply(test[15:50], as.factor)

y_mod<- exp(predict(mod, newdata = test))

previsione<-data.frame(
  ID=1:4800,
  prediction=y_mod
)
write.csv(previsione, "previsioni2.csv", row.names=F)

#Previsioni 3
dati<-read.csv("pre_mod_completo.csv", header=T)
str(dati)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))

dati[15:51]<-lapply(dati[15:51], as.factor)

mod <- lm(log(selling_price) ~ bs(square_meters,3, knots =quantile(square_meters, probs = c(0.25,0.5,0.75))) +  zone_rilivellata   + 
            optic.fiber+ pool + conditions + lift + room_num + terrace + 
            energy_efficiency_class  +bath_num + concierge   + balcony +
            + bs(condominium_fees,3,knots =quantile(condominium_fees, probs = c(0.25,0.5,0.75))) 
          +floor + total_floors_in_building  + luxury + security, data = dati)
test<-read.csv("test_mod.csv", header=T)
test<-test %>%
  mutate(across(where(is.character), as.factor))

test[15:52]<-lapply(test[15:52], as.factor)

y_mod<- exp(predict(mod, newdata = test))

previsione<-data.frame(
  ID=1:4800,
  prediction=y_mod
)
write.csv(previsione, "previsioni3.csv", row.names=F)


#RIDGE
#Modelli
dati<-read.csv("pre_mod_parz.csv", header=T)
str(dati)
dati<-dati %>%
  mutate(across(where(is.character), as.factor))

dati[13:39]<-lapply(dati[13:39], as.factor)

set.seed(123)

library(glmnet)

# The lambda parameter can be then conveniently selected via cross-validation
X_shrinkage <- model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                            +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75)))-condominium_fees, data = dati)[, -1]
y_shrinkage <- dati$selling_price

# We need to set alpha = 0 to use the ridge
lambda_ridge_grid <- exp(seq(-6, 6, length = 100))
m_ridge <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0, lambda = lambda_ridge_grid)
#se metto alpha=1 ottengo la LASSO

par(mfrow = c(1, 1))
plot(m_ridge, xvar = "lambda")
#stime dei coef in corrispondenza dei valori di lmabda

# How to select the "optimal" lambda?
resid_ridge <- matrix(0, nrow(test), length(lambda_ridge_grid))
resid_log_ridge <- matrix(0, nrow(val), length(lambda_ridge_grid))

y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                        +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = val)[, -1], s = lambda_ridge_grid))


lambda_ridge_optimal <- lambda_ridge_grid[which.min(data_cv$MSLE)]
lambda_ridge_optimal

#lambda è vicino allo zero isgnifica ch e siamo vicini alle stime OLS
#only a minor penalization is needed

par(mfrow = c(1, 2))
plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
abline(v = log(lambda_ridge_optimal), lty = "dashed")

plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
abline(v = log(lambda_ridge_optimal), lty = "dashed")

# Optimal model on the validation set
y_hat_ridge <- exp(predict(m_ridge, newx = model.matrix(log(selling_price) ~.+ bs(square_meters, 3, quantile(square_meters, c(0.25,0.5,0.75))) - square_meters
                                                        +bs(condominium_fees, 3, quantile(condominium_fees, c(0.25,0.5,0.75))) -condominium_fees, data = test)[, -1], s = lambda_ridge_optimal))

MAE(val$selling_price, y_hat_ridge) #81335.63
MSLE(val$selling_price, y_hat_ridge)
