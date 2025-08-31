library(tidyverse)
library(skimr)
library(sf)
rm(list=ls());graphics.off()

setwd("G:/My Drive/Università/Magistrale/Secondo anno M/Data Mining")
source("funzioni.R")


#Caricamento dati---------------

data <- read.csv("training.csv", header = T, stringsAsFactors = T)
train <- data

skim(train)
sum(is.na(train))

#Studio della variabile risposta---------------
y_train <- train$selling_price
sum(is.na(y_train))

hist(y_train, freq = T, col = "white", main = "Distribuzione marginale variabile risposta")
hist(log(y_train), freq = T, col = "white", main = "Distribuzione marginale variabile risposta in scala log")


#Bathrooms number -----------------------------------------------

train$bathrooms_number
summary(train$bathrooms_number) #Ho 25 valori mancanti 
train[is.na(train$bathrooms_number), ]

library(vcd)
assocstats(table(train$bathrooms_number, train$rooms_number))
# 0.648


table(train$bathrooms_number, train$rooms_number)
#       1    2    3    4    5   5+
# 1   446 2619 1631  190   18    3
# 2     5  105 1293  867  207   63
# 3     1    0   39  156  136   89
# 3+    0    1    0   16   34   56


id_na <- which(is.na(train$bathrooms_number))
train[id_na, ]$bathrooms_number 

# effettuo imputazione sulla moda

for(i in id_na) {
  num_stanze <- train$rooms_number[i]
  if (!is.na(num_stanze)) {
    if (num_stanze == 1) {
      train$bathrooms_number[i] <- 1
    } else if (num_stanze == 2) {
      train$bathrooms_number[i] <- 1
    } else if (num_stanze == 3) {
      train$bathrooms_number[i] <- 2 
    } else if (num_stanze == 4) {
      train$bathrooms_number[i] <- 2
    } else if (num_stanze == 5) {
      train$bathrooms_number[i] <- 2
    } else if (num_stanze == "5+") { 
      train$bathrooms_number[i] <- 3 
    }
  } else {
    train$bathrooms_number[i] <- 1
  }
}


summary(train$bathrooms_number)
sum(is.na(train$bathrooms_number))


#lift------------------------------------------------------------- 

sum(is.na(train$lift))
#ho 141 NA. 
#Sicuramente osservo quanti piani ha la casa

assocstats(table(train$lift, train$floor))

# Contingency Coeff.: 0.308 





#other feature------------------------------------------------------
sum(is.na(train$other_features))
train <- train %>%
  mutate(other_feature_missing = ifelse(is.na(other_features), 1, 0))

all_features_list <- list()

for (i in 1:nrow(train)) {
  current_value_original <- train$other_features[i] 
  
  if (!is.na(current_value_original)) {
    current_value_processed <- tolower(current_value_original) # Converti in minuscolo
    
    if (current_value_processed != "") {
      
      current_value_processed <- gsub("(pvc)(double exposure)", "\\1 | \\2", current_value_processed)
      current_value_processed <- gsub("(pvc)(exposure)", "\\1 | \\2", current_value_processed)
      
      features_in_row <- trimws(unlist(strsplit(current_value_processed, "\\s*\\|\\s*")))
      features_in_row <- features_in_row[features_in_row != ""]
      all_features_list[[i]] <- features_in_row
    } else {
      
      all_features_list[[i]] <- character(0)
    }
  } else {
    all_features_list[[i]] <- character(0)
  }
}


unique_features <- unique(unlist(all_features_list))
unique_features <- unique_features[unique_features != ""]

for (feature in unique_features) {
  col_name <- gsub("\\s+|/|-", "_", feature) 
  col_name <- gsub("[^a-zA-Z0-9_]", "", col_name) 
  train[[paste0("feature_", col_name)]] <- 0 
}

str(train)
for (i in 1:nrow(train)) {
  if (length(all_features_list[[i]]) > 0) {
    for (feature_present in all_features_list[[i]]) {
      col_name_to_update <- gsub("\\s+|/|-", "_", feature_present)
      col_name_to_update <- gsub("[^a-zA-Z0-9_]", "", col_name_to_update)
      train[[paste0("feature_", col_name_to_update)]][i] <- 1
    }
  }
}
str(train)
unique_features


#Cosa posso accorpare? 

#feature balcony----------------------------------
#Sicuramente balconies inserendo il numero di balconi 

summary(as.factor(train$feature_8_balconies))
summary(as.factor(train$feature_6_balconies))
summary(as.factor(train$feature_balcony))

#Inserisco in balcony 

train %>%
  dplyr::filter(feature_6_balconies == 1) %>%
  dplyr::select(feature_6_balconies, feature_balcony)

train[train$feature_8_balconies == 1, "feature_balcony"] <- 1 
train[train$feature_6_balconies == 1, "feature_balcony"] <- 1

#Rimuovo le altre due 
train <- train %>%
  dplyr::select(-feature_8_balconies, -feature_6_balconies)

unique_features

train <- train %>%
  mutate(across(contains("feature"), as.factor))

train <- train %>%
  dplyr::select(-ID)

str(train)

train <- train %>%
  dplyr::select(-other_feature_missing)


#feature concierge---------------------------------------------------

train <- train %>%
  mutate(concierge = ifelse(feature_half_day_concierge == 1 | feature_full_day_concierge == 1 |feature_reception == 1, 1, 0))%>%
  dplyr::select(-feature_half_day_concierge, -feature_full_day_concierge)%>%
  dplyr::select(-feature_reception)




boxplot(log(train$selling_price) ~ train$concierge)


#Pensare se inserire o meno reception 



#floor------------------------------------------------------------------------

train %>%
  filter(floor == "mezzanine" & feature_attic == 1)

levels(train$floor)[10] <- "0"
levels(train$floor)[11] <- "0.5"
levels(train$floor)[12] <- "-1"

# train$floor <- as.ordered(as.character(train$floor))
train$rooms_number <- as.ordered(train$rooms_number)
train$bathrooms_number <- as.ordered(train$bathrooms_number)
# train$total_floors_in_building <- as.ordered(train$total_floors_in_building) 


#exposure----------------------------------------------------------------
train <- train %>%
  mutate(exposure = ifelse( feature_exposure_north_south == 1 | feature_exposure_east_west == 1 | feature_exposure_south_east == 1 | feature_exposure_north_west == 1|
                              feature_exposure_south_west == 1 | feature_exposure_north_east == 1, 2, ifelse(
                                feature_exposure_east == 1 | feature_exposure_west == 1 | feature_exposure_south == 1 | feature_exposure_north == 1, 1, ifelse(
                                  feature_exposure_north_east_west == 1 |feature_exposure_north_south_west == 1 | feature_exposure_north_south_east == 1 | feature_exposure_south_east_west == 1, 3, ifelse(
                                    feature_exposure_north_south_east_west == 1, 4, "Info not available"
                                  )
                                )
                              ) )) %>%
  dplyr::select(-contains("feature_exposure"))

#proprerty_land_balcony--------------------------------------------------

train %>%
  filter(feature_property_land1_balcony == 1)
#La rimuovo

train <- train %>%
  dplyr::select(-feature_property_land1_balcony)


#double exposure-----------------------------------------------------

train$exposure[train$feature_double_exposure == 1] <- "2"

train <- train %>%
  dplyr::select(-feature_double_exposure)


#internal_exposure-----------------------------------------
# train$exposure[train$feature_internal_exposure == 1] <- "Internal"
# train <- train %>%
#   dplyr::select(-feature_internal_exposure)

train$exposure <- as.factor(train$exposure)
# levels(train$exposure)[1:4] <- c("1 ext", "2 ext", "3 ext", "4 ext")
table(train$exposure)

#Ricontrollare se alcune avevano esposizione sia interna che esterna


#glass/window--------------------------------------


train <- train %>%
  mutate(
    glass = ifelse(
      feature_window_frames_in_glass___wood == 1 |
        feature_window_frames_in_glass___metal == 1 |
        feature_window_frames_in_glass___pvc == 1,
      "Single",
      ifelse(
        feature_window_frames_in_double_glass___pvc == 1 |
          feature_window_frames_in_double_glass___metal == 1 |
          feature_window_frames_in_double_glass___wood == 1,
        "double",
        ifelse(
          feature_window_frames_in_triple_glass___pvc == 1 |
            feature_window_frames_in_triple_glass___wood == 1 |
            feature_window_frames_in_triple_glass___metal == 1,
          "triple",
          "Info not avaible"
        )
      )
    )
  ) %>%
  mutate(
    material = ifelse(
      feature_window_frames_in_glass___wood == 1 |
        feature_window_frames_in_double_glass___wood == 1 |
        feature_window_frames_in_triple_glass___wood == 1,
      "wood",
      ifelse(
        feature_window_frames_in_glass___pvc == 1 |
          feature_window_frames_in_double_glass___pvc == 1 |
          feature_window_frames_in_triple_glass___pvc == 1,
        "pvc",
        ifelse(
          feature_window_frames_in_glass___metal == 1 |
            feature_window_frames_in_double_glass___metal == 1 |
            feature_window_frames_in_triple_glass___metal == 1,
          "metal",
          "Info not avaible"
        )
      )
    )
  )%>%
  dplyr::select(-contains("window"))

table(train$glass)
table(train$material)


#disable access-------------------------------------
# ho soltanto tre osservazioni 
summary(train$feature_disabled_access)

train[train$feature_disabled_access == 1, ]
boxplot(train$selling_price ~ train$feature_disabled_access)

#Rimuovo questa colonna perchè la reputo irrilevante 
train <- train %>%
  dplyr::select(-other_features, -feature_disabled_access)


#sistemo alcuni livelli ----------------------------
#Rendo numeriche le variabili che sono numeriche 

levels(train$total_floors_in_building)[levels(train$total_floors_in_building) == "1 floor"] <- "1"
levels(train$rooms_number)
levels(train$availability)

#avaibility-------------------------------------------------------------
#Osservo availability
summary(train$availability)

train$available <- as.factor((as.character(train$availability) == "available"))
levels(train$available) <- c("No", "Yes")

library(lubridate)

train$availability <- as.character(train$availability)

train <- train %>%
  mutate(
    availability_str = as.character(availability),
    available_2023 = factor(as.integer(grepl("2023", availability_str))),
    available_2024 = factor(as.integer(grepl("2024", availability_str))),
    available_2025 = factor(as.integer(grepl("2025", availability_str))),
    available_2026 = factor(as.integer(grepl("2026", availability_str)))
  ) %>%
  dplyr::select(-availability_str)

skim(train)

#Osservo car parking

#car parking-----------------------------------------------------

levels(train$car_parking)

# train <- train %>%
#   mutate(
#     car_parking_privato = factor(as.integer(grepl("garage/box", as.character(car_parking)))),
#     car_parking_shared = factor(as.integer(grepl("shared", as.character(car_parking)))),
#     no_car_parking = factor(as.integer(grepl("no", as.character(car_parking))))
#   ) %>%
#   dplyr::select(-car_parking)

table(train$car_parking, useNA = "always")
train$car_parking <- as.character(train$car_parking)

train$car_parking[train$car_parking=="1 in garage/box"]<-"box"
train$car_parking[train$car_parking=="2 in garage/box"]<-"box"
train$car_parking[train$car_parking=="5 in garage/box"]<-"box"
train$car_parking[train$car_parking=="1 in garage/box, 1 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="1 in garage/box, 2 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="1 in garage/box, 3 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="1 in garage/box, 5 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 1 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 2 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 3 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 6 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 7 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="7 in garage/box, 3 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 8 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="2 in garage/box, 16 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="1 in garage/box, 4 in shared parking"]<-"box, shared"
train$car_parking[train$car_parking=="1 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="2 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="7 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="6 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="10 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="20 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="3 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="4 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="5 in shared parking"]<-"shared"
train$car_parking[train$car_parking=="9 in shared parking"]<-"shared"

table(train$car_parking)



#lift--------------------------------------------
#Lift ha 141 NA
train[is.na(train$lift),]

train$total_floors_in_building <- as.numeric(train$total_floors_in_building)

train[is.na(train$total_floors_in_building), ]


train$lift[is.na(train$lift) & train$total_floors_in_building > 3] <- "yes"  
train$lift[is.na(train$lift) & train$total_floors_in_building <= 3] <- "no" 
summary(train$lift)
#Ho ancora NA perchè in total_floors_in_building ho NA.


#total floor-------------------------------------------

na_total_floor <- train[is.na(train$total_floors_in_building),]
id_na_total_floor <- which(is.na(train$total_floors_in_building))
summary(train$condominium_fees)



# Rendo numerica condoominium_fees
levels(train$condominium_fees)[levels(train$condominium_fees) == "No condominium fees"] <- 0
train$condominium_fees <- as.numeric(as.character(train$condominium_fees))

train$total_floors_in_building <- as.factor(train$total_floors_in_building)


# train <- train %>%
#   mutate(Condominio = ifelse(condominium_fees == 0, "No", "Yes"))


train$is_na_total_floor <- rep(0, nrow(train))
train$is_na_total_floor[id_na_total_floor] <- 1





train <- train %>% group_by(floor) %>%  
  mutate(
    total_floors_in_building = if_else(
      is.na(total_floors_in_building),
      calcola_moda(total_floors_in_building),
      total_floors_in_building
    )
  ) %>%
  ungroup()


sum(is.na(train$total_floors_in_building))

# library(VIM)
# train_subset <- train %>%
#   dplyr::select(total_floors_in_building, square_meters, condominium_fees, floor, 
#                 heating_centralized, lift)
# 
# n <- nrow(train)
# 
# impute_total_floors <- kNN(train_subset, variable = "total_floors_in_building", k =floor(sqrt(n)))
# 
# table(impute_total_floors$total_floors_in_building_imp)
# 
# (impute_total_floors[id_na_total_floor, ])


# total_floors_imputato <- impute_total_floors$total_floors_in_building
# sum(train$total_floors_in_building[-id_na_total_floor] == impute_total_floors$total_floors_in_building[-id_na_total_floor])
# length(id_na_total_floor)
# train$total_floors_in_building <- impute_total_floors$total_floors_in_building

summary(train$total_floors_in_building)
str(train$total_floors_in_building)
train$total_floors_in_building <- as.numeric(as.character(train$total_floors_in_building))


#lift------------------------------------------------- 


train$lift[is.na(train$lift) & train$total_floors_in_building >= 3] <- "yes"  
train$lift[is.na(train$lift) & train$total_floors_in_building < 3] <- "no" 
sum(is.na(train$lift))

#Non ho più na

#Variabile conditions 

levels(train$conditions)
table(train$conditions)


#conditions-------------------------------------------------

table(train$energy_efficiency_class, train$conditions)

train[train$available == "No", "conditions"]
levels(train$conditions) <- c(levels(train$conditions), "Information not available")
train$conditions[is.na(train$conditions)] <- "Information not available"
train$conditions[is.na(train$conditions)] <- "Information not available"
summary(train$conditions) #Non ho più NA


id_na_conditions <- which(is.na(train$conditions))

train[id_na_conditions,]

train <- train %>%
  dplyr::select(-availability)


#heating_centralized----------------------------------------------------------

#Metto riscaldamento indipendente tutte le osservazioni che hanno o il camino o un numero di piani pari a 1 o con una piscina o un attico 
train$heating_centralized[is.na(train$heating_centralized) & train$total_floors_in_building == "1"] <- "independent"
train$heating_centralized[is.na(train$heating_centralized) & train$feature_fireplace == "1"] <- "independent"
train$heating_centralized[is.na(train$heating_centralized) & train$feature_attic == "1"] <- "independent"
train$heating_centralized[is.na(train$heating_centralized) & train$feature_tennis_court == "1"] <- "independent"
train$heating_centralized[is.na(train$heating_centralized) & train$feature_pool == "1"] <- "independent"
train$heating_centralized[is.na(train$heating_centralized) & train$condominium_fees == 0] <- "independent"
train$heating_centralized[is.na(train$heating_centralized)] <- "central" #da riguardare



#efficiency class--------------------------------------

boxplot(train$condominium_fees ~ train$heating_centralized)
boxplot(train$year_of_construction ~ train$heating_centralized)
mosaicplot(table(train$heating_centralized, train$total_floors_in_building))
table(train$heating_centralized, train$energy_efficiency_class)
train[is.na(train$energy_efficiency_class), ]
table(train$energy_efficiency_class, train$year_of_construction)


levels(train$energy_efficiency_class)[levels(train$energy_efficiency_class) == ","] <- "Info not available"
train$energy_efficiency_class[is.na(train$energy_efficiency_class)] <- "Info not available"




table(train$available)
train$available[is.na(train$available)] <- "Yes" #Imputo con la moda

table(train$zone)
sum(is.na(train$zone))
train[is.na(train$zone),]


#condominium feeessss-------------------------------


#Suppongo siano case indipendenti
train$condominium_fees[is.na(train$condominium_fees) & train$total_floors_in_building == "1"] <- 0

id_na_condominio_fees <- which(is.na(train$condominium_fees))

train$is_na_condominio_fees <- rep(0, nrow(train))
train$is_na_condominio_fees[id_na_condominio_fees] <- 1



train <- train %>%
  mutate(Condominio = ifelse(condominium_fees == 0 & total_floors_in_building <= 3, "No", "Yes")) %>%
  mutate(Condominio = as.factor(Condominio))

train$Condominio[is.na(train$Condominio) & train$total_floors_in_building == 1] <- "No"
train$Condominio[is.na(train$Condominio) & train$total_floors_in_building == 2] <- "No"
train$Condominio[is.na(train$Condominio) & train$total_floors_in_building > 2] <- "Yes"

mod_condominium_fees <- lm(log(condominium_fees+1) ~ concierge + Condominio + feature_tennis_court + feature_pool+
                             feature_electric_gate + feature_video_entryphone + heating_centralized + energy_efficiency_class + lift, data = train[-id_na_condominio_fees, ])



summary(mod_condominium_fees)


pred_condominio <- predict(mod_condominium_fees, newdata = train[-id_na_condominio_fees, ])
pred <- round(exp(pred_condominio)-1)
length(which(is.na(pred)))
train[which(is.na(pred)), ]


train$condominium_fees[id_na_condominio_fees] <- round(exp(predict(mod_condominium_fees, train[id_na_condominio_fees, ])+1))

summary(train)


out <- which(train$condominium_fees > 12000)
train[out,]
train$condominium_fees[out] <- train$condominium_fees[out]/1000


#year of construction-----------------------------
summary(train$year_of_construction)

train %>%
  filter(year_of_construction == 1111)

id_year_na <- which(is.na(train$year_of_construction))
train$is_na_year <- rep(0, nrow(train))
train$is_na_year[id_year_na] <- 1 

train$year_of_construction[id_year_na] <-  1960

str(train$year_of_construction)
# train <- train %>% mutate(
#   year_of_construction = case_when(
#     year_of_construction < 1920 ~ "Prima del 1920",
#     between(year_of_construction, 1920, 1929) ~ "Anni 20",
#     between(year_of_construction, 1930, 1939) ~ "Anni 30",
#     between(year_of_construction, 1940, 1949) ~ "Anni 40",
#     between(year_of_construction, 1950, 1959) ~ "Anni 50",
#     between(year_of_construction, 1960, 1969) ~ "Anni 60",
#     between(year_of_construction, 1970, 1979) ~ "Anni 70",
#     between(year_of_construction, 1980, 1989) ~ "Anni 80",
#     between(year_of_construction, 1990, 1999) ~ "Anni 90",
#     between(year_of_construction, 2000, 2009) ~ "Anni 2000",
#     between(year_of_construction, 2010, 2019) ~ "Anni 2010",
#     between(year_of_construction, 2020, 2025) ~ "2020-Oggi",
#     TRUE ~ "Info not available"
#   )
# ) %>% mutate(year_of_construction = as.factor(year_of_construction))





summary(train$year_of_construction)

boxplot(train$selling_price~train$year_of_construction)


#floor------------------------------------------------------




train$floor <- as.numeric(as.character(train$floor))
train$total_floors_in_building <- as.numeric(as.character(train$total_floors_in_building))
train[which(as.numeric(as.character(train$floor)) > as.numeric(as.character(train$total_floors_in_building))), "total_floors_in_building"] <- train$floor[which(as.numeric(as.character(train$floor)) > as.numeric(as.character(train$total_floors_in_building)))] +1 



train <- train %>%
  mutate(floor_rate = as.numeric(floor)/(as.numeric(total_floors_in_building)-2+0.1)) 

#Condominio--------------------------------------------
train <- train %>%
  mutate(Condominio = ifelse(condominium_fees == 0 & total_floors_in_building <= 3, "No", "Yes"))

boxplot(train$selling_price ~train$Condominio)
table(train$Condominio) #Sono pochissimi 

#square_meters-------------------------

#Osservo la variabile square_meters
hist(train$square_meters)
boxplot(train$square_meters~train$rooms_number) #Ovviamente all'aumentare del numero di stanze aumenta square meters
summary(train$square_meters) 
err_sqmt <- which(train$square_meters < 20)

train <- train[-err_sqmt, ]

#Modello lineare per aggiustare square meters
# 
# mod_sqmtrs <- lm(log(square_meters) ~ rooms_number + bathrooms_number, data = train[-err_sqmt, ])
# pred_sqmtr <- exp(predict(mod_sqmtrs, train[err_sqmt,]))
# train$square_meters[err_sqmt] <- pred_sqmtr


hist(train$square_meters, freq = F, col = "white")
summary(train$square_meters)


levels(train$zone)


length(which(train$feature_external_exposure == 1 & train$exposure == "Info not available"))


table(train$exposure, train$feature_external_exposure)

# levels(train$exposure) <- c(levels(train$exposure), "External")
# train$exposure[which(train$feature_external_exposure == 1 & train$exposure == "Info not available")] <-  "External"




#altre variabili-------------------------------------------------------------
skim(train)

table(train$feature_optic_fiber)
table(train$feature_alarm_system)

table(train$available) #Poche 
table(train$available_2023) #Solo 32
table(train$available_2024) # 173
table(train$available_2025) #38
table(train$available_2026) # 37 

#Tengo soltanto available 

train <- train %>%
  dplyr::select(-available_2023, -available_2024, -available_2025, -available_2026)

table(train$conditions)

table(train$feature_furnished, train$conditions)
table(train$feature_partially_furnished, train$feature_furnished)
table(train$feature_only_kitchen_furnished, train$feature_furnished)


table(train$conditions)

table(train$feature_furnished, train$conditions)
table(train$feature_partially_furnished, train$feature_furnished)
table(train$feature_only_kitchen_furnished, train$feature_furnished)


train <- train %>%
  mutate(furnished = ifelse(feature_furnished  == 1, "yes", 
                            ifelse(feature_only_kitchen_furnished == 1, "only kitchen",
                                   ifelse(feature_partially_furnished == 1, "partially", "none"))))%>%
  mutate(furnished = as.factor(furnished))%>%
  dplyr::select(-feature_furnished, -feature_only_kitchen_furnished, -feature_partially_furnished)


names(train)



luxury <- train[, c("feature_pool", "feature_tennis_court", "concierge",
                    "feature_hydromassage")]

luxury <- as.data.frame(apply(luxury, 2, as.numeric))
luxury <- rowSums((luxury))
train$luxury <- as.factor(luxury) 


optional <- train[,c("feature_security_door", "feature_electric_gate", "feature_fireplace",
                     "feature_optic_fiber", "feature_cellar", "feature_video_entryphone", "feature_alarm_system",
                     "feature_centralized_tv_system", "feature_single_tv_system", "feature_tv_system_with_satellite_dish")]

optional <- as.data.frame(apply(optional, 2, as.numeric))
optional <- rowSums((optional))
train$optional <- as.factor(optional) 

#Considero le variabili numeriche

#considero nuova risposta

train <- train %>%
  mutate(prezzo_mq = selling_price/square_meters)


hist(train$total_floors_in_building)
hist((train$total_floors_in_building))
summary(train$floor_rate)

train %>%
  filter(floor_rate < -1 |
           floor_rate > 1)

table(train$floor)
table(train$total_floors_in_building)



train <- train %>%
  mutate(
    floor_rate = case_when(
      total_floors_in_building == 0 | floor > total_floors_in_building ~ NA_real_,
      total_floors_in_building == 1 ~ 1.0,
      TRUE ~ floor / total_floors_in_building
    )
  )


hist(train$floor_rate, freq = F, col = "white")
plot(x = (train$floor_rate), y = log(train$prezzo_mq))


train <- train %>%
  mutate(
    floor_category = case_when(
      floor < 0 ~ "Basement",
      floor == 0 ~ "Ground_Floor",
      floor_rate == 1 & total_floors_in_building > 1 ~ "Penthouse",
      floor_rate > 0.66 ~ "High_Floor",
      floor_rate > 0.33 ~ "Mid_Floor",
      floor_rate > 0 ~ "Low_Floor",
      TRUE ~ "Other" 
    )
  )

#zone-----------------------------------------------------------------------

train$zone <- as.character(train$zone)

relevel_zone <- c(
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
  "via fra' cristoforo" = "famagosta", 
  "corso magenta"= "sempione",
  "largo caioroli 2"= "sempione",
  "via marignano, 3"="santa giulia"
)




train <- train %>%
  mutate(zone = ifelse(train$zone %in% names(relevel_zone), relevel_zone[train$zone], train$zone))
train <- train%>%filter(!is.na(train$zone))



summary(train)

sort(unique(train$zone))

train <- train %>%
  mutate(zone = str_to_upper(zone))

sort(unique(train$zone))

train$zone <- paste((train$zone), "Milano,Italia") 

train %>%
  filter(zone == "OTHER")

train$zone[train$zone == "MOLISE - CUOCO Milano,Italia"] <- "CUOCO Milano,Italia"
train$zone[train$zone == "BOLOGNA - SULMONA Milano,Italia"] <- "SULMONA Milano,Italia"
train$zone[train$zone == "SAN VITTORE Milano,Italia"] <- "SAN VITTORE carcere Milano, Italia"
train$zone[train$zone == "CERMENATE - ABBIATEGRASSO Milano,Italia"] <- "PIAZZA ABBIATEGRASSO Milano, Italia"
train$zone[train$zone == "PONTE NUOVO Milano,Italia"] <- "PONTE NUOVO quartiere Milano,Italia"

library(tidygeocoder)
coordinate <- geo(train$zone, method = "arcgis")
summary(coordinate)
sort(unique(coordinate$address))

coordinate_unique <- coordinate %>%
  rename(zone = address) %>%  
  group_by(zone) %>%       
  slice(1) %>%             
  ungroup()               


train <- train %>%
  left_join(coordinate_unique, by = "zone")

sort(unique(train$zone))


duomo <- geo(address = "Duomo, Milano, Italy", method = "arcgis")


library(geosphere)


train$lat[train$zone == "PONTE NUOVO quartiere Milano,Italia"] <- 45.47583
train$long[train$zone == "PONTE NUOVO quartiere Milano,Italia"] <- 9.19694                                                                    


train <- train %>%
  mutate(
    distanza_duomo_metri = distHaversine(
      p1 = cbind(long, lat),  
      p2 = cbind(duomo$long, duomo$lat)    
    )
  )


train[train$long > 10, ]

train$log_dist <- log(train$distanza_duomo_metri+1)

summary(train)

ggplot(train, aes(x = ((distanza_duomo_metri)), y = log(selling_price)))+
  geom_point()+
  theme(legend.position = "none")+
  geom_smooth()

train %>%
  filter(distanza_duomo_metri > 10000)


university_zones <- c("CITTÀ STUDI Milano,Italia",
                      "BOVISA Milano,Italia", "BICOCCA Milano,Italia",
                      "NAVIGLI - DARSENA Milano,Italia",
                      "PORTA ROMANA - MEDAGLIE D'ORO Milano,Italia", 
                      "LAMBRATE Milano,Italia",
                      "SAN VITTORE carcere Milano, Italia", 
                      "SEMPIONE Milano,Italia")
train$zona_universitaria <- ifelse(train$zone %in% university_zones, 1, 0)
train$zona_universitaria <- as.factor(train$zona_universitaria)

train$Condominio <- as.factor(train$Condominio)
train$Condominio <- relevel(train$Condominio, ref = "Yes")

boxplot(train$selling_price ~ train$feature_attic)
train %>%
  filter(feature_attic == 1) %>% select(floor, total_floors_in_building)


train <- train %>%
  mutate(
    mq_x_pool = log(square_meters) * (as.numeric(feature_pool)-1),
    mq_x_bath = log(square_meters) * as.numeric(bathrooms_number)
  )

save(train, file = "train_preprocessed.RData")


#Fine preprocessing----------------------------------------------



####################################Test---------------------------------------------------------------



library(tidyverse)
library(skimr)
library(sf)
rm(list = setdiff(ls(), c("mod_condominium_fees")))


setwd("G:/My Drive/Università/Magistrale/Secondo anno M/Data Mining")

source("funzioni.R")

#Caricamento dati---------------

data <- read.csv("test.csv", header = T, stringsAsFactors = T)
test <- data



#Bathrooms number -----------------------------------------------

test$bathrooms_number
summary(test$bathrooms_number) #Ho 25 valori mancanti 
test[is.na(test$bathrooms_number), ]
id_na <- which(is.na(test$bathrooms_number))
test[id_na, ]$bathrooms_number 

# effettuo imputazione sulla moda

for(i in id_na) {
  num_stanze <- test$rooms_number[i]
  if (!is.na(num_stanze)) {
    if (num_stanze == 1) {
      test$bathrooms_number[i] <- 1
    } else if (num_stanze == 2) {
      test$bathrooms_number[i] <- 1
    } else if (num_stanze == 3) {
      test$bathrooms_number[i] <- 2 
    } else if (num_stanze == 4) {
      test$bathrooms_number[i] <- 2
    } else if (num_stanze == 5) {
      test$bathrooms_number[i] <- 2
    } else if (num_stanze == "5+") { 
      test$bathrooms_number[i] <- 3 
    }
  } else {
    test$bathrooms_number[i] <- 1
  }
}


summary(test$bathrooms_number)
sum(is.na(test$bathrooms_number))




#other feature------------------------------------------------------
sum(is.na(test$other_features))
test <- test %>%
  mutate(other_feature_missing = ifelse(is.na(other_features), 1, 0))

all_features_list <- list()

for (i in 1:nrow(test)) {
  current_value_original <- test$other_features[i] 
  
  if (!is.na(current_value_original)) {
    current_value_processed <- tolower(current_value_original) # Converti in minuscolo
    
    if (current_value_processed != "") {
      
      current_value_processed <- gsub("(pvc)(double exposure)", "\\1 | \\2", current_value_processed)
      current_value_processed <- gsub("(pvc)(exposure)", "\\1 | \\2", current_value_processed)
      
      features_in_row <- trimws(unlist(strsplit(current_value_processed, "\\s*\\|\\s*")))
      features_in_row <- features_in_row[features_in_row != ""]
      all_features_list[[i]] <- features_in_row
    } else {
      
      all_features_list[[i]] <- character(0)
    }
  } else {
    all_features_list[[i]] <- character(0)
  }
}


unique_features <- unique(unlist(all_features_list))
unique_features <- unique_features[unique_features != ""]

for (feature in unique_features) {
  col_name <- gsub("\\s+|/|-", "_", feature) 
  col_name <- gsub("[^a-zA-Z0-9_]", "", col_name) 
  test[[paste0("feature_", col_name)]] <- 0 
}

str(test)
for (i in 1:nrow(test)) {
  if (length(all_features_list[[i]]) > 0) {
    for (feature_present in all_features_list[[i]]) {
      col_name_to_update <- gsub("\\s+|/|-", "_", feature_present)
      col_name_to_update <- gsub("[^a-zA-Z0-9_]", "", col_name_to_update)
      test[[paste0("feature_", col_name_to_update)]][i] <- 1
    }
  }
}
str(test)
unique_features


test <- test %>%
  select(-feature_alarm_system1_balcony)


#feature balcony----------------------------------
#Sicuramente balconies inserendo il numero di balconi 

summary(as.factor(test$feature_1_balcony))
summary(as.factor(test$feature_balcony))

#Inserisco in balcony 

test %>%
  dplyr::filter(feature_1_balcony == 1) %>%
  dplyr::select(feature_1_balcony, feature_balcony)

test[test$feature_1_balcony == 1, "feature_balcony"] <- 1 

#Rimuovo le altre due 
test <- test %>%
  dplyr::select(-feature_1_balcony)

unique_features

test <- test %>%
  mutate(across(contains("feature"), as.factor))

test <- test %>%
  dplyr::select(-ID)

str(test)

test <- test %>%
  dplyr::select(-other_feature_missing)


#feature concierge---------------------------------------------------

test <- test %>%
  mutate(concierge = ifelse(feature_half_day_concierge == 1 | feature_full_day_concierge == 1 |feature_reception == 1, 1, 0))%>%
  dplyr::select(-feature_half_day_concierge, -feature_full_day_concierge)%>%
  dplyr::select(-feature_reception)





#floor------------------------------------------------------------------------

test %>%
  filter(floor == "mezzanine" & feature_attic == 1)

levels(test$floor)[levels(test$floor) == "ground floor"] <- "0"
levels(test$floor)[levels(test$floor) == "mezzanine"] <- "0.5"
levels(test$floor)[levels(test$floor) == "semi-basement"] <- "-1"

# test$floor <- as.ordered(as.character(test$floor))
test$rooms_number <- as.ordered(test$rooms_number)
test$bathrooms_number <- as.ordered(test$bathrooms_number)
# test$total_floors_in_building <- as.ordered(test$total_floors_in_building) 


#exposure----------------------------------------------------------------
test <- test %>%
  mutate(exposure = ifelse( feature_exposure_north_south == 1 | feature_exposure_east_west == 1 | feature_exposure_south_east == 1 | feature_exposure_north_west == 1|
                              feature_exposure_south_west == 1 | feature_exposure_north_east == 1, 2, ifelse(
                                feature_exposure_east == 1 | feature_exposure_west == 1 | feature_exposure_south == 1 | feature_exposure_north == 1, 1, ifelse(
                                  feature_exposure_north_east_west == 1 |feature_exposure_north_south_west == 1 | feature_exposure_north_south_east == 1 | feature_exposure_south_east_west == 1, 3, ifelse(
                                    feature_exposure_north_south_east_west == 1, 4, "Info not available"
                                  )
                                )
                              ) )) %>%
  dplyr::select(-contains("feature_exposure"))




#double exposure-----------------------------------------------------

test$exposure[test$feature_double_exposure == 1] <- "2"

test <- test %>%
  dplyr::select(-feature_double_exposure)


#internal_exposure-----------------------------------------
# test$exposure[test$feature_internal_exposure == 1] <- "Internal"
# test <- test %>%
#   dplyr::select(-feature_internal_exposure)

test$exposure <- as.factor(test$exposure)
# levels(test$exposure)[1:4] <- c("1 ext", "2 ext", "3 ext", "4 ext")
table(test$exposure)

levels(test$exposure)[levels(test$exposure) == "4"] <- "3"

#Ricontrollare se alcune avevano esposizione sia interna che esterna


#glass/window--------------------------------------


test <- test %>%
  mutate(
    glass = ifelse(
      feature_window_frames_in_glass___wood == 1 |
        feature_window_frames_in_glass___metal == 1 |
        feature_window_frames_in_glass___pvc == 1,
      "Single",
      ifelse(
        feature_window_frames_in_double_glass___pvc == 1 |
          feature_window_frames_in_double_glass___metal == 1 |
          feature_window_frames_in_double_glass___wood == 1,
        "double",
        ifelse(
          feature_window_frames_in_triple_glass___pvc == 1 |
            feature_window_frames_in_triple_glass___wood == 1 |
            feature_window_frames_in_triple_glass___metal == 1,
          "triple",
          "Info not avaible"
        )
      )
    )
  ) %>%
  mutate(
    material = ifelse(
      feature_window_frames_in_glass___wood == 1 |
        feature_window_frames_in_double_glass___wood == 1 |
        feature_window_frames_in_triple_glass___wood == 1,
      "wood",
      ifelse(
        feature_window_frames_in_glass___pvc == 1 |
          feature_window_frames_in_double_glass___pvc == 1 |
          feature_window_frames_in_triple_glass___pvc == 1,
        "pvc",
        ifelse(
          feature_window_frames_in_glass___metal == 1 |
            feature_window_frames_in_double_glass___metal == 1 |
            feature_window_frames_in_triple_glass___metal == 1,
          "metal",
          "Info not avaible"
        )
      )
    )
  )%>%
  dplyr::select(-contains("window"))

table(test$glass)
table(test$material)


#disable access-------------------------------------
# ho soltanto tre osservazioni 
summary(test$feature_disabled_access)

test[test$feature_disabled_access == 1, ]


#Rimuovo questa colonna perchè la reputo irrilevante 
test <- test %>%
  dplyr::select(-other_features, -feature_disabled_access)


#sistemo alcuni livelli ----------------------------
#Rendo numeriche le variabili che sono numeriche 

levels(test$total_floors_in_building)[levels(test$total_floors_in_building) == "1 floor"] <- "1"
levels(test$rooms_number)
levels(test$availability)

#avaibility-------------------------------------------------------------
#Osservo availability
summary(test$availability)

test$available <- as.factor((as.character(test$availability) == "available"))
levels(test$available) <- c("No", "Yes")

library(lubridate)

test$availability <- as.character(test$availability)

test <- test %>%
  mutate(
    availability_str = as.character(availability),
    available_2023 = factor(as.integer(grepl("2023", availability_str))),
    available_2024 = factor(as.integer(grepl("2024", availability_str))),
    available_2025 = factor(as.integer(grepl("2025", availability_str))),
    available_2026 = factor(as.integer(grepl("2026", availability_str)))
  ) %>%
  dplyr::select(-availability_str)

skim(test)

#Osservo car parking

#car parking-----------------------------------------------------

levels(test$car_parking)

# test <- test %>%
#   mutate(
#     car_parking_privato = factor(as.integer(grepl("garage/box", as.character(car_parking)))),
#     car_parking_shared = factor(as.integer(grepl("shared", as.character(car_parking)))),
#     no_car_parking = factor(as.integer(grepl("no", as.character(car_parking))))
#   ) %>%
#   dplyr::select(-car_parking)

table(test$car_parking, useNA = "always")
test$car_parking <- as.character(test$car_parking)

test$car_parking[test$car_parking=="1 in garage/box"]<-"box"
test$car_parking[test$car_parking=="2 in garage/box"]<-"box"
test$car_parking[test$car_parking=="5 in garage/box"]<-"box"
test$car_parking[test$car_parking=="1 in garage/box, 1 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="1 in garage/box, 2 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="1 in garage/box, 3 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="1 in garage/box, 5 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 1 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 2 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 3 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 6 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 7 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="7 in garage/box, 3 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 8 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="2 in garage/box, 16 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="1 in garage/box, 4 in shared parking"]<-"box, shared"
test$car_parking[test$car_parking=="1 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="2 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="7 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="6 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="10 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="20 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="3 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="4 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="5 in shared parking"]<-"shared"
test$car_parking[test$car_parking=="9 in shared parking"]<-"shared"

table(test$car_parking)



#lift--------------------------------------------
#Lift ha 141 NA
test[is.na(test$lift),]

test$total_floors_in_building <- as.numeric(test$total_floors_in_building)

test[is.na(test$total_floors_in_building), ]


test$lift[is.na(test$lift) & test$total_floors_in_building > 3] <- "yes"  
test$lift[is.na(test$lift) & test$total_floors_in_building <= 3] <- "no" 
summary(test$lift)
#Ho ancora NA perchè in total_floors_in_building ho NA.


skim(test)

#total floor-------------------------------------------

na_total_floor <- test[is.na(test$total_floors_in_building),]
id_na_total_floor <- which(is.na(test$total_floors_in_building))
summary(test$condominium_fees)

test$is_na_total_floor <- rep(0, nrow(test))
test$is_na_total_floor[id_na_total_floor] <- 1



# Rendo numerica condoominium_fees
levels(test$condominium_fees)[levels(test$condominium_fees) == "No condominium fees"] <- 0
test$condominium_fees <- as.numeric(as.character(test$condominium_fees))

test$total_floors_in_building <- as.factor(test$total_floors_in_building)


test <- test %>%
  mutate(Condominio = ifelse(condominium_fees == 0, "No", "Yes"))

# library(VIM)
# 
# 
load("train_preprocessed.RData")
# 
# vars <- c("total_floors_in_building", "square_meters", "condominium_fees", 
#           "floor", "heating_centralized", "lift")
# 
# test$total_floors_in_building <- as.numeric(as.character(test$total_floors_in_building))
# test$floor <- as.numeric(as.character(test$floor))
# 
# train_subset <- train %>% select(all_of(vars))
# test_subset  <- test  %>% select(all_of(vars))
# 
# 
# 
# train_subset[["__source__"]] <- "train"
# test_subset[["__source__"]] <- "test"
# 
# combined <- bind_rows(train_subset, test_subset)
# n_train <- nrow(train_subset)
# k_val <- floor(sqrt(n_train))
# 
# imputed_combined <- kNN(combined,
#                         variable = "total_floors_in_building",
#                         k = k_val,
#                         dist_var = vars[-1],
#                         imp_var = FALSE,
#                         donorcond = list(combined$source == "train"))
# 
# 
# imputed_test <- imputed_combined %>%
#   dplyr::filter(`__source__` == "test") %>%
#   dplyr::select(-`__source__`)


# test$total_floors_in_building <- imputed_test$total_floors_in_building

test$is_na_total_floor <- rep(0, nrow(test))
test$is_na_total_floor[id_na_total_floor] <- 1
test <- test %>% group_by(floor) %>%  
  mutate(
    total_floors_in_building = if_else(
      is.na(total_floors_in_building),
      calcola_moda(total_floors_in_building),
      total_floors_in_building
    )
  ) %>%
  ungroup()


sum(is.na(test$total_floors_in_building))

summary(test$total_floors_in_building)
str(test$total_floors_in_building)
test$total_floors_in_building <- as.numeric(as.character(test$total_floors_in_building))


#lift------------------------------------------------- 


test$lift[is.na(test$lift) & test$total_floors_in_building >= 3] <- "yes"  
test$lift[is.na(test$lift) & test$total_floors_in_building < 3] <- "no" 
sum(is.na(test$lift))

#Non ho più na

#Variabile conditions 

levels(test$conditions)
table(test$conditions)


#conditions-------------------------------------------------

table(test$energy_efficiency_class, test$conditions)

test[test$available == "No", "conditions"]
levels(test$conditions) <- c(levels(test$conditions), "Information not available")
test$conditions[is.na(test$conditions)] <- "Information not available"
test$conditions[is.na(test$conditions)] <- "Information not available"
summary(test$conditions) #Non ho più NA


id_na_conditions <- which(is.na(test$conditions))

test[id_na_conditions,]

test <- test %>%
  dplyr::select(-availability)


#heating_centralized----------------------------------------------------------

#Metto riscaldamento indipendente tutte le osservazioni che hanno o il camino o un numero di piani pari a 1 o con una piscina o un attico 
test$heating_centralized[is.na(test$heating_centralized) & test$total_floors_in_building == "1"] <- "independent"
test$heating_centralized[is.na(test$heating_centralized) & test$feature_fireplace == "1"] <- "independent"
test$heating_centralized[is.na(test$heating_centralized) & test$feature_attic == "1"] <- "independent"
test$heating_centralized[is.na(test$heating_centralized) & test$feature_tennis_court == "1"] <- "independent"
test$heating_centralized[is.na(test$heating_centralized) & test$feature_pool == "1"] <- "independent"
test$heating_centralized[is.na(test$heating_centralized) & test$condominium_fees == 0] <- "independent"
test$heating_centralized[is.na(test$heating_centralized)] <- "central" #da riguardare



#efficiency class--------------------------------------

boxplot(test$condominium_fees ~ test$heating_centralized)
boxplot(test$year_of_construction ~ test$heating_centralized)
mosaicplot(table(test$heating_centralized, test$total_floors_in_building))
table(test$heating_centralized, test$energy_efficiency_class)
test[is.na(test$energy_efficiency_class), ]
table(test$energy_efficiency_class, test$year_of_construction)


levels(test$energy_efficiency_class)[levels(test$energy_efficiency_class) == ","] <- "Info not available"
levels(test$energy_efficiency_class) <- c(levels(test$energy_efficiency_class), "Info not available")
test$energy_efficiency_class[is.na(test$energy_efficiency_class)] <- "Info not available"




table(test$available)
test$available[is.na(test$available)] <- "Yes" #Imputo con la moda

table(test$zone)
sum(is.na(test$zone))
test[is.na(test$zone),]


#condominium feeessss-------------------------------


#Suppongo siano case indipendenti
test$condominium_fees[is.na(test$condominium_fees) & test$total_floors_in_building == "1"] <- 0

id_na_condominio_fees <- which(is.na(test$condominium_fees))


id_na_condominio_fees <- which(is.na(test$condominium_fees))

test$is_na_condominio_fees <- rep(0, nrow(test))
test$is_na_condominio_fees[id_na_condominio_fees] <- 1




test <- test %>%
  mutate(Condominio = ifelse(condominium_fees == 0 & total_floors_in_building <= 3, "No", "Yes")) %>%
  mutate(Condominio = as.factor(Condominio))

test$Condominio[is.na(test$Condominio) & test$total_floors_in_building == 1] <- "No"
test$Condominio[is.na(test$Condominio) & test$total_floors_in_building == 2] <- "No"
test$Condominio[is.na(test$Condominio) & test$total_floors_in_building > 2] <- "Yes"

# mod_condominium_fees <- lm(log(condominium_fees+1) ~ concierge + Condominio + feature_tennis_court + feature_pool+
#                              feature_electric_gate + feature_video_entryphone + heating_centralized + energy_efficiency_class + lift, data = test[-id_na_condominio_fees, ])



pred_condominio <- predict(mod_condominium_fees, newdata = test[-id_na_condominio_fees, ])
pred <- round(exp(pred_condominio)-1)
length(which(is.na(pred)))
test[which(is.na(pred)), ]


test$condominium_fees[id_na_condominio_fees] <- round(exp(predict(mod_condominium_fees, test[id_na_condominio_fees, ])+1))

summary(test)


out <- which(test$condominium_fees > 12000)
test[out,]
test$condominium_fees[out] <- test$condominium_fees[out]/1000


#year of construction-----------------------------
summary(test$year_of_construction)

test %>%
  filter(year_of_construction == 1111)

id_year_na <- which(is.na(test$year_of_construction))
test$is_na_year <- rep(0, nrow(test))
test$is_na_year[id_year_na] <- 1 

test$year_of_construction[id_year_na] <-  1960

# str(test$year_of_construction)
# test <- test %>% mutate(
#   year_of_construction = case_when(
#     year_of_construction < 1920 ~ "Prima del 1920",
#     between(year_of_construction, 1920, 1929) ~ "Anni 20",
#     between(year_of_construction, 1930, 1939) ~ "Anni 30",
#     between(year_of_construction, 1940, 1949) ~ "Anni 40",
#     between(year_of_construction, 1950, 1959) ~ "Anni 50",
#     between(year_of_construction, 1960, 1969) ~ "Anni 60",
#     between(year_of_construction, 1970, 1979) ~ "Anni 70",
#     between(year_of_construction, 1980, 1989) ~ "Anni 80",
#     between(year_of_construction, 1990, 1999) ~ "Anni 90",
#     between(year_of_construction, 2000, 2009) ~ "Anni 2000",
#     between(year_of_construction, 2010, 2019) ~ "Anni 2010",
#     between(year_of_construction, 2020, 2025) ~ "2020-Oggi",
#     TRUE ~ "Info not available"
#   )
# ) %>% mutate(year_of_construction = as.factor(year_of_construction))

summary(test$year_of_construction)

#floor------------------------------------------------------

test$floor <- as.numeric(as.character(test$floor))
test$total_floors_in_building <- as.numeric(as.character(test$total_floors_in_building))
test[which(as.numeric(as.character(test$floor)) > as.numeric(as.character(test$total_floors_in_building))), "total_floors_in_building"] <- test$floor[which(as.numeric(as.character(test$floor)) > as.numeric(as.character(test$total_floors_in_building)))] +1 



test <- test %>%
  mutate(floor_rate = as.numeric(floor)/(as.numeric(total_floors_in_building)-2+0.1)) 

#Condominio--------------------------------------------
test <- test %>%
  mutate(Condominio = ifelse(condominium_fees == 0 & total_floors_in_building <= 3, "No", "Yes"))


#square_meters-------------------------

#Osservo la variabile square_meters
hist(test$square_meters)
boxplot(test$square_meters~test$rooms_number) #Ovviamente all'aumentare del numero di stanze aumenta square meters
summary(test$square_meters) 
err_sqmt <- which(test$square_meters < 20)


#Modello lineare per aggiustare square meters

# pred_sqmtr <- exp(predict(mod_sqmtrs, test[err_sqmt,]))
# test$square_meters[err_sqmt] <- pred_sqmtr


hist(test$square_meters, freq = F, col = "white")
summary(test$square_meters)


levels(test$zone)


length(which(test$feature_external_exposure == 1 & test$exposure == "Info not available"))


table(test$exposure, test$feature_external_exposure)

# levels(test$exposure) <- c(levels(test$exposure), "External")
# test$exposure[which(test$feature_external_exposure == 1 & test$exposure == "Info not available")] <-  "External"




#altre variabili-------------------------------------------------------------
skim(test)

table(test$feature_optic_fiber)
table(test$feature_alarm_system)

table(test$available) #Poche 
table(test$available_2023) #Solo 32
table(test$available_2024) # 173
table(test$available_2025) #38
table(test$available_2026) # 37 

#Tengo soltanto available 

test <- test %>%
  dplyr::select(-available_2023, -available_2024, -available_2025, -available_2026)

table(test$conditions)

table(test$feature_furnished, test$conditions)
table(test$feature_partially_furnished, test$feature_furnished)
table(test$feature_only_kitchen_furnished, test$feature_furnished)


table(test$conditions)

table(test$feature_furnished, test$conditions)
table(test$feature_partially_furnished, test$feature_furnished)
table(test$feature_only_kitchen_furnished, test$feature_furnished)


test <- test %>%
  mutate(furnished = ifelse(feature_furnished  == 1, "yes", 
                            ifelse(feature_only_kitchen_furnished == 1, "only kitchen",
                                   ifelse(feature_partially_furnished == 1, "partially", "none"))))%>%
  mutate(furnished = as.factor(furnished))%>%
  dplyr::select(-feature_furnished, -feature_only_kitchen_furnished, -feature_partially_furnished)


names(test)



luxury <- test[, c("feature_pool", "feature_tennis_court", "concierge",
                   "feature_hydromassage")]

luxury <- as.data.frame(apply(luxury, 2, as.numeric))
luxury <- rowSums((luxury))
test$luxury <- as.factor(luxury) 


optional <- test[,c("feature_security_door", "feature_electric_gate", "feature_fireplace",
                    "feature_optic_fiber", "feature_cellar", "feature_video_entryphone", "feature_alarm_system",
                    "feature_centralized_tv_system", "feature_single_tv_system", "feature_tv_system_with_satellite_dish")]

optional <- as.data.frame(apply(optional, 2, as.numeric))
optional <- rowSums((optional))
test$optional <- as.factor(optional) 

#Considero le variabili numeriche



hist(test$total_floors_in_building)
hist((test$total_floors_in_building))
summary(test$floor_rate)

test %>%
  filter(floor_rate < -1 |
           floor_rate > 1)

table(test$floor)
table(test$total_floors_in_building)



test <- test %>%
  mutate(
    floor_rate = case_when(
      total_floors_in_building == 0 | floor > total_floors_in_building ~ NA_real_,
      total_floors_in_building == 1 ~ 1.0,
      TRUE ~ floor / total_floors_in_building
    )
  )


hist(test$floor_rate, freq = F, col = "white")



test <- test %>%
  mutate(
    floor_category = case_when(
      floor < 0 ~ "Basement",
      floor == 0 ~ "Ground_Floor",
      floor_rate == 1 & total_floors_in_building > 1 ~ "Penthouse",
      floor_rate > 0.66 ~ "High_Floor",
      floor_rate > 0.33 ~ "Mid_Floor",
      floor_rate > 0 ~ "Low_Floor",
      TRUE ~ "Other" 
    )
  )

#zone-----------------------------------------------------------------------

test$zone <- as.character(test$zone)

relevel_zone <- c(
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
  "via fra' cristoforo" = "famagosta", 
  "corso magenta"= "sempione",
  "largo caioroli 2"= "sempione",
  "via marignano, 3"="santa giulia"
)




test <- test %>%
  mutate(zone = ifelse(test$zone %in% names(relevel_zone), relevel_zone[test$zone], test$zone))
test <- test%>%filter(!is.na(test$zone))



summary(test)

sort(unique(test$zone))

test <- test %>%
  mutate(zone = str_to_upper(zone))

sort(unique(test$zone))

test$zone <- paste((test$zone), "Milano,Italia") 

test %>%
  filter(zone == "OTHER")

test$zone[test$zone == "MOLISE - CUOCO Milano,Italia"] <- "CUOCO Milano,Italia"
test$zone[test$zone == "BOLOGNA - SULMONA Milano,Italia"] <- "SULMONA Milano,Italia"
test$zone[test$zone == "SAN VITTORE Milano,Italia"] <- "SAN VITTORE carcere Milano, Italia"
test$zone[test$zone == "CERMENATE - ABBIATEGRASSO Milano,Italia"] <- "PIAZZA ABBIATEGRASSO Milano, Italia"
test$zone[test$zone == "PONTE NUOVO Milano,Italia"] <- "PONTE NUOVO quartiere Milano,Italia"

library(tidygeocoder)
coordinate <- geo(test$zone, method = "arcgis")
summary(coordinate)
sort(unique(coordinate$address))

coordinate_unique <- coordinate %>%
  rename(zone = address) %>%  
  group_by(zone) %>%       
  slice(1) %>%             
  ungroup()               


test <- test %>%
  left_join(coordinate_unique, by = "zone")

sort(unique(test$zone))


duomo <- geo(address = "Duomo, Milano, Italy", method = "arcgis")


library(geosphere)


test$lat[test$zone == "PONTE NUOVO quartiere Milano,Italia"] <- 45.47583
test$long[test$zone == "PONTE NUOVO quartiere Milano,Italia"] <- 9.19694                                                                    


test <- test %>%
  mutate(
    distanza_duomo_metri = distHaversine(
      p1 = cbind(long, lat),  
      p2 = cbind(duomo$long, duomo$lat)    
    )
  )


test[test$long > 10, ]

test$log_dist <- log(test$distanza_duomo_metri+1)

summary(test)


test %>%
  filter(distanza_duomo_metri > 10000)


university_zones <- c("CITTÀ STUDI Milano,Italia",
                      "BOVISA Milano,Italia", "BICOCCA Milano,Italia",
                      "NAVIGLI - DARSENA Milano,Italia",
                      "PORTA ROMANA - MEDAGLIE D'ORO Milano,Italia", 
                      "LAMBRATE Milano,Italia",
                      "SAN VITTORE carcere Milano, Italia", 
                      "SEMPIONE Milano,Italia")
test$zona_universitaria <- ifelse(test$zone %in% university_zones, 1, 0)
test$zona_universitaria <- as.factor(test$zona_universitaria)

test$Condominio <- as.factor(test$Condominio)
test$Condominio <- relevel(test$Condominio, ref = "Yes")


test %>%
  filter(feature_attic == 1) %>% select(floor, total_floors_in_building)




test <- test %>%
  mutate(
    mq_x_pool = log(square_meters) * (as.numeric(feature_pool)-1),
    mq_x_bath = log(square_meters) * as.numeric(bathrooms_number)
  )

save(test, file = "test_preprocessed.RData")


#Fine preprocessing----------------------------------------------



rm(list = ls())
setwd("G:/My Drive/Università/Magistrale/Secondo anno M/Data Mining")
load("train_preprocessed.RData")
load("test_preprocessed.RData")

source("funzioni.R")
# train
train$glass <- as.factor(train$glass)
train$floor_category <- as.factor(train$floor_category)
train$Condominio <- as.factor(train$Condominio)
train$zone <- as.factor(train$zone)
train$material <- as.factor(train$material)
train$car_parking <- as.factor(train$car_parking)
train$is_na_condominio_fees <- as.factor(train$is_na_condominio_fees)
train$is_na_total_floor <- as.factor(train$is_na_total_floor)
train$is_na_year <- as.factor(train$is_na_year)


# test
test$glass <- as.factor(test$glass)
test$floor_category <- as.factor(test$floor_category)
test$Condominio <- as.factor(test$Condominio)
test$zone <- as.factor(test$zone)
test$material <- as.factor(test$material)
test$car_parking <- as.factor(test$car_parking)
test$is_na_condominio_fees <- as.factor(test$is_na_condominio_fees)
test$is_na_total_floor <- as.factor(test$is_na_total_floor)
test$is_na_year <- as.factor(test$is_na_year)

# Metto "Yes" come baseline
train$Condominio <- relevel(train$Condominio, ref = "Yes")


#Group lasso--------------------------------------------------------------------


#Group Lasso 




# train <- train %>%
#   select(-prezzo_mq)
# 
# 
# set.seed(1)
# idx_train <- sample(1:nrow(train), 0.7*nrow(train), F)
# train_small <- train[idx_train,]
# val <- train[-idx_train, ]
# 
# 
# 
# library(grpreg)
# library(forcats)
# 
# group_times <- function(x){
#   if(is.factor(x)){
#     group_times <- length(fct_unique(x)) - 1
#   } else {
#     group_times <- 1
#   }
#   group_times
# }
# 
# groups <- NULL
# for(j in colnames(subset(train_small, select = -selling_price))){
#   groups <- c(groups, rep(j, times = group_times(train_small[, j])))
# }
# 
# 
# X_shrinkage <- model.matrix(selling_price~., data = train_small)[, -1]
# y_shrinkage <- train_small$selling_price
# 
# colnames_X <- colnames(X_shrinkage)
# groups <- character(length(colnames_X))
# 
# original_vars <- names(train_small)[-14]
# 
# for(i in seq_along(colnames_X)){
#   col_name <- colnames_X[i]
#   best_match <- col_name
#   max_match_length <- 0
# 
#   for(var in original_vars){
#     if(startsWith(col_name, var) && nchar(var) > max_match_length){
#       best_match <- var
#       max_match_length <- nchar(var)
#     }
#   }
# 
#   groups[i] <- best_match
# }
# 
# 
# # Applica la group lasso
# lambda_grp_grid <- exp(seq(-11, 0, length = 100))
# 
# m_grp_lasso <- grpreg(X = X_shrinkage,
#                       y = log(y_shrinkage),
#                       group = groups,
#                       lambda = lambda_grp_grid,
#                       family = "gaussian")
# 
# plot(m_grp_lasso, log.l = TRUE)
# set.seed(1)
# grp_lasso_cv <- cv.grpreg(X_shrinkage, log(y_shrinkage), group = groups, lambda = lambda_grp_grid)
# 
# plot(grp_lasso_cv)
# (lambda_grp_optimal <- grp_lasso_cv$lambda.min)
# 
# y_hat_grp_lasso <- exp(predict(m_grp_lasso, X = model.matrix(formula_lm_s, data = val)[, -1],
#                                lambda = lambda_grp_optimal))
# 
# MAE_group_lasso <- MAE(val$selling_price, y_hat_grp_lasso)
# predict(m_grp_lasso, type="ngroups", lambda=lambda_grp_optimal)
# predict(m_grp_lasso, type="groups", lambda=lambda_grp_optimal)
# predict(m_grp_lasso, type="nvars", lambda=lambda_grp_optimal)
# 
# 
# #Lasso 
# 
# 
# lambda_en_grid <- exp(seq(-10, 0, length = 100))
# m_en <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 1, lambda = lambda_en_grid)
# 
# plot(m_en, xvar = "lambda")
# 
# resid_en <- matrix(0, nrow(val), length(lambda_en_grid))
# resid_log_en <- matrix(0, nrow(val), length(lambda_en_grid))
# 
# y_hat_en <- exp(predict(m_en, newx = model.matrix(selling_price ~ ., data = val)[, -1], s = lambda_en_grid))
# for (j in 1:length(lambda_en_grid)) {
#   resid_en[, j] <- val$selling_price - y_hat_en[, j]
#   resid_log_en[, j] <- log(val$selling_price) - log(y_hat_en[, j])
# }
# 
# data_cv <- data.frame(
#   lambda = lambda_en_grid,
#   MAE = apply(resid_en, 2, function(x) mean(abs(x))),
#   MSLE = apply(resid_log_en^2, 2, function(x) mean(x))
# )
# 
# lambda_en_optimal <- lambda_en_grid[which.min(data_cv$MSLE)]
# lambda_en_optimal
# 
# par(mfrow = c(1, 2))
# plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
# abline(v = log(lambda_en_optimal), lty = "dashed")
# 
# plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
# abline(v = log(lambda_en_optimal), lty = "dashed")
# 
# 
# y_hat_en <- exp(predict(m_en, newx = model.matrix(selling_price ~ ., data = val)[, -1], s = lambda_en_optimal))
# 
# MAE(val$selling_price, y_hat_en)
# 
# en_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)
# 
# 
# 
# 
# #Elastic net 
# 
# 
# 
# lambda_en_grid <- exp(seq(-10, 0, length = 100))
# m_en <- glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)
# 
# plot(m_en, xvar = "lambda")
# 
# resid_en <- matrix(0, nrow(val), length(lambda_en_grid))
# resid_log_en <- matrix(0, nrow(val), length(lambda_en_grid))
# 
# y_hat_en <- exp(predict(m_en, newx = model.matrix(selling_price ~ ., data = val)[, -1], s = lambda_en_grid))
# for (j in 1:length(lambda_en_grid)) {
#   resid_en[, j] <- val$selling_price - y_hat_en[, j]
#   resid_log_en[, j] <- log(val$selling_price) - log(y_hat_en[, j])
# }
# 
# data_cv <- data.frame(
#   lambda = lambda_en_grid,
#   MAE = apply(resid_en, 2, function(x) mean(abs(x))),
#   MSLE = apply(resid_log_en^2, 2, function(x) mean(x))
# )
# 
# lambda_en_optimal <- lambda_en_grid[which.min(data_cv$MSLE)]
# lambda_en_optimal
# 
# par(mfrow = c(1, 2))
# plot(log(data_cv$lambda), data_cv$MAE, type = "b", pch = 16, cex = 0.6, ylab = "MAE (validation)", xlab = expression(log(lambda)))
# abline(v = log(lambda_en_optimal), lty = "dashed")
# 
# plot(log(data_cv$lambda), data_cv$MSLE, type = "b", pch = 16, cex = 0.6, ylab = "MSLE", xlab = "p")
# abline(v = log(lambda_en_optimal), lty = "dashed")
# 
# y_hat_en <- exp(predict(m_en, newx = model.matrix(selling_price ~ ., data = val)[, -1], s = lambda_en_optimal))
# 
# MAE(val$selling_price, y_hat_en)
# 
# en_cv <- cv.glmnet(X_shrinkage, log(y_shrinkage), alpha = 0.5, lambda = lambda_en_grid)





#Modelli---------------------------------------------------------------

boxplot(log(train$selling_price) ~ train$feature_cellar)
boxplot(log(train$selling_price) ~ train$feature_centralized_tv_system)
boxplot(log(train$selling_price) ~ train$feature_terrace)
boxplot(log(train$selling_price) ~ train$feature_tavern)
boxplot(log(train$selling_price) ~ train$feature_balcony)
boxplot(log(train$selling_price) ~ train$feature_external_exposure)
boxplot(log(train$selling_price) ~ train$feature_internal_exposure)
boxplot(log(train$selling_price) ~ train$bathrooms_number)
boxplot(log(train$selling_price) ~ train$energy_efficiency_class)
boxplot(log(train$selling_price) ~ train$furnished)
boxplot(log(train$selling_price) ~ train$optional)
boxplot(log(train$selling_price) ~ train$feature_optic_fiber)
boxplot(log(train$selling_price) ~ train$feature_tv_system_with_satellite_dish)
boxplot(log(train$selling_price) ~ train$feature_tennis_court)
boxplot(log(train$selling_price) ~ train$feature_balcony)
boxplot(log(train$selling_price) ~ train$material)
boxplot(log(train$selling_price) ~ train$glass)
boxplot(log(train$selling_price) ~ train$furnished)
boxplot(log(train$selling_price) ~ train$feature_single_tv_system)
boxplot(log(train$selling_price) ~ train$luxury)
boxplot(log(train$selling_price) ~ train$feature_closet)
boxplot(log(train$selling_price) ~ train$feature_electric_gate)
boxplot(log(train$selling_price) ~ train$feature_fireplace)


ggplot(train, aes(x = log(selling_price), fill = as.factor(luxury))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable luxury",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()



ggplot(train, aes(x = log(selling_price), fill = as.factor(optional))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable optional",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(feature_hydromassage))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable hydromassage",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(feature_pool))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable pool",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(feature_optic_fiber))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable optic fiber",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(feature_video_entryphone))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable video entryphone",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

ggplot(train, aes(x = log(selling_price), fill = as.factor(floor_category))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable floor category",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(car_parking))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable floor category",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(feature_private_garden))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable private garden",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

ggplot(train, aes(x = log(selling_price), fill = as.factor(material))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable material",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(glass))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable glass",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(is_na_condominio_fees))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable is na condominio fees",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

ggplot(train, aes(x = log(selling_price), fill = as.factor(Condominio))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable Condominio",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()

ggplot(train, aes(x = log(selling_price), fill = as.factor(is_na_year))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable is na year",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


ggplot(train, aes(x = log(selling_price), fill = as.factor(is_na_total_floor))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density distribution for variable is na total floor",
    x = "Log of selling price",
    y = "Density",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()


cor(train$selling_price, train$square_meters)
cor(train$selling_price, train$mq_x_bath)
cor(train$selling_price, train$mq_x_pool) #Pochissimo 

cor(train$selling_price, train$distanza_duomo_metri) #negativa ha senso 

cor(train$selling_price, train$total_floors_in_building) #Bassissima 





#Modello lineare base---------------------------------------------


formula_lm <- as.formula(
  log(selling_price) ~ 
    log(square_meters) + 
    log(condominium_fees / square_meters + 1) +
    floor * luxury +                   
    feature_alarm_system + 
    optional + 
    feature_pool + 
    feature_cellar +
    conditions + 
    lift + 
    rooms_number + 
    feature_terrace + 
    energy_efficiency_class + 
    Condominio +                   
    year_of_construction + 
    is_na_year + 
    is_na_condominio_fees + 
    zone +                           
    distanza_duomo_metri * luxury +  
    mq_x_bath + 
    furnished + 
    floor_category + 
    feature_private_garden + 
    bathrooms_number
)


mod_lm <- lm(formula_lm, train_small)
summary(mod_lm)

pred <- exp(predict(mod_lm, val))
(MAE_lm <- MAE(pred, val$selling_price))



#Modello lineare con spline -----------------------------------------------------


formula_lm_s <- as.formula(
  log(selling_price) ~ 
    bs(log(square_meters), degree = 3) + 
    bs(log(condominium_fees / square_meters + 1), degree = 3) +
    bs(floor, degree = 3) * luxury +          
    feature_alarm_system + 
    feature_pool + 
    optional +
    feature_cellar +
    conditions + 
    lift + 
    rooms_number + 
    feature_terrace + 
    energy_efficiency_class + 
    Condominio +                            
    bs(year_of_construction, degree = 3) + 
    is_na_year + 
    is_na_condominio_fees + 
    zone +                              
    bs(distanza_duomo_metri, degree = 3) * luxury +  
    mq_x_bath + 
    furnished + 
    floor_category + 
    feature_private_garden + 
    bathrooms_number 
)


mod_lm_s <- lm(formula_lm_s, train_small)
summary(mod_lm_s)

pred_s <- exp(predict(mod_lm_s, val))
(MAE_lm_s <- MAE(pred_s, val$selling_price))



#Modello lineare con effetti misti-------------------------------------------

library(lme4)

formula_mx <- as.formula( log(selling_price) ~ 
                            log(square_meters) + 
                            log(condominium_fees / square_meters + 1) +
                            floor * luxury +                   
                            feature_alarm_system + 
                            optional + 
                            feature_pool + 
                            feature_cellar +
                            conditions + 
                            lift + 
                            rooms_number + 
                            feature_terrace + 
                            energy_efficiency_class + 
                            Condominio +                   
                            year_of_construction + 
                            is_na_year + 
                            is_na_condominio_fees + 
                            (1|zone) +                           
                            distanza_duomo_metri * luxury +  
                            mq_x_bath + 
                            furnished + 
                            floor_category + 
                            feature_private_garden + 
                            bathrooms_number)

train_small$zone <- as.factor(train_small$zone)
contrasts(train_small$zone) <- contr.sum(nlevels(train_small$zone))

mod_lmer <- lmer(formula_mx, train_small)
pred_mx <- exp(predict(mod_lmer, val,re.form = NULL))
(MAE_MX <- MAE(pred_mx, val$selling_price))
VarCorr(mod_lmer)
ranef(mod_lmer)$zone




#Modello a effetti misti con spline-----------------------------------------------------------------




formula_mx_s <- as.formula(  log(selling_price) ~ 
                               bs(log(square_meters), degree = 3) + 
                               bs(log(condominium_fees / square_meters + 1), degree = 3) +
                               bs(floor, degree = 3) * luxury +          
                               feature_alarm_system + 
                               optional + 
                               feature_pool + 
                               conditions + 
                               lift + 
                               feature_cellar +
                               rooms_number + 
                               feature_terrace + 
                               energy_efficiency_class + 
                               Condominio +                            
                               bs(year_of_construction, degree = 3) + 
                               is_na_year + 
                               is_na_condominio_fees + 
                               (1|zone) +                              
                               bs(distanza_duomo_metri, degree = 3) * luxury +  
                               mq_x_bath + 
                               furnished + 
                               floor_category + 
                               feature_private_garden + 
                               bathrooms_number )

train_small$zone <- as.factor(train_small$zone)
contrasts(train_small$zone) <- contr.sum(nlevels(train_small$zone))

mod_lmer_s <- lmer(formula_mx_s, train_small)
pred_mx_s <- exp(predict(mod_lmer_s, val,re.form = NULL))
(MAE_MX_s <- MAE(pred_mx_s, val$selling_price))
VarCorr(mod_lmer_s)
ranef(mod_lmer_s)$zone



#Gam model------------------------------------------------------------------------------

formula_gamma <- as.formula(
  log(selling_price) ~ 
    s(log(square_meters)) + 
    s(log(condominium_fees/square_meters + 1)) +
    s(floor, by = luxury) + 
    feature_alarm_system  + optional +
    feature_pool + conditions + lift + rooms_number + feature_terrace +
    energy_efficiency_class+ 
    feature_cellar +
    Condominio +
    s(year_of_construction)+
    is_na_year  +
    is_na_condominio_fees + 
    zone+
    s(distanza_duomo_metri, by = luxury)+
    mq_x_bath + furnished + floor_category + feature_private_garden + bathrooms_number
)

contrasts(train$zone) <- contr.sum(nlevels(train$zone))
contrasts(test$zone) <- contr.sum(nlevels(test$zone))

mod_gamma <- gam(formula_gamma, 
                 data = train_small,
                 method = "REML")

pred_gamma <- exp(predict(mod_gamma, val))
(MAE_gamma <- MAE(pred_gamma, val$selling_price))





#Modello migliore----------------------------------------------------------------

formula_gam <- as.formula(
  log(selling_price) ~ 
    s(log(square_meters)) + 
    s(log(condominium_fees/square_meters + 1)) +
    s(floor, by = luxury) + 
    feature_alarm_system   + optional +
    feature_pool + conditions + lift + rooms_number + feature_terrace +
    energy_efficiency_class + 
    feature_cellar +
    Condominio +
    s(year_of_construction)+
    is_na_year  +
    is_na_condominio_fees + 
    s(zone, bs = "re")+
    s((distanza_duomo_metri), by = luxury)+
    mq_x_bath + furnished + floor_category + feature_private_garden + bathrooms_number
)

contrasts(train$zone) <- contr.sum(nlevels(train$zone))
contrasts(test$zone) <- contr.sum(nlevels(test$zone))

mod_gam <- gam(formula_gam, 
               data = train_small,,
               method = "REML")


pred_gam <- exp(predict(mod_gam, val))
(MAE_gam <- MAE(pred_gam, val$selling_price))


summary(mod_gam)

ranef_vals <- predict(mod_gam, type = "terms")



coefs <- coef(mod_gam)
zone_effects <- coefs[grep("^s\\(zone\\)\\.", names(coefs))]
zone_names <- gsub("^s\\(zone\\)\\.", "", names(zone_effects))
names(zone_effects) <- gsub("^s\\(zone\\)\\.", "", names(zone_effects))
print(zone_effects)

livelli <- levels(zone)
names(zone_effects) <- livelli
exp(sort(zone_effects))-1



gam.check(mod_gam)


################################PREVISIONI########################################

mod_gam <- gam(formula_gam, 
               data = train, 
               method = "REML")



summary(mod_gam)


coefs <- coef(mod_gam)
zone_effects <- coefs[grep("^s\\(zone\\)\\.", names(coefs))]
zone_names <- gsub("^s\\(zone\\)\\.", "", names(zone_effects))
names(zone_effects) <- gsub("^s\\(zone\\)\\.", "", names(zone_effects))
print(zone_effects)

livelli <- levels(zone)
names(zone_effects) <- livelli
(exp(sort(zone_effects))-1)*100  #Per l'interpretazione dei coef di zone

table(train$zone)


pred_res <- exp(predict(mod_gam))
mae_res <- (pred_res-train$selling_price)
sort(mae_res, decreasing = T)
err <- train[ c(4787, 3079, 7164, 6122, 5934 ),]

View(err)





predizioni_gam <- exp(predict(mod_gam, newdata = test))
# plot(mod_gam)

previsione<-data.frame(
  ID=1:4800,
  prediction=predizioni_gam
)
# write.csv(previsione, "submission_Pozzi_Tommaso.csv", row.names=F)

#Tabella risultati


ris <- data.frame(
  Modello = c("Lineare", "Lineare con B-Spline (degree = 3)", "Lineare con zone random", "Lineare con zone random e B-spline(degree = 3)", "Gam con zone fisso", "Gam con zone random"),
  MAE = c(MAE_lm, MAE_lm_s, MAE_MX, MAE_MX_s, MAE_gamma, MAE_gam)
)


library(xtable)

xtable <- xtable(ris, caption = "Risultati in termini di MAE dei modelli stimati sul validation set")
print(xtable, rownames  = F)




#Alcuni grafici 


anova(mod_gam)
gam.check(mod_gam)

# plot(mod_gam, pages = 1, scheme = 1, shade = TRUE)


library(gratia)


appraise(mod_gam)         
qq_plot(mod_gam)       

train %>%
  group_by(zone) %>%
  summarise(med_price = median(selling_price, na.rm = TRUE)) %>%
  arrange(med_price) %>%
  mutate(zone = factor(zone, levels = zone)) -> zone_order


train$zone <- factor(train$zone, levels = levels(zone_order$zone))

ggplot(train, aes(x = zone, y = log(selling_price))) +
  geom_boxplot(outlier.size = 0.5, fill = "steelblue", alpha = 0.7) +
  labs(
    x = "Zone",  
    y = "Prezzo di vendita (log)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Nessuna etichetta zona
    axis.ticks.x = element_blank(), # Nessuna tacca asse X
    plot.title = element_text(size = 14, face = "bold")
  )



floor_plots <- draw(mod_gam, select = c("s(floor):luxury0", "s(floor):luxury1", 
                                        "s(floor):luxury2", "s(floor):luxury3", 
                                        "s(floor):luxury4"))

distanza_plot <- draw(mod_gam, select = c("s(distanza_duomo_metri):luxury0", "s(distanza_duomo_metri):luxury1",
                                          "s(distanza_duomo_metri):luxury2", "s(distanza_duomo_metri):luxury3",
                                          "s(distanza_duomo_metri):luxury4"))

sqmrt_plot <- draw(mod_gam, select = c("s(log(square_meters))"))
year <- draw(mod_gam, select = c("s(year_of_construction)"), smooth_col = "blue")





###################FUNZIONI#####################################################

MAE <- function(pred, true){
  mean(abs(pred-true))
}



boxcox_inverse <- function(y_bc, lambda) {
  if (lambda == 0) {
    return(exp(y_bc))
  } else {
    return((lambda * y_bc + 1)^(1 / lambda))
  }
}


K_fold_CV <- function(k, data, formula){
  set.seed(1)
  folds <- sample(1:k, size = nrow(data), replace = TRUE)
  mae_results <- numeric(k)
  for (i in 1:k) {
    validation_indices <- which(folds == i)
    validation_set <- data[validation_indices, ]
    training_set <- data[-validation_indices, ]
    
    mod <- lm(formula, data = training_set)
    
    pred <- exp(predict(mod, newdata = validation_set))
    
    actuals <- (validation_set$selling_price)
    mae <- MAE(pred, validation_set$selling_price)
    mae_results[i] <- mae
  }
  cat("MAE per ogni fold:\n")
  print(mae_results)
  cat("\nMAE medio (stima finale):\n")
  print(mean(mae_results))
}

K_fold_CV_mx <- function(k, data, formula){
  set.seed(1)
  folds <- sample(1:k, size = nrow(data), replace = TRUE)
  mae_results <- numeric(k)
  for (i in 1:k) {
    validation_indices <- which(folds == i)
    validation_set <- data[validation_indices, ]
    training_set <- data[-validation_indices, ]
    
    mod <- lmer(formula, data = training_set)
    
    pred <- exp(predict(mod, newdata = validation_set, re.form = NULL))
    
    actuals <- (validation_set$selling_price)
    mae <- MAE(pred, validation_set$selling_price)
    mae_results[i] <- mae
  }
  cat("MAE per ogni fold:\n")
  print(mae_results)
  cat("\nMAE medio (stima finale):\n")
  print(mean(mae_results))
}


K_fold_CV_gam <- function(k, data, formula){
  set.seed(1)
  folds <- sample(1:k, size = nrow(data), replace = TRUE)
  mae_results <- numeric(k)
  for (i in 1:k) {
    validation_indices <- which(folds == i)
    validation_set <- data[validation_indices, ]
    training_set <- data[-validation_indices, ]
    
    mod <- gam(formula, 
               data = training_set, 
               method = "REML")
    
    pred <- exp(predict(mod, newdata = validation_set))
    
    actuals <- (validation_set$selling_price)
    mae <- MAE(pred, validation_set$selling_price)
    mae_results[i] <- mae
  }
  cat("MAE per ogni fold:\n")
  print(mae_results)
  cat("\nMAE medio (stima finale):\n")
  print(mean(mae_results))
}



calcola_moda <- function(x) {
  
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  
  valori_unici <- unique(x)
  valori_unici[which.max(tabulate(match(x, valori_unici)))]
}































