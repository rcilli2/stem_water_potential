library(readxl)
library(lubridate)
library(ranger)
library(caret)

# Dataset con Valori SWP e Riflettività PlanetScope SuperDove già estratti ed uniti
df = read_xlsx(path = 'C:/Users/senza/Downloads/Vite_PS_fild_TEBAKA.xlsx')
df$year = year(df$`DATA  RILIEVO field`) #year
df$doy = yday(df$`DATA  RILIEVO field`) #day of the year
colnames(df)

#Splitting Strategy con validazione in altra data o altra area
training_validation = list()
# training_validation[[1]] = which(df$Study_Area == "Tormaresca" & df$year == 2022 )
# training_validation[[2]] = which(df$Study_Area == "Tormaresca" & df$year == 2023 )
# training_validation[[3]] = which(df$Study_Area == "Zeuli" & df$year == 2022 )
# training_validation[[4]] = which(df$Study_Area == "Zeuli" & df$year == 2023 )

#Splitting Strategy con validazione pianta/sensore mai visti prima (leave-one-plant out)
training_validation = list()
for (i in 1:length(sort(unique(df$ID_field)))) {
  training_validation[[i]] = which(df$ID_field == sort(unique(df$ID_field))[i]  )
}

#Selezione Variabili (le cose non cambiano nemmeno aggiungendo clima)
#colnames(df)
features = c( "doy", # 
              ######
             "coastal_blue", "blue",
             "green_i", "green", "yellow",
             "red", "rededge", "nir",
             ###########################
             "NDVI", "NDWI", "OSAVI", "TCARI",
             "TCARI_OSAVI", "EVI", "MCARI", 
             "MCARI_OSAVI", "SR", "TVI", "NDRE"
             )

#Selezione Target (Stem Water Potential)
target = "SWP"

df = as.data.frame(df)

# Definisci una griglia di iperparametri per il tuning
tuneGrid <- expand.grid(
  mtry = c(1:length(features)),                # Numero di variabili da provare in ogni split
  splitrule = "extratrees",
  min.node.size = c(1,10*c(1:13))    # Dimensione minima del nodo terminale
)

# Crea una funzione di controllo per il training
trainControl <- trainControl(
  method = "cv", 
  number = 5,                    # Numero di fold della cross-validazione
  index = training_validation,
  search = "grid",
  allowParallel = T,
  savePredictions = "final" #T
)

# Addestra un modello di foresta casuale utilizzando ranger
model <- train(
  #response ~ predictor1 + predictor2, 
  #data = trainData,
  x = df[,features],
  y = df[,target],
  method = "ranger",
  num.trees = 50,
  trControl = trainControl,
  tuneGrid = tuneGrid,
  importance = 'permutation',        # Impostazione per calcolare l'importanza delle variabili
  verbose = T,
  metric = "Rsquared"
)


model$finalModel

library(hexbin)

x = model$pred$obs
y = model$pred$pred
h = hexbin(x,y,xbins = 50)
plot(h)
cor(x,y)


plot(model$results$min.node.size, model$results$Rsquared)
plot(model$results$mtry, model$results$Rsquared)


# Assuming you have a model object and it contains feature importance
importance_values <- importance(model$finalModel)

# Create a data frame with sorted importance values
importance_df <- data.frame(
  Feature = names(importance_values),
  Importance = importance_values
)

# Ensure the feature names are treated as a factor with levels in the order of importance
importance_df$Feature <- factor(importance_df$Feature,
                                levels = importance_df$Feature[order(importance_df$Importance)])

# Load necessary library
library(ggplot2)

# Create the barplot
ggplot(importance_df, 
       aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Flip the coordinates to make the plot horizontal
  labs(title = "Feature Importance (model after Space-Time CV)",
       x = "Features",
       y = "Importance") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "bold")) # Customize text size and boldness

