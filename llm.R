
library(tidyverse)
library(rollama)

rollama::ping_ollama()


the_schema <- jsonlite::toJSON(   list( politicalSymbols = list(type = "boolean"),
                     politicalText = list(type = "boolean"),
                     politicalFigures = list(type = "boolean"),
                     politicalActions = list(type = "boolean"),
                     politicalSettings = list(type = "boolean"),
                     ideologicalVisuals = list(type = "boolean"),
                     gesturesOrPoses = list(type = "boolean"),
                     politicalClothingAccessories = list(type = "boolean"),
                     mediaAndTechnology = list(type = "boolean"),
                     environmentalCues = list(type = "boolean"),
                     implicitPoliticalIndicators = list(type = "boolean"),
                     historicalOrCurrentEvents = list(type = "boolean"),
                     propagandaTechniques = list(type = "boolean"),
                     mostPoliticalFeature = list(type="string")))

# rollama::check_model_installed()
rollama::list_models()
res <- query(q = "Tell me about Canada.",format =the_schema)
