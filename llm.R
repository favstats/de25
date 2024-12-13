
library(tidyverse)
library(rollama)

# rollama::ping_ollama()

# rollama::check_model_installed()
# rollama::list_models() %>% View()
res <- query(q = "Tell me about Canada.", format = jsonlite::fromJSON('{
    "type": "object",
    "properties": {
      "name": {
        "type": "string"
      },
      "capital": {
        "type": "string"
      },
      "languages": {
        "type": "array",
        "items": {
          "type": "string"
        }
      }
    },
    "required": [
      "name",
      "capital", 
      "languages"
    ]
  }'))
