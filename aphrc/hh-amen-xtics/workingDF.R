library(haven)
library(dplyr)
library(tibble)

## Convert values to labels
working_df <- (df
          %>% as_factor()
          %>% as_tibble()
)

str(working_df)

