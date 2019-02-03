#### ---- Project: APHRC Verbal Autopsy Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jan 29 (Tue) ----

library(dplyr)
library(scales)
library(expss)
library(DT)
library(tibble)
library(tidyr)

load("loadData.rda")
load("globalFunctions.rda")

# In this script, we check each and every variable. Then, recode, re-label.
## Create an indicator(varname_keepcase) variable for cases to drop

#### ---- 1. Verbal Autopsy performed? ----

vadone_tab <- propFunc(working_df, "vadone", coltotal = TRUE)
vadone_tab <- datatable(vadone_tab, caption = extractLabs("vadone"), rownames = FALSE)

# Cases with vadone == "no" were dropped
working_df <- (working_df
	%>% mutate(vadone_keepcase = ifelse(vadone=="yes", 1, 0))
)

#### ---- 2. Interview round -----

round_event_tab <- (working_df
	%>% propFunc("round_event", coltotal = TRUE)
	%>% datatable(caption = extractLabs("round_event"), rownames = FALSE)
)

# Drop case with missing round number
working_df <- (working_df
	%>% mutate(round_event_keepcase = ifelse(grepl("[0-9]", round_event), 1, 0))
)

#### ---- 3. Slum area -----

slumarea_tab <- (working_df
	%>% propFunc("slumarea", coltotal = TRUE)
	%>% datatable(caption = extractLabs("slumarea"), rownames = FALSE)
)

# Drop imputed cases
working_df <- (working_df
	%>% mutate(slumarea_keepcase = ifelse(grepl("korogocho|viwandani", slumarea), 1, 0))
)

#### ---- 4. Gender -----

gender_tab <- (working_df
	%>% propFunc("gender", coltotal = TRUE)
	%>% datatable(caption = extractLabs("gender"), rownames = FALSE)
)

# No cases to drop in gender

#### ---- 5. Year of death ----

yeardeath_tab <- (working_df
	%>% propFunc("yeardeath", coltotal = TRUE)
	%>% datatable(caption = extractLabs("yeardeath"), rownames = FALSE)
)

# Drop a case with imputed year of death
working_df <- (working_df
	%>% mutate(yeardeath_keepcase = ifelse(grepl("[0-9]", yeardeath), 1, 0))
)

#### ---- 6. Age at death in years ----

agedeath_years_tab <- (working_df
	%>% propFunc("agedeath_years", coltotal = TRUE)
	%>% datatable(caption = extractLabs("agedeath_years"), rownames = FALSE)
)

# Convert cases labeled less than 1 to 0 and "" to be dropped
working_df <- (working_df
	%>% mutate(agedeath_years = ifelse(grepl("less", agedeath_years)
		, 0
		, agedeath_years
		)
	)
	%>% mutate(agedeath_years_keepcase = ifelse(grepl("[0-9]", agedeath_years), 1, 0))
)

#### ---- 7. Age group at death ----

agegroupdeath_tab <- (working_df
	%>% propFunc("agegroupdeath", coltotal = TRUE)
	%>% datatable(caption = extractLabs("agegroupdeath"), rownames = FALSE)
)

# Drop a case with ""
working_df <- (working_df
	%>% mutate(agegroupdeath_keepcase = ifelse(grepl("[0-9]", agegroupdeath), 1, 0))
)

#### ---- 8. Result of verbal autopsy ----

intvwresult_tab <- (working_df
	%>% propFunc("intvwresult", coltotal = TRUE)
	%>% datatable(caption = extractLabs("intvwresult"), rownames = FALSE)
)

# Drop cases with incomplete VA result (anything other than complete/other)
working_df <- (working_df
	%>% mutate(intvwresult_keepcase = ifelse(grepl("completed|other", intvwresult), 1, 0))
)

#### ---- 9. Caretaker for the deceased ----

respwascaretaker_tab <- (working_df
	%>% propFunc("respwascaretaker", coltotal = TRUE)
	%>% datatable(caption = extractLabs("respwascaretaker"), rownames = FALSE)
)

#### ---- Section1: General symptom variables -----

## Convert yes and no to 1 and 0, and NA otherwise

sec1_var <- grep("^sx_", colnames(working_df), value = TRUE)
patterns <- c("^yes", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(sec1_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(sec1_var, as.numeric)
)

## Compute the total number of general symptoms exprienced by each respondent

working_df <- (working_df
	%>% mutate(total_symptoms = rowSums(working_df[, sec1_var], na.rm = TRUE))
)
codebook <- updateCodebook(var = "total_symptoms"
	, lab = "Total number of general symptoms"
)

total_symptoms_tab <- (working_df
	%>% propFunc("total_symptoms", coltotal = TRUE)
	%>% datatable(caption = extractLabs("total_symptoms"), rownames = FALSE)
)


#### ---- Death by injury or illness? ----

b4dth_var <- grep("^b4dth_", colnames(working_df), value = TRUE)
patterns <- c("^yes|ill|inju", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(b4dth_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(b4dth_var, as.numeric)
)


#### ---- Duration of illness/injury (days) before death ----
durofillorinjury_tab <- (working_df
	%>% propFunc("durofillorinjury", coltotal = TRUE)
	%>% datatable(caption = extractLabs("durofillorinjury"), rownames = FALSE)
)

## Recode missing and NIU to NA
patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("durofillorinjury", patterns, replacements, insert = FALSE)
	%>% mutate_at("durofillorinjury", as.numeric)
)

#### ---- Sought health care ----
soughthealthcare_tab <- (working_df
	%>% propFunc("soughthealthcare", coltotal = TRUE)
	%>% datatable(caption = extractLabs("soughthealthcare"), rownames = FALSE)
)

## Recode missing and NIU to NA
patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("soughthealthcare", patterns, replacements, insert = FALSE)
)

#### ---- Sought health care from? -----

carefrom_var <- grep("^(carefrom_)(?!.*first)", colnames(working_df), value = TRUE, perl = TRUE)
patterns <- c("^yes", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(carefrom_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(carefrom_var, as.numeric)
)

## Compute the total number of general symptoms exprienced by each respondent

working_df <- (working_df
	%>% mutate(total_carefrom = rowSums(working_df[, carefrom_var], na.rm = TRUE))
)
codebook <- updateCodebook(var = "total_carefrom"
	, lab = "Total number of facilities the respondent sought care from."
)

total_carefrom_tab <- (working_df
	%>% propFunc("total_carefrom", coltotal = TRUE)
	%>% datatable(caption = extractLabs("total_carefrom"), rownames = FALSE)
)

#### ---- First place to sought care from ----
carefrom_first_tab <- (working_df
	%>% propFunc("carefrom_first", coltotal = TRUE)
	%>% datatable(caption = extractLabs("carefrom_first"), rownames = FALSE)
)

## Recode missing and NIU to NA and combine other levels
patterns <- c("gov", "private", "pharmacy", "religious|traditional|^other", "NIU|miss|don|niu")
replacements <- c("Government", "Private", "Others (Religious/Healer)", "Pharmacy/Drug seller", NA)
working_df <- (working_df
	%>% recodeLabs("carefrom_first", patterns, replacements, insert = FALSE)
)


#### ---- Ill with? at the time of death -----

illwith_var <- grep("^illwith_", colnames(working_df), value = TRUE, perl = TRUE)
patterns <- c("^yes", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(illwith_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(illwith_var, as.numeric)
)

## Compute the total number of illness that caused the death

working_df <- (working_df
	%>% mutate(total_illwith = rowSums(working_df[, illwith_var], na.rm = TRUE))
)
codebook <- updateCodebook(var = "total_illwith"
	, lab = "Total number of illness that caused death"
)

total_illwith_tab <- (working_df
	%>% propFunc("total_illwith", coltotal = TRUE)
	%>% datatable(caption = extractLabs("total_illwith"), rownames = FALSE)
)


#### ---- Injuries that might have caused death -----

injury_var <- grep("^injury_", colnames(working_df), value = TRUE, perl = TRUE)
patterns <- c("^yes", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(injury_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(injury_var, as.numeric)
)

## Compute the total number of injuries that caused the death

working_df <- (working_df
	%>% mutate(total_injury = rowSums(working_df[, injury_var], na.rm = TRUE))
)
codebook <- updateCodebook(var = "total_injury"
	, lab = "Total number of injuries that caused death"
)

total_injury_tab <- (working_df
	%>% propFunc("total_injury", coltotal = TRUE)
	%>% datatable(caption = extractLabs("total_injury"), rownames = FALSE)
)


#### ---- Nature of injury (accidental/intentional) ----

injuryintended_tab <- (working_df
	%>% propFunc("injuryintended", coltotal = TRUE)
	%>% datatable(caption = extractLabs("injuryintended"), rownames = FALSE)
)

## Recode missing and NIU to NA
patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("injuryintended", patterns, replacements, insert = FALSE)
	%>% mutate_at("injuryintended", as.numeric)
)

## All cases missing in injuryintended. Maybe drop

#### ---- Most immediate cause of death ----

cod_immediate_tab <- (working_df
	%>% propFunc("cod_immediate", coltotal = TRUE)
	%>% datatable(caption = extractLabs("cod_immediate"), rownames = FALSE)
)

## Recode missing and NIU to NA
patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("cod_immediate", patterns, replacements, insert = FALSE)
)

#### ---- How long survived most immediate cause of death ----

surv_immediate_tab <- (working_df
	%>% propFunc("surv_immediate", coltotal = TRUE)
	%>% datatable(caption = extractLabs("surv_immediate"), rownames = FALSE)
)

## Recode missing and NIU to NA
patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("surv_immediate", patterns, replacements, insert = FALSE)
)


#### ---- SECTION 3: ALL DEATHS ABOVE AGE 5 ----

#### ---- Ill witht the following before death? ----

va5_illwith_var <- grep("^va5_illwith_", colnames(working_df), value = TRUE, perl = TRUE)
patterns <- c("^yes", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(va5_illwith_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(va5_illwith_var, as.numeric)
)

## Compute the total number of illness that caused death

working_df <- (working_df
	%>% mutate(total_va5_illwith = rowSums(working_df[, va5_illwith_var], na.rm = TRUE))
)
codebook <- updateCodebook(var = "total_va5_illwith"
	, lab = "Total number of illness which caused death (adults)"
)

total_va5_illwith_tab <- (working_df
	%>% propFunc("total_va5_illwith", coltotal = TRUE)
	%>% datatable(caption = extractLabs("total_va5_illwith"), rownames = FALSE)
)

#### ---- Adult had the following? ----

va5_had_var <- grep("^va5_had_", colnames(working_df), value = TRUE, perl = TRUE)
patterns <- c("^yes", "^no", "NIU|miss|don")
replacements <- c(1, 0, NA)
working_df <- (working_df
	%>% recodeLabs(va5_had_var, patterns, replacements, insert = FALSE)
	%>% mutate_at(va5_had_var, as.numeric)
)

## Compute the total number of illness that caused death

working_df <- (working_df
	%>% mutate(total_va5_had = rowSums(working_df[, va5_had_var], na.rm = TRUE))
)
codebook <- updateCodebook(var = "total_va5_had"
	, lab = "Total number of other desceases (adults)"
)

total_va5_had_tab <- (working_df
	%>% propFunc("total_va5_had", coltotal = TRUE)
	%>% datatable(caption = extractLabs("total_va5_had"), rownames = FALSE)
)


#### ---- Cases to completely drop ----

indicators <- grep("_keepcase", colnames(working_df), value = TRUE)
working_df <- (working_df
	%>% mutate(dropcase = apply(working_df[, indicators], 1, function(x){any(x==0)}))
	%>% select(-c(indicators))
)

vars_drop <- c("injuryintended")
