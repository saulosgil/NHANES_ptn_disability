#' Example R code to replicate NCHS Data Brief No.303, Figures 1
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016

#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.

#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm

#' ------------------------------------------------------------------------------------------------------------

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(readr)
library(dplyr)
library(survey)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

#' # Data preparation
# Download & Read SAS Transport Files
# Demographic (DEMO)

# Demographic ---------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT", tf <- tempfile(), mode="wb")
DEMO_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT", tf <- tempfile(), mode="wb")
DEMO_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT", tf <- tempfile(), mode="wb")
DEMO_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT", tf <- tempfile(), mode="wb")
DEMO_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# Dietary interview ---------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DRXTOT.XPT", tf <- tempfile(), mode="wb")
DIET_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DRXTKCAL",
                                       "DRXTPROT",
                                       "DRXTCARB",
                                       "DRXTTFAT",
                                       "DRXTCALC")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DRXTOT_B.XPT", tf <- tempfile(), mode="wb")
DIET_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DRXTKCAL",
                                       "DRXTPROT",
                                       "DRXTCARB",
                                       "DRXTTFAT",
                                       "DRXTCALC")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DR1TOT_C.XPT", tf <- tempfile(), mode="wb")
DIET_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT",
                                       "DR1TCALC")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1TOT_D.XPT", tf <- tempfile(), mode="wb")
DIET_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT",
                                       "DR1TCALC")]

# Body measures -------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT", tf <- tempfile(), mode="wb")
BODY_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT", tf <- tempfile(), mode="wb")
BODY_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT", tf <- tempfile(), mode="wb")
BODY_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT", tf <- tempfile(), mode="wb")
BODY_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# DEXA ----------------------------------------------------------------------------------------

# Body measures -------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx.xpt", tf <- tempfile(), mode="wb")
DEXA_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]

# 01-02
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_b.xpt", tf <- tempfile(), mode="wb")
DEXA_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]

# 03-04
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_c.xpt", tf <- tempfile(), mode="wb")
DEXA_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]

# 05-06
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.xpt", tf <- tempfile(), mode="wb")
DEXA_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]


# Append Files
DEMO <- dplyr::bind_rows(DEMO_99,
                         DEMO_01,
                         DEMO_03,
                         DEMO_05)

DIET <-  dplyr::bind_rows(DIET_99,
                          DIET_01,
                          DIET_03,
                          DIET_05)

BODY <-  dplyr::bind_rows(BODY_99,
                          BODY_01,
                          BODY_03,
                          BODY_05)

DEXA <-  dplyr::bind_rows(DEXA_99,
                          DEXA_01,
                          DEXA_03,
                          DEXA_05)

# Merge DEMO and DIET files

DEMO_DIET <- dplyr::left_join(DEMO, DIET, by="SEQN")

# Merge DEMO_DIET and BODY

DEMO_DIET_BODY <- dplyr::left_join(DEMO_DIET, BODY, by="SEQN")

# Merge DEMO_DIET_BODY and DEXA - full data frame

df <- dplyr::left_join(DEMO_DIET_BODY, DEXA, by="SEQN")

# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar
readr::write_csv2(x = df, file = "df.csv")
df <- read.csv2(file = "df.csv")


###### PRECISA COLOCAR O PESO ABAIXO DOS MUTATE ##########

One <-
  df |>
  # remove duplicates
  dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # adjusting protein, CHO, energy, fat, calcium due to distinct assessments
  dplyr::mutate(DRXTPROT = tidyr::replace_na(DRXTPROT, 0),
                DR1TPROT = tidyr::replace_na(DR1TPROT, 0),
                DR1TCARB = tidyr::replace_na(DR1TCARB, 0),
                DRXTCARB = tidyr::replace_na(DRXTCARB, 0),
                DRXTKCAL = tidyr::replace_na(DRXTKCAL, 0),
                DR1TKCAL = tidyr::replace_na(DR1TKCAL, 0),
                DRXTTFAT = tidyr::replace_na(DRXTTFAT, 0),
                DR1TTFAT = tidyr::replace_na(DR1TTFAT, 0),
                DRXTCALC = tidyr::replace_na(DRXTCALC, 0),
                DR1TCALC = tidyr::replace_na(DR1TCALC, 0)) |>
  # create apendicular lean mass
  dplyr::mutate(APLM = DXDLALE + DXDLLLE + DXDRALE + DXDRLLE,
                APLMBMI = APLM/BMXBMI,
                # create obesity class
                OBESITY = case_when(BMXBMI >= 30 ~ "OBESO",
                                    BMXBMI < 30 ~ "NORMAL"),
                # create osteoporose class
                OSSO = case_when(DXXLSBMD > 0.915  ~ "NORMAL",
                                 DXXLSBMD >= 0.762 & DXXLSBMD < 0.915 ~ "OSTEOPENIA",
                                 DXXLSBMD <= 0.761 ~ "OSTEOPOROSE"),
                # create protein consumption variable of distinct assessments
                PTN = DR1TPROT + DRXTPROT,
                # create relative protein consumption variable
                PTNKG = PTN/BMXWT,
                # create CHO consumption variable
                CHO = DR1TCARB + DRXTCARB,
                # create FAT consumption variable
                FAT = DRXTTFAT + DR1TTFAT,
                # create CALCIUM consuption variable
                CAL = DRXTCALC + DR1TCALC,
                # create ENERGY consumption variable
                ENERGY = DRXTKCAL + DR1TKCAL,
                # create protein consumption status
                PTN_STATUS = case_when(PTNKG < 0.8 ~ "BAIXO",
                                       PTNKG >= 0.8 & PTNKG < 1.2 ~ "ADEQUADO",
                                       PTNKG >= 1.2  & PTNKG < 1.6 ~ "MODERADO",
                                       PTNKG >= 1.6 ~ "ELEVADO"),
                # create low ALM
                ALM_STATUS = case_when(RIAGENDR == 2 & APLMBMI < 512 ~ "BAIXO",
                                       RIAGENDR == 2 & APLMBMI >= 521 ~ "ALTO",
                                       RIAGENDR == 1 & APLMBMI < 789 ~ "BAIXO",
                                       RIAGENDR == 1 & APLMBMI >= 789 ~ "ALTO"),
                # create low alm plus obesity
                SARC = case_when(ALM_STATUS == "BAIXO" & OBESITY == "OBESO" ~ "SARCOBESO",
                                 ALM_STATUS == "ALTO" & OBESITY == "OBESO" ~ "OBESO",
                                 ALM_STATUS == "BAIXO" & OBESITY == "NORMAL" ~ "SARC",
                                 ALM_STATUS == "ALTO" & OBESITY == "NORMAL" ~ "NORMAL"),
                # Peso de 8 anos
                MEC8YR = case_when(SDDSRVYR <= 2 ~ 2/4 * WTMEC4YR,
                                  (SDDSRVYR > 2 ~ 1/4 * WTMEC2YR)),
                inAnalysis= (RIDAGEYR >= 65 & !is.na(DXXLSBMD))
)

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# extraindo a base do subset - o autor chamou de NHANES

id <- as_tibble(NHANES$variables$SEQN)
lombar <- as_tibble(NHANES$variables$DXXLSBMD)
raca <- as_tibble(NHANES$variables$RIDRETH1)
ptn <- as_tibble(NHANES$variables$PTNKG)
ptn_status <- as_tibble(NHANES$variables$PTN_STATUS)
osteo <- as_tibble(NHANES$variables$OSSO)
ALM <- as_tibble(NHANES$variables$APLM)
ALMBMI <- as_tibble(NHANES$variables$APLMBMI)
BMI <- as_tibble(NHANES$variables$BMXBMI)
obese <- as_tibble(NHANES$variables$OBESITY)
APLMBMI <- as_tibble(NHANES$variables$ALM_STATUS)
ALM_STATUS <- as_tibble(NHANES$variables$ALM_STATUS)
SARC <- as_tibble(NHANES$variables$SARC)

teste <-
  bind_cols(id,
            lombar,
            ptn,
            ptn_status,
            osteo,
            ALM,
            ALMBMI,
            BMI,
            obese,
            ALM_STATUS,
            SARC)
head(teste)

teste <-
  teste |>
  rename(ic = value...1,
         lombar = value...2,
         ptn = value...3,
         ptn_status = value...4,
         osso = value...5,
         ALM = value...6,
         ALMBMI = value...7,
         BMI = value...8,
         obese = value...9,
         ALM_STATUS = value...10,
         sarc = value...11)
head(teste, n = 10)

teste |>
  filter(!ptn < 0.1) |>
  group_by(ptn_status) |>
  summarise(mean(lombar))


  group_by(ptn_status) |>
  summarise(mean(lombar))


teste |>
  filter(!ptn < 0.01) |>
  ggplot(aes(x = ptn_status,
             y = lombar)) +
  geom_col()

cor.test(teste$BMI, teste$lombar)

teste

hist(log(teste$ptn), breaks = 300)

# ordenando para ver os dados iguais
teste |>
  arrange(value...3)

# tamanho da amostra
count(teste)

# descritivo
summary(teste) # SEQN


glm(formula = value...1 ~ value...2,data = teste)
glm(formula = Depression.Score ~ RIAGENDR,data = One_sem_duplicados)

# Fim dos testes Saulo ------------------------------------------------------------------------

#' ## Analysis
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count )
  p <- svyby(varformula, byformula, design, svymean )
  outSum <- left_join(select(c,-se), p)
  outSum
}

#' ### Calculate prevalence of depression overall, by gender, by age group, and by age and gender
#' Adults
getSummary(~Depression, ~one, NHANES)
#' By sex
getSummary(~Depression, ~Gender, NHANES)
#' By age
getSummary(~Depression, ~Age.Group, NHANES)
#' By sex and age
getSummary(~Depression, ~Gender + Age.Group, NHANES)

#' ### Compare Prevalence Between Men And Women
svyttest(Depression~Gender, NHANES)$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="20-39"))$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="40-59"))$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="60 and over"))$p.value %>% as.numeric


#' ### Pairwise t-testing by age groups for total, men, and women
#' Differences by age group, among all adults
# Full output from svyttest command
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="40-59"))
# Displaying p-values only
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="40-59"))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="60 and over"))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="40-59" | Age.Group=="60 and over"))$p.value %>% as.numeric
#' Differences by age group, among men
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="20-39" | Age.Group=="40-59")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="20-39" | Age.Group=="60 and over")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="40-59" | Age.Group=="60 and over")))$p.value %>% as.numeric
#' Differences by age group, among women
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="20-39" | Age.Group=="40-59")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="20-39" | Age.Group=="60 and over")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="40-59" | Age.Group=="60 and over")))$p.value %>% as.numeric

