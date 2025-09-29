## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Worker for data visualization routines
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 23, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Performing estimations for data points")
}
source("src/analysis/worker-analysis.R")

source("src/viz/grouped_bars.R")
source("src/viz/horizontal_bars.R")
source("src/viz/dumbbells.R")
source("src/viz/sankey_advice&rep.R")
source("src/viz/sankey_drm.R")
source("src/viz/jg_upset.R")

path2fonts <- glue::glue(
  "{path2DA}/6. Country Reports/0. Fonts"
)
sysfonts::font_add(
  "inter",
  regular    = glue::glue("{path2fonts}/InterTight-Regular.ttf"),
  bold       = glue::glue("{path2fonts}/InterTight-Bold.ttf"),
  italic     = glue::glue("{path2fonts}/InterTight-Italic.ttf"),
  bolditalic = glue::glue("{path2fonts}/InterTight-SemiBoldItalic.ttf")
)
showtext::showtext_auto()

path2data4web <- glue::glue(
  "{path2EU}/reports/eu-thematic-reports/data-viz/output/data4web_gpp.csv"
)
pvars <- c(
  "prevalence2", "access2info", "access2rep", "access2drm",
  "rp_time", "rp_fair", "rp_cost", "rp_outcome",
  "TRT_judges", "TRT_prosecutors", "TRT_pda"
)
data4web <- read_csv(
  path2data4web,
  show_col_types = FALSE
) %>% 
  filter(
    (id %in% pvars) & 
      (demographic == "Total Sample") & 
      (level == "eu")
  ) %>% 
  select(
    country_name = country, id, value
  )

eu_rankings_data <- read_csv(
  path2data4web,
  show_col_types = FALSE
) %>% 
  filter(
    (id %in% pvars) & 
      (demographic == "Total Sample") & 
      (level == "national")
  ) %>% 
  select(
    country_name = country, id, value
  ) %>% 
  distinct(country_name, id, value, .keep_all = TRUE) %>% 
  group_by(id) %>% 
  arrange(desc(value)) %>% 
  mutate(
    rank=row_number()
  )

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 1 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 1")
}

#### Prevalence of Non-Trivial Legal Problems ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "prevalence_nontrivial_problems",
        country = country,
        w = 280,
        h = 265,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Prevalence of Non-Trivial Legal Problems by Category ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "prevalence_nontrivial_problems_by_category",
        country = country,
        w = 280,
        h = 180
      )
      
    }
  )
)

#### Co-occurrence of Non-Trivial Legal Problems ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "cooccurence_nproblems",
        country = country,
        w = 280,
        h = 225,
        perc = FALSE
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 2.1 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 2.1")
}

#### Access to Adequate Information and Advice ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category")
        w <- 280
        h <- 300
        
      } else {
        omit.cats = NULL
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "access2information",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Access to Adequate Information and Advice: Experience vs Perception ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_dumbbells(
        name = "access2information",
        country = country,
        w = 280,
        h = 265
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 2.2 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 2.2")
}

#### Access to Adequate Assistance and Representation (Sankey) ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      gen_sankey_advice(master, country)
    }
  )
)

#### Access to Adequate Assistance and Representation ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category")
        w <- 280
        h <- 300
        
      } else {
        omit.cats = NULL
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "access2representation",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Contacted Advisers ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "contacted_advisers",
        country = country,
        w = 280,
        h = 120
      )
      
    }
  )
)

#### Mechanisms Preventing Access to Assistance and Representation ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "access2representation_barriers",
        country = country,
        w = 280,
        h = 120
      )
      
    }
  )
)

#### Access to Adequate Assistance and Representation: Experience vs Perception ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_dumbbells(
        name = "access2representation",
        country = country,
        w = 280,
        h = 265
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 2.3 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 2.3")
}

#### Access to Dispute Resolution Mechanisms (Sankey) ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      gen_sankey_rdm(master, country)
    }
  )
)

#### Access to Dispute Resolution Mechanisms ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 300
        
      }
      
      gen_grouped_bars(
        name = "access2DRM",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Contacted Mechanisms ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "contacted_mechanisms",
        country = country,
        w = 280,
        h = 120
      )
      
    }
  )
)

#### Mechanisms Preventing Access to Assistance and Representation ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "access2DRM_barriers",
        country = country,
        w = 280,
        h = 100
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 2.4 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 2.4")
}

#### Dispute Resolution Processes that Finalized in Less than One Year ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "timeliness_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Perceived Quickness of the Dispute Resolution Process ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "quickness_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats
      )
      
    }
  )
)

#### Perceived Fairness of the Dispute Resolution Process ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "fair_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Costliness of Dispute Resolution Processes ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "cost_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Perceived Costliness of the Dispute Resolution Process ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "expensive_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 2.5 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 2.5")
}

#### Outcome of People’s Legal Problems ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "outcome_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Satisfaction with the Resolution Outcome  ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      if (country == "Malta") {
        omit.cats <- c("Category", "Age Group", "Problem Status")
        w <- 280
        h <- 220
        
      } else {
        omit.cats = c("Category")
        w <- 280
        h <- 375
        
      }
      
      gen_grouped_bars(
        name = "satisfaction_rp",
        country = country,
        w = w,
        h = h,
        omit.cats = omit.cats,
        margin_set = margin(-20,-60,0,35)
      )
      
    }
  )
)

#### Status of the Resolution Problem ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "status_rp",
        country = country,
        w = 280,
        h = 100
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 3 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 3")
}

#### Justice Gap  ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "justice_gap_keepdk",
        country = country,
        w = 280,
        h = 265,
        omit.cats = NULL,
        margin_set = margin(-20,-25,0,0)
      )
      
    }
  )
)

#### Justice Gap (Co-Occurrence)  ----
invisible(
  gen_jg_upset(
    master, w = 280, h = 180,
    ylim = c(
      "Italy" = 20.5,
      "Malta" = 18.5
    )
  ) 
)

#### Hardships ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "hardships",
        country = country,
        w = 280,
        h = 75
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 4 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 4")
}

#### Knowledge of Legal Rights and Responsibilities ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "knowledge_rights",
        country = country,
        w = 280,
        h = 265,
        omit.cats = NULL
      )
      
    }
  )
)

#### Fully Satisfied Expert Needs ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "expert_support",
        country = country,
        w = 280,
        h = 265,
        omit.cats = NULL
      )
      
    }
  )
)

#### Perceived Causes of the Problem ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "perceived_causes",
        country = country,
        w = 280,
        h = 145
      )
      
    }
  )
)

#### Legal Empowerment ----
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_horizontal_bars(
        name = "legal_empowerment",
        country = country,
        w = 280,
        h = 75
      )
      
    }
  )
)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Module 5 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(!interactive()){
  verbose_message("--- Drawing Figures for Module 5")
}

#### Trust in Judges ----
trust_judges <- trust[["judges"]]
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "trust_judges",
        country = country,
        w = 280,
        h = 300,
        omit.cats = NULL,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Trust in Prosecutors ----
trust_prosecutors <- trust[["prosecutors"]]
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "trust_prosecutors",
        country = country,
        w = 280,
        h = 300,
        omit.cats = NULL,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)

#### Trust in Prosecutors ----
trust_pda <- trust[["pda"]]
invisible(
  lapply(
    c("Malta", "Italy"),
    function(country){
      
      gen_grouped_bars(
        name = "trust_pda",
        country = country,
        w = 280,
        h = 300,
        omit.cats = NULL,
        eu.avg = TRUE,
        eu.avg.data = data4web
      )
      
    }
  )
)
