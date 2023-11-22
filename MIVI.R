# set lwd
setwd("~/Downloads/MIVI/")

library(ggplot2)
library(dplyr)

# term_value=15 is flower budding

# download all Mv: id, observed_on, lat, lon, state, country
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true
mivi_all <- read.csv("./MIVI-ALL.csv") %>%
    mutate(date=as.Date(observed_on, format="%Y-%m-%d"))

# download no sign 21: id; add stage
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=21
mivi_young <- read.csv("MIVI-YOUNG.csv") %>%
    mutate(state="Vegetation")

# download flowering 15: id; add stage
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=13
mivi_flowering <- read.csv("./MIVI-FLOWERING.csv") %>%
  mutate(stage="Flowering")

# download fruiting 14: id; add stage
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=14
mivi_fruiting <- read.csv("./MIVI-FRUITING.csv") %>%
  mutate(stage="Seeding")

# join each based on id
mivi_all %>% inner_join(mivi_young, c("id", "id"))
mivi_all %>% inner_join(mivi_flowering, c("id", "id"))
mivi_all %>% inner_join(mivi_fruiting, c("id", "id"))







# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=21
# mivi_young <- read.csv("./MIVI-YOUNG.csv") %>%
#     select(-place_country_name) %>%
#     mutate(date = as.Date(observed_on, format="%Y-%m-%d"), stage="Vegetation")

# mivi_flowering <- read.csv("./MIVI-FLOWERING.csv") %>%
#     mutate(date = as.Date(observed_on, format="%Y-%m-%d"), stage="Flowering")

# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=14
# mivi_fruiting <- read.csv("./MIVI-FRUITING.csv") %>%
#     mutate(date = as.Date(observed_on, format="%Y-%m-%d"), stage="Seeding")

# ggplot(mivi_young, aes(x=longitude,y=latitude)) + geom_point()
# ggplot(mivi_young, aes(x=longitude,y=latitude,color=place_state_name)) + geom_point()

# ggplot(mivi_young, aes(x=latitude)) + geom_histogram(binwidth=0.2)


# mivi_all <- rbind(mivi_young, mivi_flowering, mivi_fruiting)

timeclip <- c(as.Date("2014-01-01"), as.Date("2023-12-30"))

pdf(file="output1.pdf")

# Time series by latitude, color by stage
ggplot(mivi_all, aes(date, latitude)) + geom_point(aes(color=stage)) +
  scale_x_date(limits=timeclip, date_breaks = "1 year", date_labels = "%Y") +
  labs(title="Observations by latitude over time")

# Time series within one year by latitude

dev.off()

