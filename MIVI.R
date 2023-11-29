# set lwd
setwd("~/Documents/MIVI/")

library(ggplot2)
library(dplyr)

# 

# download all Mv: id, observed_on, lat, lon, state, country
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true
mivi_all <- read.csv("./MIVI-ALL.csv") %>%
    mutate(date=as.Date(observed_on, format="%Y-%m-%d")) %>% select(-observed_on) %>%
    select(-place_country_name)  # not really useful

# download no sign 21: id; add stage
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=21
mivi_young <- read.csv("MIVI-YOUNG.csv") %>%
    mutate(stage="Vegetation")

# download flowering 15: id; add stage
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=13
mivi_flowering <- read.csv("./MIVI-FLOWERING.csv") %>%
  mutate(stage="Flowering")

# download fruiting 14: id; add stage
# quality_grade=research&identifications=any&place_id=97394&taxon_id=116710&verifiable=true&term_id=12&term_value_id=14
mivi_fruiting <- read.csv("./MIVI-FRUITING.csv") %>%
  mutate(stage="Seeding")

# join each based on id
mivi_all <- mivi_all %>% left_join(mivi_young, by="id")
mivi_all <- mivi_all %>% left_join(mivi_flowering, by="id") %>% mutate(stage = coalesce(stage.x, stage.y)) %>% select(-stage.x, -stage.y)
mivi_all <- mivi_all %>% left_join(mivi_fruiting, by="id") %>% mutate(stage = coalesce(stage.x, stage.y)) %>% select(-stage.x, -stage.y)

# memory cleanup
rm(mivi_young, mivi_flowering, mivi_fruiting)



# ggplot(mivi_young, aes(x=longitude,y=latitude)) + geom_point()
# ggplot(mivi_young, aes(x=longitude,y=latitude,color=place_state_name)) + geom_point()

# ggplot(mivi_young, aes(x=latitude)) + geom_histogram(binwidth=0.2)



timeclip <- c(as.Date("2019-01-01"), as.Date("2023-12-30"))
timeclip <- c(min(mivi_all$date), max(mivi_all$date))

# pdf(file="output1.pdf")

# Time series by latitude, color by stage
ggplot(mivi_all, aes(date, latitude)) + geom_point(aes(color=stage), alpha=0.75) +
  scale_x_date(limits=timeclip, date_breaks = "1 year", date_labels = "%Y")  + # scale_color_hue() # +
  labs(title="Observations by latitude over time")


# dev.off()


mivi_all$stage <- factor(mivi_all$stage, ordered=TRUE, levels=c("Vegetation", "Flowering", "Seeding"))

mivi_all <- mivi_all %>% mutate(yearweek = as.integer(strftime(date, format="%V")))
mivi_all <- mivi_all %>% mutate(julian = as.integer(strftime(date, format="%j")))

# make group by quartiles
mivi_all$group <- ntile(mivi_all$latitude, 50)
mivi_annotate$group <- ntile(mivi_annotate$elevation, 50)

mivi_annotate <- mivi_all %>% filter(!is.na(stage))

# ggplot(mivi_annotate, aes(yearweek, stage)) + geom_boxplot() + coord_flip() +
#   facet_grid(~group) # +
#   # scale_x_date(limits=timeclip, date_breaks = "1 year", date_labels = "%Y")

# grouped plots by julian date
# TODO: relabel x axis with dates
ggplot(mivi_annotate, aes(julian, latitude)) + geom_point(aes(color=stage), alpha=0.7) + facet_grid(rows=vars(group)) + scale_color_brewer(palette="Set2") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), panel.spacing.y=unit(1, "mm"))

# julian date ~ latitude
ggplot(mivi_all, aes(julian, latitude)) + geom_point(aes(color=stage), alpha=0.5)

# lat,lon plot colored by phenology
# ggplot(mivi_all, aes(longitude, latitude)) + geom_point(aes(color=stage), alpha=0.5) + scale_color_brewer(palette="Set2")


#####
# ELEVATION MEMES

library(elevatr)

coords <- data.frame(x=mivi_annotate$longitude, y=mivi_annotate$latitude, ele_id=mivi_annotate$id) # %>% slice(1:100)

elevations <- get_elev_point(coords, prj=4326, src="epqs")

# elevations <- elevations %>% left_join(coords, by="ele_id")
mivi_all <- mivi_all %>% left_join(elevations, by=join_by("id" == "ele_id")) %>% select(-elev_units, -geometry)

ggplot(mivi_all, aes(julian, elevation)) + geom_point(aes(color=stage))

# Save file
write.csv(mivi_all, file="./MIVI-PROCESSED.csv", na='')



P40 <- createPalette(52, c("#FF0000", "#00FF00", "#0000FF"), range = c(30, 80))
swatch(P40)
P40 <- sortByHue(P40)
P40 <- as.vector(t(matrix(P40, ncol=4)))
names(P40) <- NULL

ggplot(mivi_e, aes(longitude, latitude)) + geom_point(aes(color=factor(group_e)), alpha = 1, show.legend=FALSE) + scale_color_manual(values=P40)

mivi_e <- mivi_all %>% filter(!is.na(elevation))

mivi_all$group_e <- ntile(mivi_all$elevation, 50)

ggplot(mivi_e, aes(julian, elevation)) + geom_point(aes(color=stage), alpha=0.7) + facet_grid(rows=vars(group_e)) + scale_color_brewer(palette="Set2") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(), panel.spacing.y=unit(1, "mm"))


######


mivi_prc <- read.csv("./MIVI-PROCESSED.csv") %>% select(-X)

mivi_all <- mivi_all %>% left_join(select(mivi_prc, elevation, id), by=c("id"="id"))
rm(mivi_prc)


mivi_annotate <- mivi_all %>% filter(!is.na(stage))

first_seed <- function(df) {
  df <- df %>% filter(stage == "Seeding")
  return (min(df$julian, na.rm=TRUE))  # Note: returns Inf if there are none in the selection
}


## LAT
mivi_annotate$group <- ntile(mivi_annotate$latitude, 60)


a <- mivi_annotate %>% group_by(group) %>% summarise(mlat=mean(latitude, na.rm=TRUE))
b <- mivi_annotate %>% group_by(group) %>% group_map(~ first_seed(.x))

a <- bind_cols(a, do.call(rbind.data.frame, b)[,1]) %>% mutate(fseed = ...3) %>% select(-...3)

a <- remove_missing(a, finite=TRUE)

ggplot(a, aes(mlat, fseed)) + geom_point() + geom_smooth(method='lm')


## ELE

mivi_annotate$group <- ntile(mivi_annotate$elevation, 30)

a <- mivi_annotate %>% group_by(group) %>% summarise(mele=mean(elevation, na.rm=TRUE))
b <- mivi_annotate %>% group_by(group) %>% group_map(~ first_seed(.x))

a <- bind_cols(a, do.call(rbind.data.frame, b)[,1]) %>% mutate(fseed = ...3) %>% select(-...3)

a <- remove_missing(a, finite=TRUE)

ggplot(a, aes(mele, fseed)) + geom_point() + geom_smooth(method='lm')


####



