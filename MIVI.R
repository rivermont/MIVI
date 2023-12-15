# Will Bennett
# https://github.com/rivermont/MIVI
#####

# set lwd
setwd("~/Documents/MIVI/")

library(dplyr)
library(ggplot2)
library(elevatr)
library(corrtable)
library(table1)
library(knitr)

first_fruit <- function(df) {
    df <- df %>% filter(stage == "Fruiting")
    if(nrow(df)==0) {return (Inf)}
    return (min(df$julian))
}

get_lat_quants <- function(df, n) {
    df$group <- ntile(df$latitude, n)
    
    a <- df %>% group_by(group) %>% summarize(avglat=mean(latitude))
    
    b <- df %>% group_by(group) %>% group_map(~first_fruit(.x))
    
    a <- bind_cols(a, do.call(rbind.data.frame, b)[,1], .name_repair = "unique_quiet") %>%
        mutate(firstfruit = ...3) %>% select(-...3)
    a <- remove_missing(a, finite=TRUE, na.rm=TRUE)
    
    return (a)
}

get_ele_quants <- function(df, n) {
    df$group <- ntile(mivi_annotated$elevation, n)
    
    a <- df %>% group_by(group) %>% summarize(avgele=mean(elevation))
    
    b <- df %>% group_by(group) %>% group_map(~first_fruit(.x))
    
    a <- bind_cols(a, do.call(rbind.data.frame, b)[,1], .name_repair = "unique_quiet") %>%
        mutate(firstfruit = ...3) %>% select(-...3)
    a <- remove_missing(a, finite=TRUE, na.rm=TRUE)
    
    return (a)
}

# Loading data
mivi_all <- read.csv("./MIVI-ALL.csv") %>%
    mutate(date=as.Date(observed_on, format="%Y-%m-%d")) %>% select(-observed_on)

mivi_young <- read.csv("MIVI-YOUNG.csv") %>%
    mutate(stage="Vegetation")

mivi_flowering <- read.csv("./MIVI-FLOWERING.csv") %>%
    mutate(stage="Flowering")
mivi_flowering <- mivi_flowering %>% left_join(mivi_all, by="id")

mivi_fruiting <- read.csv("./MIVI-FRUITING.csv") %>%
    mutate(stage="Fruiting")

mivi_all <- mivi_all %>% left_join(mivi_young, by="id")
mivi_all <- mivi_all %>% left_join(mivi_fruiting, by="id") %>%
    mutate(stage = coalesce(stage.x, stage.y)) %>% select(-stage.x, -stage.y)
mivi_all <- rbind(mivi_all, mivi_flowering)

rm(mivi_young, mivi_flowering, mivi_fruiting)

# Retrieve elevations
mivi_annotated <- mivi_all %>% filter(!is.na(stage))

coords <- data.frame(x=mivi_annotated$longitude,
                     y=mivi_annotated$latitude, ele_id=mivi_annotated$id)

elevations <- get_elev_point(coords, prj=4326, src="epqs")

mivi_all <- mivi_all %>% left_join(elevations, by=join_by("id" == "ele_id")) %>%
    select(-elev_units, -geometry)

rm(coords, elevations)

write.csv(mivi_all, file="./MIVI-PROCESSED.csv", na='')

# Data processing
mivi_all <- subset(mivi_all, id != "130398055")

mivi_all <- mivi_all %>% mutate(julian = as.integer(strftime(date, format="%j")))

mivi_all$stage <- factor(mivi_all$stage, ordered=TRUE,
                         levels=c("Vegetation", "Flowering", "Fruiting"))

mivi_annotated <- mivi_all %>% filter(!is.na(stage))

# Latitude
quants = data.frame()

for(i in 10:300) {
    a <- get_lat_quants(mivi_annotated, i)
    quants <- rbind(quants, data.frame(i, cor(a$firstfruit, a$avglat)))
}
rm(i, a)

colnames(quants) <- c("n quantiles", "correlation")
plot(quants)

y <- which.min(quants$corr)
n <- quants[y,1]
paste0("n quantiles for best correlation is: ", n)

rm(quants, y)

data <- get_lat_quants(mivi_annotated, n)
model_lat <- lm(firstfruit~avglat, data=data)

print(
    ggplot(data, aes(avglat, firstfruit)) + geom_point() + geom_smooth(method='lm') +
        ylab("Julian day") + xlab("mean latitude") + labs(title="First Fruiting Day by Latitude")
)

res <- resid(model_lat)
qqnorm(res)
qqline(res)

paste0("Average absolute residual: ", format(mad(res), digits=6))

cor.test(data$firstfruit, data$avglat, alternative="less")

correlation_matrix(data, use="lower")

rm(data, model_lat, res)

# Elevation
quants = data.frame()

for(i in 10:300) {
    a <- get_ele_quants(mivi_annotated, i)
    quants <- rbind(quants, data.frame(i, cor(a$firstfruit, a$avgele)))
}
rm(i, a)

colnames(quants) <- c("n quantiles", "correlation")
plot(quants)

y <- which.min(quants$corr)
n <- quants[y,1]
paste0("n quantiles for best correlation is: ", n)

rm(quants, y)

data = get_ele_quants(mivi_annotated, n)
model_ele <- lm(firstfruit~avgele, data=data)

ggplot(data, aes(avgele, firstfruit)) + geom_point() + geom_smooth(method='lm') +
    ylab("Julian day") + xlab("mean elevation") + labs(title="First Fruiting Day by Elevation")

res <- resid(model_ele)
qqnorm(res)
qqline(res)

paste0("Average absolute residual: ", format(mad(res), digits=6))

cor.test(data$firstfruit, data$avgele, alternative="less")

rm(data, model_ele, res)

# Tables
table1(~ place_country_name, data=mivi_all %>% mutate(stage=addNA(stage)))

table1(~ latitude + longitude + elevation + julian + place_country_name | stage, data=mivi_all %>% mutate(stage=addNA(stage)))

