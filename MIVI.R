library(ggplot2)

mivi_young <- read.csv("./MIVI-YOUNG.csv")
mivi_flowering <- read.csv("./MIVI-FLOWERING.csv")
mivi_fruiting <- read.csv("./MIVI-FRUITING.csv")

ggplot(mivi_young, aes(x=longitude,y=latitude)) + geom_point()
ggplot(mivi_young, aes(x=longitude,y=latitude,color=place_state_name)) + geom_point()

ggplot(mivi_young, aes(x=latitude)) + geom_histogram(binwidth=0.2)
