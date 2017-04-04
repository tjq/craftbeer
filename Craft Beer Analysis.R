library(dplyr)

abv.by.style <- beers %>% group_by(style) %>% summarise(avg.abv = mean(abv), min.abv = min(abv), max.abv = max(abv))
abv.by.style$style <- factor(abv.by.style$style, levels = abv.by.style$style[order(abv.by.style$avg.abv)])

ibu.by.style <- beers %>% group_by(style) %>% summarise(avg.ibu = mean(ibu), min.ibu = min(ibu), max.ibu = max(ibu))
ibu.by.style$style <- factor(ibu.by.style$style, levels = ibu.by.style$style[order(ibu.by.style$avg.ibu)])

abv.limits <- aes(ymax = max.abv, ymin = min.abv)
abv.by.style <- abv.by.style[complete.cases(abv.by.style),]

ibu.limits <- aes(ymax = max.ibu, ymin = min.ibu)

g1 <- ggplot(abv.by.style, aes(x=as.factor(style), y = avg.abv)) + geom_point() + geom_errorbar(abv.limits) +
  theme(axis.text.x = element_text(angle=90))

plot(g1)

g2 <- ggplot(ibu.by.style, aes(x=as.factor(style), y = avg.ibu)) + geom_point() + geom_errorbar(ibu.limits) +
  theme(axis.text.x = element_text(angle=90))

plot(g2)

