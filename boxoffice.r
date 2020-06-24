## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, warning = F, message = F)


## ----libraries-----------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(patchwork)
library(corrr)
library(modelr)
library(gameofthrones)
library(MASS)
library(olsrr)
library(xgboost)
library(mltools)
library(formattable)
library(e1071)
library(kableExtra)
library(texreg)

## ----custom_colors-------------------------------------------------------------------------------------------------------------------------------------------------------
customGreen0 <- "#E0EEEE"
customGreen <- "#71CA97"
customRed <- "#ff7f7f"
customBlue <- "#00B2EE"


## ----read_boxoffice, include = F-----------------------------------------------------------------------------------------------------------------------------------------
boxoffice <- read_csv("boxoffice2.csv")


## ----new_variables, cache=TRUE-------------------------------------------------------------------------------------------------------------------------------------------
boxoffice$month <- factor(boxoffice$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

## create new categorical variables to account for major vs. mid vs. minor (and top 15 vs. other)

boxoffice$studio.status.threecat <- rep(0, nrow(boxoffice))

for(i in 1:nrow(boxoffice)){
  n <- boxoffice %>% filter(distributor == distributor[i]) %>%
    nrow()
  boxoffice$studio.status.threecat[i] <- ifelse(n <= 10, "minor", ifelse(n <= 45, "mid", "major"))
}

# reorder studio status categorical variables (minor-mid-major)

boxoffice$studio.status.threecat <- factor(boxoffice$studio.status.threecat, levels = c("minor", "mid", "major"))


## ----boxoffice_year, cache=TRUE------------------------------------------------------------------------------------------------------------------------------------------

avg.boxoffice.byyear <- tapply(boxoffice$dom.box.mil, boxoffice$year, mean) %>% enframe(name = "year", value = "boxoffice")

ggplot(avg.boxoffice.byyear, aes(year, boxoffice, group = 1)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 100)) +
  ylab("Average boxoffice") +
  ggtitle("Average boxoffice performance (millions, USD) by year")


## ----summary_plots_top15, cache=TRUE-------------------------------------------------------------------------------------------------------------------------------------
table.majmid <- boxoffice %>% group_by(distributor) %>%
  filter(n() > 10) %>% summarize(box.office.avg = mean(dom.box.mil), total.movies = n(), 
                                 rt.critics.avg = mean(rt.critics), theaters.avg = mean(dom.theaters.open, na.rm = T)) %>%
  arrange(box.office.avg) # major jump from studios that distributed 11 to 22 films and those who distributed 47+

g.d1 <- ggplot(table.majmid) +
  geom_col(aes(reorder(distributor, total.movies), total.movies, fill = distributor)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Daenerys") +
  ylab("Films released") +
  xlab("Studio") +
  ggtitle("Total number of movies released by top 15 studios, 2014-2018")

g.d2 <- ggplot(table.majmid) +
  geom_col(aes(reorder(distributor, box.office.avg), box.office.avg, fill = distributor)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Daenerys") +
  ylab("Average boxoffice") +
  xlab("Studio") +
  ggtitle("Average boxoffice performance (millions, USD) per film for top 15 studios,\n2014-2018")

g.d3 <- ggplot(table.majmid) +
  geom_col(aes(reorder(distributor, rt.critics.avg), rt.critics.avg, fill = distributor)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Daenerys") +
  ylab("Average score") +
  xlab("Studio") +
  ggtitle("Average Rotten Tomatoes score (critics) per film for top 15 studios, 2014-2018")

g.d4 <- ggplot(table.majmid) +
  geom_col(aes(reorder(distributor, theaters.avg), theaters.avg, fill = distributor)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Daenerys") +
   ylab("Average number of theaters on release") +
  xlab("Studio") +
  ggtitle("Average number of theaters on release per film for top 15 studios, 2014-2018")


## ----total_movies_top15, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------
g.d1


## ----average_boxoffice_top15, cache=TRUE---------------------------------------------------------------------------------------------------------------------------------
g.d2


## ----average_RT_score_top15, cache=TRUE----------------------------------------------------------------------------------------------------------------------------------
g.d3


## ----discontinuity_RT_score_boxoffice, cache=TRUE------------------------------------------------------------------------------------------------------------------------

ggplot() +
  geom_smooth(data = filter(boxoffice, rt.critics < 60), mapping = aes(rt.critics, dom.box.mil), method = "lm") +
  geom_smooth(data = filter(boxoffice, rt.critics >= 60), mapping = aes(rt.critics, dom.box.mil), method = "lm") +
  geom_vline(xintercept = 60, color = "red") +
  xlab("Score") +
  ylab("Boxoffice") +
  ggtitle("Discontinuity plot of Rotten Tomatoes score and domestic boxoffice\n(millions, USD)") +
  coord_cartesian(expand=F)



## ----average_theaters_top15, cache=TRUE----------------------------------------------------------------------------------------------------------------------------------
g.d4


## ----correlation_matrix_top15, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------
cor.table <- table.majmid %>% dplyr::select(-distributor) %>% correlate() %>% rearrange() %>%
  mutate(box.office.avg = round(box.office.avg, 2),
         total.movies = round(total.movies, 2),
         rt.critics.avg = round(rt.critics.avg, 2),
         theaters.avg = round(theaters.avg, 2)) %>%
  rename(` ` = rowname,
         `Average boxoffice performance` = box.office.avg,
         `Total movies` = total.movies,
         `Average Rotten Tomatoes score (critics)` = rt.critics.avg,
         `Average number of theaters` = theaters.avg)

cor.table$` ` <- c("Average number of theaters", "Total movies", "Average boxoffice performance", "Average Rotten Tomatoes score (critics)")

cor.table[is.na(cor.table)] <- ""

sign_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black")))
            )

format_table(cor.table, align = c("l", "c", "c", "c", "r"),
             list(
               ` ` = formatter("span", style = ~ style(font.weight = "bold", color = c("#454545"))),
               `Average boxoffice performance` = sign_formatter,
               `Total movies` = sign_formatter,
               `Average Rotten Tomatoes score (critics)` = sign_formatter,
               `Average number of theaters` = sign_formatter
               )
             ) %>%
  kable_styling(full_width = F) %>% column_spec(1, width = "10em") %>% column_spec(2, width = "10em") %>% column_spec(3, width = "10em") %>%
  column_spec(4, width = "10em") %>% column_spec(5, width = "10em") %>% add_header_above(c("Correlations, studio-level values" = 5))



## ----average_boxoffice_studio_status, cache=TRUE-------------------------------------------------------------------------------------------------------------------------
tab2 <- boxoffice %>% group_by(.$studio.status.threecat) %>%
  summarize(dom.box.mil = round(mean(dom.box.mil), 2)) %>%
  rename(`Studio status` = ".$studio.status.threecat", `Average domestic boxoffice (millions)` = dom.box.mil)

format_table(tab2,
             align = c("l", "r"),
             list(
               `Average domestic boxoffice (millions)` = color_tile(customGreen0, customGreen),
               `Studio status` = formatter("span", style = ~ style(color = "grey",font.weight = "bold", width = "50%"))
             )) %>%
  kable_styling(full_width = F) %>% column_spec(2, width = "10em")



## ----boxplot_boxoffice_all, cache=TRUE-----------------------------------------------------------------------------------------------------------------------------------
ggplot(boxoffice, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() + coord_flip()


## ----boxplot_majors, cache=TRUE------------------------------------------------------------------------------------------------------------------------------------------
is_outlier <- function(x, y) {
  return(x < quantile(x, 0.25) - y * IQR(x) | x > quantile(x, 0.75) + y * IQR(x))
}

dat.maj <- boxoffice %>% filter(studio.status.threecat == "major") %>% mutate(outlier = ifelse(is_outlier(dom.box.mil, 3), name, ""))

ggplot(dat.maj, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() +
  geom_text_repel(mapping = aes(label = outlier), hjust = -0.1) +
  xlab("Studio status") +
  ylab("Boxoffice") +
  ggtitle("Boxplot of domestic boxoffice performance (millions, USD) for major studios")


## ----boxplot_mids, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
dat.mid <- boxoffice %>% filter(studio.status.threecat == "mid") %>% mutate(outlier = ifelse(is_outlier(dom.box.mil, 1.5), name, ""))

ggplot(dat.mid, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() +
  geom_text_repel(mapping = aes(label = outlier), hjust = -0.1) +
  xlab("Studio status") +
  ylab("Boxoffice") +
  ggtitle("Boxplot of domestic boxoffice performance (millions, USD) for mid-sized studios")


## ----boxplot_minors, cache=TRUE------------------------------------------------------------------------------------------------------------------------------------------
dat.min <- boxoffice %>% filter(studio.status.threecat == "minor") %>% mutate(outlier = ifelse(is_outlier(dom.box.mil, 1.5), name, ""))

ggplot(dat.min, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() +
  geom_text_repel(mapping = aes(label = outlier), hjust = -0.1) +
  xlab("Studio status") +
  ylab("Boxoffice") +
  ggtitle("Boxplot of domestic boxoffice performance (millions, USD) for minor studios")


## ----density_plots, cache=TRUE-------------------------------------------------------------------------------------------------------------------------------------------
boxoffice %>% filter(studio.status.threecat == "major") %>% ggplot(aes(dom.box.mil)) +
  geom_density(fill = "gray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(boxoffice$dom.box.mil[boxoffice$studio.status.threecat == "major"]), color = "red") +
  xlab("Boxoffice") +
  ggtitle("Density plot of domestic boxoffice performance (millions, USD) for major studios")

boxoffice %>% filter(studio.status.threecat == "mid") %>% ggplot(aes(dom.box.mil)) +
  geom_density(fill = "gray", color = "black", alpha = 0.7) + 
  geom_vline(xintercept = mean(boxoffice$dom.box.mil[boxoffice$studio.status.threecat == "mid"]), color = "red") +
  xlab("Boxoffice") +
  ggtitle("Density plot of domestic boxoffice performance (millions, USD)\nfor mid-sized studios")

boxoffice %>% filter(studio.status.threecat == "minor") %>% ggplot(aes(dom.box.mil)) +
  geom_density(fill = "gray", color = "black", alpha = 0.7) + 
  geom_vline(xintercept = mean(boxoffice$dom.box.mil[boxoffice$studio.status.threecat == "minor"]), color = "red") +
  xlab("Boxoffice") +
  ggtitle("Density plot of domestic boxoffice performance (millions, USD) for minor studios")


## ----correlations_all_numeric, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------
bo.corrs <- boxoffice %>% dplyr::select(dom.box.mil, rt.critics, rt.audience, director.avg.rt.adj, lead.actor.avg.rt.adj, dom.theaters.open, dom.theaters.max, open.percent.dom.box, dom.open.mil, year) %>% correlate() %>%
  mutate(dom.box.mil = round(dom.box.mil, 2),
         rt.critics = round(rt.critics, 2),
         rt.audience = round(rt.audience, 2),
         director.avg.rt.adj = round(director.avg.rt.adj, 2),
         lead.actor.avg.rt.adj = round(lead.actor.avg.rt.adj, 2),
         dom.theaters.open = round(dom.theaters.open, 2),
         dom.theaters.max = round(dom.theaters.max, 2),
         open.percent.dom.box = round(open.percent.dom.box, 2),
         dom.open.mil = round(dom.open.mil, 2),
         year = round(year, 2)) %>%
  rename(` ` = rowname,
         `Total boxoffice (millions)` = dom.box.mil,
         `Rotten Tomatoes score (critics)` = rt.critics,
         `Rotten Tomatoes score (audience)` = rt.audience,
         `Average director RT score (adjusted)` = director.avg.rt.adj,
         `Average lead actor RT score (adjusted)` = lead.actor.avg.rt.adj,
         `Number of theaters on release` = dom.theaters.open,
         `Maximum number of theaters` = dom.theaters.max,
         `Opening as a percentage of total boxoffice` = open.percent.dom.box,
         `Opening boxoffice (millions)` = dom.open.mil)

bo.corrs$` ` <- colnames(bo.corrs)[-1]

bo.corrs[is.na(bo.corrs)] <- ""

format_table(bo.corrs, align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "r"),
             list(
               ` ` = formatter("span", style = ~ style(font.weight = "bold", color = c("#454545"))),
               `Total boxoffice (millions)` = sign_formatter,
               `Rotten Tomatoes score (critics)` = sign_formatter,
               `Rotten Tomatoes score (audience)` = sign_formatter,
               `Average director RT score (adjusted)` = sign_formatter,
               `Average lead actor RT score (adjusted)` = sign_formatter,
               `Number of theaters on release` = sign_formatter,
               `Maximum number of theaters` = sign_formatter,
               `Opening as a percentage of total boxoffice` = sign_formatter,
               `Opening boxoffice (millions)` = sign_formatter,
               year = sign_formatter
             )
) %>% add_header_above(c("Correlations, all numeric variables" = 11))


## ----RT_scores_robustness, cache = T-------------------------------------------------------------------------------------------------------------------------------------
rt.sample <- read_csv("rt.sample.csv")

critics.corrs <- rt.sample %>% dplyr::select(rt.critics, rt.critics.day1, rt.critics.day3, rt.critics.day14, rt.critics.day28) %>% correlate() %>% rename(` ` = rowname) %>% mutate_at(vars(-` `), funs(round(., 4)))

critics.corrs[is.na(critics.corrs)] <- ""

critics.corrs %>% kable() %>% kable_styling(full_width = F) %>% add_header_above(c("Correlations, Rotten Tomatoes scores (critics), day of release and times after" = 6))

audience.corrs <- rt.sample %>% dplyr::select(rt.audience, rt.audience.day1, rt.audience.day3, rt.audience.day14, rt.audience.day28) %>% correlate() %>% rename(` ` = rowname) %>% mutate_at(vars(-` `), funs(round(., 4)))

audience.corrs[is.na(audience.corrs)] <- ""

audience.corrs %>% kable() %>% kable_styling(full_width = F) %>% add_header_above(c("Correlations, Rotten Tomatoes scores (audience), day of release and times after" = 6))


## ----RT_scores_year, cache=TRUE------------------------------------------------------------------------------------------------------------------------------------------
tab1 <- boxoffice %>% group_by(year) %>% summarize(rt.critics = mean(rt.critics))
tab2 <- boxoffice %>% group_by(year) %>% summarize(rt.audience = mean(rt.audience))

ggplot() +
  geom_line(tab1, mapping = aes(year, rt.critics, color = "blue"), size = 2) +
  geom_line(tab2, mapping = aes(year, rt.audience, color = "green"), size = 2) +
  ylim(c(0, 75)) +
  ylab("Score") +
  ggtitle("Average Rotten Tomatoes score (critics and audience) by year") +
  scale_color_manual(name = "Score type", values = c("blue" = "blue", "green" = "green"), labels = c("Critics", "Audience"), guide = "legend") +
  coord_cartesian(expand=F)


## ----RT_scores_studio_status, cache=TRUE---------------------------------------------------------------------------------------------------------------------------------
tab5 <- boxoffice %>% group_by(studio.status.threecat) %>% summarize(rt.critics = mean(rt.critics), rt.audience = mean(rt.audience))

tab5 %>% ggplot() +
  geom_point(aes(studio.status.threecat, rt.critics, color = "blue"), size = 2) +
  geom_point(aes(studio.status.threecat, rt.audience, color = "green"), size = 2) +
  coord_cartesian(ylim = c(0, 75)) +
  ylab("Score") +
  xlab("Studio status") +
  ggtitle("Average Rotten Tomatoes score (critics and audience)\nby studio status") +
  scale_color_manual(name = "Score type", values = c("blue" = "blue", "green" = "green"), labels = c("Critics", "Audience"), guide = "legend")


## ----density_plots_RT_studio_status, cache = T---------------------------------------------------------------------------------------------------------------------------
ggplot(boxoffice) + geom_density(mapping = aes(rt.critics, fill = "blue", color = "blue"), alpha = 0.5) +
  geom_density(mapping = aes(rt.audience, fill = "green", color = "green"), alpha = 0.7) +
  facet_wrap(facets = vars(studio.status.threecat), nrow = 2) +
  xlab("Rotten Tomatoes score") +
  ggtitle("Density plots of Rotten Tomatoes scores (critics and audience)\nby studio status") +
  scale_color_identity() +
  scale_fill_identity(name = "Score type", breaks = c("blue", "green"), labels = c("Critics", "Audience"), guide = "legend") +
  coord_cartesian(expand=F)


## ----theaters_density, cache = T-----------------------------------------------------------------------------------------------------------------------------------------

tab.theaters <- boxoffice %>% group_by(year) %>% summarize(dom.theaters.open = mean(dom.theaters.open, na.rm = T))

ggplot() +
  geom_line(tab.theaters, mapping = aes(year, dom.theaters.open), size = 2, color = "orange") +
  ylab("Theaters") +
  ylim(0, 3000) +
  ggtitle("Average number of theaters on release by year") +
  coord_cartesian(expand=F)

options(scipen = 999)

ggplot(boxoffice) + geom_density(mapping = aes(dom.theaters.open), fill = "orange", color = "orange", alpha = 0.5) +
  facet_wrap(facets = vars(studio.status.threecat), nrow = 2) +
  xlab("Theaters") +
  ggtitle("Density plots of theaters on release\nby studio status") +
  coord_cartesian(expand=F)



## ----month_plots, cache=TRUE---------------------------------------------------------------------------------------------------------------------------------------------
tab.month <- boxoffice %>% group_by(month) %>% summarize(n = n(), avg.boxoffice = mean(dom.box.mil), avg.rt.critics = mean(rt.critics), 
                                            avg.rt.audience = mean(rt.audience), avg.theaters = mean(dom.theaters.open, na.rm = T))


g.m1 <- ggplot(data = tab.month) +
  geom_col(aes(month, n, fill = month)) +
  scale_fill_got_d(option = "margaery") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  ylab("Movies released") +
  ggtitle("Number of movies released\nin each month")

g.m2 <- ggplot(data = tab.month) +
  geom_col(aes(month, avg.boxoffice, fill = month)) +
  scale_fill_got_d(option = "margaery") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  ylab("Average Boxoffice") +
  ggtitle("Average boxoffice performance by month")

g.m3 <- ggplot(data = tab.month) +
  geom_col(aes(month, avg.rt.critics, fill = month)) +
  scale_fill_got_d(option = "margaery") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  ylab("Score") +
  ggtitle("Average Rotten Tomatoes score\n(critics) by month")

g.m4 <- ggplot(data = tab.month) +
  geom_col(aes(month, avg.rt.audience, fill = month)) +
  scale_fill_got_d(option = "margaery") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  ylab("Score") +
  ggtitle("Average Rotten Tomatoes score\n(audience) by month")

g.m5 <- ggplot(data = tab.month) +
  geom_col(aes(month, avg.theaters, fill = month)) +
  scale_fill_got_d(option = "margaery") +
  ylab("Average theaters") +
  ggtitle("Average number of theaters\non release by month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


## ----month_plots_num_theaters, cache = T---------------------------------------------------------------------------------------------------------------------------------
g.m1 + g.m5


## ----month_plots_RT_boxoffice, cache = T---------------------------------------------------------------------------------------------------------------------------------
(g.m3 + g.m4) / g.m2


## ---- plots_genre, cache=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
genre.sum1 <- boxoffice %>%
  summarize(action = mean(genreAction), animation = mean(genreAnimation), art_foreign = mean(genreArtForeign),
            classic = mean(genreClassic), comedy = mean(genreComedy), documentary = mean(genreDocumentary),
            drama = mean(genreDrama), horror = mean(genreHorror), family = mean(genreFamily), mystery = mean(genreMystery),
            romance = mean(genreRomance), scifi = mean(genreSciFi)) %>%
  gather(action, animation, art_foreign, classic, comedy, documentary, drama, horror, family, mystery, romance, scifi, key = "genre", value = "proportion")

genre.sum2 <- boxoffice %>%
  summarize(action = mean(dom.box.mil[boxoffice$genreAction == 1]), animation = mean(dom.box.mil[boxoffice$genreAnimation == 1]),
            art_foreign = mean(dom.box.mil[boxoffice$genreArtForeign == 1]), classic = mean(dom.box.mil[boxoffice$genreClassic == 1]), 
            comedy = mean(dom.box.mil[boxoffice$genreComedy == 1]), documentary = mean(dom.box.mil[boxoffice$genreDocumentary == 1]), 
            drama = mean(dom.box.mil[boxoffice$genreDrama == 1]), horror = mean(dom.box.mil[boxoffice$genreHorror == 1]), 
            family = mean(dom.box.mil[boxoffice$genreFamily == 1]), mystery = mean(dom.box.mil[boxoffice$genreMystery == 1]), 
            romance = mean(dom.box.mil[boxoffice$genreRomance == 1]), scifi = mean(dom.box.mil[boxoffice$genreSciFi == 1])) %>%
  gather(action, animation, art_foreign, classic, comedy, documentary, drama, horror, family, mystery, romance, scifi, key = "genre", value = "boxoffice.avg")

genre.sum3 <- boxoffice %>%
  summarize(action = mean(rt.critics[boxoffice$genreAction == 1]), animation = mean(rt.critics[boxoffice$genreAnimation == 1]),
            art_foreign = mean(rt.critics[boxoffice$genreArtForeign == 1]), classic = mean(rt.critics[boxoffice$genreClassic == 1]), 
            comedy = mean(rt.critics[boxoffice$genreComedy == 1]), documentary = mean(rt.critics[boxoffice$genreDocumentary == 1]), 
            drama = mean(rt.critics[boxoffice$genreDrama == 1]), horror = mean(rt.critics[boxoffice$genreHorror == 1]), 
            family = mean(rt.critics[boxoffice$genreFamily == 1]), mystery = mean(rt.critics[boxoffice$genreMystery == 1]), 
            romance = mean(rt.critics[boxoffice$genreRomance == 1]), scifi = mean(rt.critics[boxoffice$genreSciFi == 1])) %>%
  gather(action, animation, art_foreign, classic, comedy, documentary, drama, horror, family, mystery, romance, scifi, key = "genre", value = "rt.critics")

genre.sum4 <- boxoffice %>%
  summarize(action = mean(rt.audience[boxoffice$genreAction == 1]), animation = mean(rt.audience[boxoffice$genreAnimation == 1]),
            art_foreign = mean(rt.audience[boxoffice$genreArtForeign == 1]), classic = mean(rt.audience[boxoffice$genreClassic == 1]), 
            comedy = mean(rt.audience[boxoffice$genreComedy == 1]), documentary = mean(rt.audience[boxoffice$genreDocumentary == 1]), 
            drama = mean(rt.audience[boxoffice$genreDrama == 1]), horror = mean(rt.audience[boxoffice$genreHorror == 1]), 
            family = mean(rt.audience[boxoffice$genreFamily == 1]), mystery = mean(rt.audience[boxoffice$genreMystery == 1]), 
            romance = mean(rt.audience[boxoffice$genreRomance == 1]), scifi = mean(rt.audience[boxoffice$genreSciFi == 1])) %>%
  gather(action, animation, art_foreign, classic, comedy, documentary, drama, horror, family, mystery, romance, scifi, key = "genre", value = "rt.audience")


genre.tab <- left_join(genre.sum1, genre.sum2) %>% left_join(genre.sum3) %>% left_join(genre.sum4)

g.g1 <- ggplot(genre.tab) +
  geom_col(aes(reorder(genre, proportion), proportion, fill = genre)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Lannister") +
  xlab("Genre") +
  ggtitle("Proportion of films in each genre")

g.g2 <- ggplot(genre.tab) +
  geom_col(aes(reorder(genre, boxoffice.avg), boxoffice.avg, fill = genre)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Lannister") +
  ylab("Boxoffice") +
  xlab("Genre") +
  ggtitle("Average boxoffice performance\n(millions, USD) by genre")

g.g3 <- ggplot(genre.tab) +
  geom_col(aes(reorder(genre, rt.critics), rt.critics, fill = genre)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Lannister") +
  ylab("Score") +
  xlab("Genre") +
  ggtitle("Average Rotten Tomatoes score\n(critics) by genre")

g.g4 <- ggplot(genre.tab) +
  geom_col(aes(reorder(genre, rt.audience), rt.audience, fill = genre)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Lannister") +
  ylab("Score") +
  xlab("Genre") +
  ggtitle("Average Rotten Tomatoes score\n(audience) by genre")

g.g1 + g.g2 + g.g3 + g.g4


## ----lm_models_RT_scores, cache=TRUE-------------------------------------------------------------------------------------------------------------------------------------

m2a <- lm(dom.box.mil.log ~ rt.critics, data = boxoffice)
m2b <- lm(dom.box.mil.log ~ rt.audience, data = boxoffice)

m2a.dat <- broom::tidy(m2a) %>% mutate_at(vars(-term), funs(round(., 4)))
m2b.dat <- broom::tidy(m2b) %>% mutate_at(vars(-term), funs(round(., 4)))

m2a.dat %>% format_table() %>%
kable_styling(full_width = F) %>% add_header_above(c("Results for RT critics model" = 5)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(m2a.dat), color = c("#474747"), background = c("#E0EEEE"))

kable(m2b.dat) %>%
kable_styling(full_width = F) %>% add_header_above(c("Results for RT audience model" = 5)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(m2b.dat), color = c("#474747"), background = c("#E0EEEE"))


## ----plots_log_RT_models, cache = T--------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = boxoffice) +
  geom_point(aes(rt.critics, dom.box.mil.log), alpha = 0.5, color = "blue") +
  geom_abline(slope = m2a.dat$estimate[2], intercept = m2a.dat$estimate[1], color = "red") +
  xlab("Score") +
  ylab("Boxoffice") +
  ggtitle("Boxoffice performance (millions, USD, logged) \n regressed on Rotten Tomatoes score (critics)")

ggplot(data = boxoffice) +
  geom_point(aes(rt.audience, dom.box.mil.log), alpha = 0.5, color = "green") +
  geom_abline(slope = m2b.dat$estimate[2], intercept = m2b.dat$estimate[1], color = "red") +
  xlab("Score") +
  ylab("Boxoffice") +
  ggtitle("Boxoffice performance (millions, USD, logged) \n regressed on Rotten Tomatoes score (audience)")


## ----plots_reg_RT_models, cache = T--------------------------------------------------------------------------------------------------------------------------------------
m2a.grid <- boxoffice %>% data_grid(rt.critics = seq(0, 100, 1)) %>% add_predictions(m2a) %>% mutate(pred = exp(pred))
m2b.grid <- boxoffice %>% data_grid(rt.audience = seq(0, 100, 1)) %>% add_predictions(m2b) %>% mutate(pred = exp(pred))

ggplot(m2a.grid, aes(rt.critics, pred)) +
  geom_line(color = "red") +
  geom_point(boxoffice, mapping = aes(rt.critics, dom.box.mil), alpha = 0.5, color = "blue") +
  xlab("Score") +
  ylab("Boxoffice") +
  ggtitle("Boxoffice performance (millions, USD) \n predicted by Rotten Tomatoes score (critics)") +
  coord_cartesian(expand=F)

ggplot(m2b.grid, aes(rt.audience, pred)) +
  geom_line(color = "red") +
  geom_point(boxoffice, mapping = aes(rt.audience, dom.box.mil), alpha = 0.5, color = "green") +
  xlab("Score") +
  ylab("Boxoffice") +
  ggtitle("Boxoffice performance (millions, USD) \n predicted by Rotten Tomatoes score (audience)") +
  coord_cartesian(expand=F)


## ----lm_model_full, cache=TRUE-------------------------------------------------------------------------------------------------------------------------------------------
m8d.2.1 <- lm(dom.box.mil.log ~ rt.critics + rt.audience + dom.theaters.open + year + month +
                genreAction + genreAnimation + genreArtForeign + genreClassic +
                genreComedy + genreDocumentary + genreDrama + genreHorror + genreFamily + genreMystery +
                genreRomance + genreSciFi + studio.status.threecat, data = boxoffice)

m8d.2.1.summary <- round(broom::glance(m8d.2.1), 2)
m8d.2.1.rmse <- round(modelr::rmse(m8d.2.1, boxoffice), 4)


## ----lm_model_full_interactions, cache=TRUE------------------------------------------------------------------------------------------------------------------------------
## final model (m8d.2.1) with interaction terms (rt.critics*studio.status.threecat, dom.theaters.open*studio.status.threecat, dom.theaters.open*month)

m8d.2.1.i <- lm(dom.box.mil.log ~ rt.critics*studio.status.threecat + rt.audience + dom.theaters.open*studio.status.threecat + year + dom.theaters.open*month +
                genreAction + genreAnimation + genreArtForeign + genreClassic +
                genreComedy + genreDocumentary + genreDrama + genreHorror + genreFamily + genreMystery +
                genreRomance + genreSciFi, data = boxoffice)

m8d.2.1.i.summary <- round(broom::glance(m8d.2.1.i), 2)
m8d.2.1.i.rmse <- round(modelr::rmse(m8d.2.1.i, boxoffice), 4)


## ----both_models, cache = T, results = 'asis'----------------------------------------------------------------------------------------------------------------------------
htmlreg(list(m8d.2.1, m8d.2.1.i),
          digits = 6, star.symbol = "\\*", doctype = F, center = F, caption = "")


## ----term_plot_model1, cache = T-----------------------------------------------------------------------------------------------------------------------------------------
m8d.2.1.dat <- broom::tidy(m8d.2.1)

m8d.2.1.dat[-1, ] %>% ggplot(aes(term, estimate, fill = term)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Lannister") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2,
                position = position_dodge(0.9)) +
  ylab("Estimated effect on box office (in millions) (logged)") +
  ggtitle("Estimated coefficients of predictors, model 1")


## ----plot_interaction_theaters_studio, cache=TRUE------------------------------------------------------------------------------------------------------------------------
# visualize dom.theaters.open*studio.status.threecat

dat <- data_grid(boxoffice, dom.theaters.open = seq_range(dom.theaters.open, 20), studio.status.threecat = c("minor", "mid", "major"),
          rt.critics = mean(boxoffice$rt.critics), rt.audience = mean(boxoffice$rt.audience), 
          year = 2016, month = "Dec", genreAction = 1, genreAnimation = 0, genreArtForeign = 0, genreClassic = 0,
          genreComedy = 0, genreDocumentary = 0, genreDrama = 0, genreHorror = 0, genreFamily = 0, genreMystery = 0,
          genreRomance = 0, genreSciFi = 0) %>% add_predictions(m8d.2.1.i)

dat$LoCI <- predict(m8d.2.1.i, newdata = dat, 
                 interval = "confidence", 
                 level = 0.95)[, 2] %>% exp()
dat$HiCI <- predict(m8d.2.1.i, newdata = dat, 
                        interval = "confidence", 
                        level = 0.95)[, 3] %>% exp()

ggplot(dat,
       aes(dom.theaters.open, exp(pred))) +
  geom_smooth(
    aes(
      ymin = LoCI, ymax = HiCI,
      fill = studio.status.threecat, color = studio.status.threecat
    ),
    stat = "identity") +
  xlab("Theaters") +
  ylab("Predicted boxoffice") +
  ggtitle("Predicted domestic boxoffice performance (millions, USD)\nand number of theaters at open, by studio status") +
  scale_color_discrete(guide = F) +
  labs(fill = "Studio status") +
  guides(fill = guide_legend(override.aes = list(linetype = 0))) +
  coord_cartesian(expand=F)


## ----plot_interactions_RT_studio, cache=T--------------------------------------------------------------------------------------------------------------------------------
# visualize rt.critics*studio.status.threecat

dat2 <- data_grid(boxoffice, dom.theaters.open = mean(dom.theaters.open, na.rm = T), studio.status.threecat = c("minor", "mid", "major"),
                 rt.critics = seq_range(rt.critics, 20), rt.audience = mean(boxoffice$rt.audience), 
                 year = 2016, month = "Dec", genreAction = 1, genreAnimation = 0, genreArtForeign = 0, genreClassic = 0,
                 genreComedy = 0, genreDocumentary = 0, genreDrama = 0, genreHorror = 0, genreFamily = 0, genreMystery = 0,
                 genreRomance = 0, genreSciFi = 0) %>% add_predictions(m8d.2.1.i)

dat2$LoCI <- predict(m8d.2.1.i, newdata = dat2, 
                    interval = "confidence", 
                    level = 0.9)[, 2] %>% exp()
dat2$HiCI <- predict(m8d.2.1.i, newdata = dat2, 
                    interval = "confidence", 
                    level = 0.9)[, 3] %>% exp()

g.r1 <- ggplot(dat2,
       aes(rt.critics, exp(pred))) +
  geom_smooth(
    aes(
      ymin = LoCI, ymax = HiCI,
      fill = studio.status.threecat, color = studio.status.threecat
    ),
    stat = "identity") +
  xlab("Score") +
  ylab("Predicted boxoffice") +
  ggtitle("Predicted domestic boxoffice performance (millions, USD)\nand Rotten Tomatoes score\n(critics), by studio status") +
  scale_color_discrete(guide = F) +
  labs(fill = "Studio status") +
  guides(fill = guide_legend(override.aes = list(linetype = 0))) +
  coord_cartesian(expand=F)

## with r.audience*studio.status.threecat interaction

m8d.2.1.i.v2 <- lm(dom.box.mil.log ~ rt.audience*studio.status.threecat + rt.critics + dom.theaters.open*studio.status.threecat + year + dom.theaters.open*month +
                genreAction + genreAnimation + genreArtForeign + genreClassic +
                genreComedy + genreDocumentary + genreDrama + genreHorror + genreFamily + genreMystery +
                genreRomance + genreSciFi, data = boxoffice)

dat4 <- data_grid(boxoffice, dom.theaters.open = mean(dom.theaters.open, na.rm = T), studio.status.threecat = c("minor", "mid", "major"),
                  rt.audience = seq_range(rt.audience, 20), rt.critics = mean(boxoffice$rt.critics), 
                  year = 2016, month = "Dec", genreAction = 1, genreAnimation = 0, genreArtForeign = 0, genreClassic = 0,
                  genreComedy = 0, genreDocumentary = 0, genreDrama = 0, genreHorror = 0, genreFamily = 0, genreMystery = 0,
                  genreRomance = 0, genreSciFi = 0) %>% add_predictions(m8d.2.1.i.v2)

dat4$LoCI <- predict(m8d.2.1.i.v2, newdata = dat4, 
                     interval = "confidence", 
                     level = 0.9)[, 2] %>% exp()
dat4$HiCI <- predict(m8d.2.1.i.v2, newdata = dat4, 
                     interval = "confidence", 
                     level = 0.9)[, 3] %>% exp()

g.r2 <- ggplot(dat4,
       aes(rt.audience, exp(pred))) +
  geom_smooth(
    aes(
      ymin = LoCI, ymax = HiCI,
      fill = studio.status.threecat, color = studio.status.threecat
    ),
    stat = "identity") +
  xlab("Score") +
  ylab("Predicted boxoffice") +
  ggtitle("Predicted domestic boxoffice performance (millions, USD)\nand Rotten Tomatoes score\n(audience), by studio status") +
  scale_color_discrete(guide = F) +
  labs(fill = "Studio status") +
  guides(fill = guide_legend(override.aes = list(linetype = 0))) +
  coord_cartesian(expand=F)

g.r1 + g.r2


## ----plot_interactions_theaters_month, cache = T-------------------------------------------------------------------------------------------------------------------------
# visualize dom.theaters.open*month

months.levels <- levels(boxoffice$month)

dat3 <- data_grid(boxoffice, dom.theaters.open = seq_range(dom.theaters.open, 20), studio.status.threecat = "major",
                  rt.critics = mean(boxoffice$rt.critics), rt.audience = mean(boxoffice$rt.audience), 
                  year = 2016, month = months.levels, genreAction = 1, genreAnimation = 0, genreArtForeign = 0, genreClassic = 0,
                  genreComedy = 0, genreDocumentary = 0, genreDrama = 0, genreHorror = 0, genreFamily = 0, genreMystery = 0,
                  genreRomance = 0, genreSciFi = 0) %>% add_predictions(m8d.2.1.i)

dat3$LoCI <- predict(m8d.2.1.i, newdata = dat3, 
                    interval = "confidence", 
                    level = 0.9)[, 2] %>% exp()
dat3$HiCI <- predict(m8d.2.1.i, newdata = dat3, 
                    interval = "confidence", 
                    level = 0.9)[, 3] %>% exp()

ggplot(dat3,
       aes(dom.theaters.open, exp(pred))) +
  geom_smooth(
    aes(
      ymin = LoCI, ymax = HiCI,
      fill = month, color = month
    ),
    stat = "identity") +
  xlab("Theaters") +
  ylab("Predicted boxoffice") +
  ggtitle("Predicted domestic boxoffice performance (millions, USD)\nand number of theaters at open, by month") +
  scale_color_discrete(guide = F) +
  labs(fill = "Month") +
  guides(fill = guide_legend(override.aes = list(linetype = 0))) +
  coord_cartesian(expand=F)


## ----robust_plots_lm_model_full_interactions, cache = TRUE---------------------------------------------------------------------------------------------------------------
boxoffice.nona <- boxoffice %>% dplyr::select(dom.box.mil.log, dom.box.mil, rt.critics, rt.audience, dom.theaters.open, year, month,
                                              genreAction, genreAnimation, genreArtForeign, genreClassic,
                                              genreComedy, genreDocumentary, genreDrama, genreHorror, genreFamily, genreMystery,
                                              genreRomance, genreSciFi, studio.status.threecat) %>% na.omit()
boxoffice.nona <- boxoffice.nona %>% mutate(pred = m8d.2.1.i$fitted.values, resid = m8d.2.1.i$residuals)

ggplot(boxoffice.nona, aes(pred, resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ylab('Residuals') +
  xlab("Predicted values") +
  ggtitle("Residual plot for model 2")

ggplot(boxoffice.nona, aes(pred, dom.box.mil.log)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ylab("Actual values") +
  xlab("Predicted values") +
  ggtitle("Predicted vs. actual values for model 2")


## ----robustness tests, cache=TRUE----------------------------------------------------------------------------------------------------------------------------------------
outliers <- ols_test_outlier(m8d.2.1.i)

mod2.cor.test <- ols_test_correlation(m8d.2.1.i)


## ----VIF_model2, cache = T-----------------------------------------------------------------------------------------------------------------------------------------------
m2.vif <- ols_vif_tol(m8d.2.1.i) %>% as.tibble()

m2.vif %>% format_table() %>%
kable_styling(full_width = F) %>% add_header_above(c("Robustness measures for model 2" = 3)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(m2.vif), color = c("#474747"), background = c("#E0EEEE"))


## ----VIF_model1, cache = T-----------------------------------------------------------------------------------------------------------------------------------------------
m1.vif <- ols_vif_tol(m8d.2.1) %>% as.tibble()

m1.vif %>% format_table() %>%
kable_styling(full_width = F) %>% add_header_above(c("Robustness measures for model 1" = 3)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(m1.vif), color = c("#474747"), background = c("#E0EEEE"))


## ----stepwise_model, cache = T-------------------------------------------------------------------------------------------------------------------------------------------
# stepwise model

k <- ols_step_forward_p(m8d.2.1)
# if criterion is R.squared of at least 0.6, then model is: dom.box.mil.log ~ dom.theaters.open + rt.audience + month
#  if criterion is C(p) < 2P, then dom.theaters.open + rt.audience + month + studio.status.threecat + rt.critics
k.tab <- tibble(predictors = k$predictors, mallows_cp = k$mallows_cp, rsquare = k$rsquare, rmse = k$rmse)
k.tab$idx <- row.names(k.tab) %>% as.numeric()

k.tab %>% dplyr::select(-idx) %>% format_table() %>%
kable_styling(full_width = F) %>% add_header_above(c("Results for forward selection stepwise model" = 4)) %>%
  column_spec(1, "10em") %>% row_spec(1:5, color = c("#474747"), background = c("#00CD00")) %>%
  row_spec(6:12, color = c("#474747"), background = c("#E0EEEE"))


## ----xgboost_model_setup, cache = T--------------------------------------------------------------------------------------------------------------------------------------
boxoffice.sub2 <- boxoffice %>% dplyr::select(dom.box.mil.log, dom.box.mil, rt.critics, rt.audience, dom.theaters.open, year, month,
                                              genreAction, genreAnimation, genreArtForeign, genreClassic,
                                              genreComedy, genreDocumentary, genreDrama, genreHorror, genreFamily, genreMystery,
                                              genreRomance, genreSciFi, studio.status.threecat) %>% na.omit()

boxoffice.sub2.onehot <- one_hot(data.table::data.table(boxoffice.sub2))

predictors2 <- data.matrix(boxoffice.sub2.onehot[, -c(1, 2)])
label <- boxoffice.sub2.onehot$dom.box.mil.log
test_idx <- sample(nrow(boxoffice.sub2), nrow(boxoffice.sub2) / 3)

# parameters

seed <- 2020
gamma <- 0
eta <- 0.1
max_depth <- 3
subsample <- 0.8
lambda <- 1000
nrounds <- 1000
set.seed(seed)

param.tab <- tibble(seed, gamma, eta, max_depth, subsample, lambda, nrounds)

param.tab %>% format_table() %>%
  kable_styling(full_width = F) %>% add_header_above(c("XGBoost Hyperparameters" = 7)) %>%
  column_spec(1, "10em") %>% row_spec(1, color = c("#474747"), background = c("#E0EEEE"))


## ----xgboost_model_compute, cache = T------------------------------------------------------------------------------------------------------------------------------------

xgb_penalty2 <- xgboost(data = predictors2[-test_idx, ],
                       label = label[-test_idx],
                       params = list(gamma = gamma, eta = eta, max_depth = max_depth, subsample = subsample, lambda = lambda),
                       objective = "reg:squarederror", nrounds = nrounds, verbose = 0)
pred_penalty2 <- predict(xgb_penalty2, predictors2[test_idx, ])

errors <- rbind(xgb_penalty2$evaluation_log)



## ----xgboost_importance_plot, cache = T----------------------------------------------------------------------------------------------------------------------------------
xgb_penalty2.imp <- xgb.importance(model = xgb_penalty2)
xgb.ggplot.importance(xgb_penalty2.imp, legend = F) + theme(legend.position = "none")


## ----xgb_fit_metrics, cache = T------------------------------------------------------------------------------------------------------------------------------------------
xgb.test <- tibble(pred = pred_penalty2, resid = label[test_idx] - pred_penalty2)

ss.tot <- sum((label[test_idx] - mean(label[test_idx]))^2)
ss.res <- sum((label[test_idx] - pred_penalty2)^2)

xgb.test.r.squared <- 1 - (ss.res / ss.tot)

ggplot(xgb.test) +
         geom_point(mapping = aes(pred, resid)) +
  geom_hline(yintercept = 0, color = "blue") +
  ylab('Residuals') +
  xlab("Predicted values") +
  ggtitle("Residual plot for XGBoost model")


## ----folds_model2, cache = T---------------------------------------------------------------------------------------------------------------------------------------------

folds <- 5
N <- nrow(boxoffice.nona)
fold_number <- sample(1:folds, N, replace = T)
predictors <- c("rt.critics", "rt.audience", "dom.theaters.open", "year", "month",
"genreAction", "genreAnimation", "genreArtForeign", "genreClassic",
"genreComedy", "genreDocumentary", "genreDrama", "genreHorror", "genreFamily", "genreMystery",
"genreRomance", "genreSciFi", "studio.status.threecat")

error <- vector(mode = "numeric", length = folds)
r.squared <- vector(mode = "numeric", length = folds)
for(i in 1:folds){
  fold_idx <- (1:N)[fold_number == i]
  mod <- lm(dom.box.mil.log ~ rt.critics*studio.status.threecat + rt.audience + dom.theaters.open*studio.status.threecat + year +
              dom.theaters.open*month + genreAction + genreAnimation + genreArtForeign + genreClassic +
              genreComedy + genreDocumentary + genreDrama + genreHorror + genreFamily + genreMystery +
              genreRomance + genreSciFi, data = boxoffice.nona[-fold_idx,])
  pred <- predict(mod, boxoffice.nona[fold_idx, predictors])
  actual <- as_vector(boxoffice.nona[fold_idx, c("dom.box.mil.log")])
  error[i] <- mltools::rmse(preds = pred, actuals = actual)
  ss.tot <- sum((actual - mean(actual, na.rm = T))^2)
  ss.res <- sum((actual - pred)^2)
  r.squared[i] <- 1 - (ss.res / ss.tot)
}

avg_error.mod2 <- round(mean(error), 4)
range_error.mod2 <- round(range(error), 4)

avg_r.squared.mod2 <- round(mean(r.squared), 4)
range_r.squared.mod2 <- round(range(r.squared), 4)



## ----folds_step_model, cache = T-----------------------------------------------------------------------------------------------------------------------------------------

folds <- 5
N <- nrow(boxoffice.nona)
fold_number <- sample(1:folds, N, replace = T)
predictors <- c("rt.critics", "rt.audience", "dom.theaters.open", "month", "studio.status.threecat")


error <- vector(mode = "numeric", length = folds)
r.squared <- vector(mode = "numeric", length = folds)
for(i in 1:folds){
  fold_idx <- (1:N)[fold_number == i]
  mod <- lm(dom.box.mil.log ~ rt.critics + rt.audience + dom.theaters.open + month + studio.status.threecat, data = boxoffice.nona[-fold_idx,])
  pred <- predict(mod, boxoffice.nona[fold_idx, predictors])
  actual <- as_vector(boxoffice.nona[fold_idx, c("dom.box.mil.log")])
  error[i] <- mltools::rmse(preds = pred, actuals = actual)
  ss.tot <- sum((actual - mean(actual, na.rm = T))^2)
  ss.res <- sum((actual - pred)^2)
  r.squared[i] <- 1 - (ss.res / ss.tot)
}

avg_error.mod3 <- round(mean(error), 4)
range_error.mod3 <- round(range(error), 4)

avg_r.squared.mod3 <- round(mean(r.squared), 4)
range_r.squared.mod3 <- round(range(r.squared), 4)


## ----xgb_folds, cache = T------------------------------------------------------------------------------------------------------------------------------------------------

folds <- 5
N <- nrow(boxoffice.sub2.onehot)
fold_number <- sample(1:folds, N, replace = T)

error <- vector(mode = "numeric", length = folds)
r.squared <- vector(mode = "numeric", length = folds)
for(i in 1:folds){
  fold_idx <- (1:N)[fold_number == i]
  xgb <- xgboost(data = predictors2[-fold_idx, ], label = label[-fold_idx],
                  params = list(gamma = gamma, eta = eta, max_depth = max_depth, subsample = subsample, lambda = lambda),
                 objective = "reg:squarederror", nrounds = nrounds, verbose = 0)
  pred <- predict(xgb, predictors2[fold_idx, ])
  error[i] <- mltools::rmse(preds = pred, actuals = label[fold_idx])
  ss.tot <- sum((label[fold_idx] - mean(label[fold_idx]))^2)
  ss.res <- sum((label[fold_idx] - pred)^2)
  r.squared[i] <- 1 - (ss.res / ss.tot)
}

avg_error.xgb <- round(mean(error), 4)
range_error.xgb <- round(range(error), 4)

avg_r.squared.xgb <- round(mean(r.squared), 4)
range_r.squared.xgb <- round(range(r.squared), 4)


## ----folds_table, cache = T----------------------------------------------------------------------------------------------------------------------------------------------

folds.tab <- tibble(Model = c("Model 2", "Stepwise model", "XGB model"), 
       `Average error` = c(avg_error.mod2, avg_error.mod3, avg_error.xgb),
       `Minimum error` = c(range_error.mod2[1], range_error.mod3[1], range_error.xgb[1]),
       `Maximum error` = c(range_error.mod2[2], range_error.mod3[2], range_error.xgb[2]),
       `Average r.squared` = c(avg_r.squared.mod2, avg_r.squared.mod3, avg_r.squared.xgb),
       `Minimum r.squared` = c(range_r.squared.mod2[1], range_r.squared.mod3[1], range_r.squared.xgb[1]),
       `Maximum r.squared` = c(range_r.squared.mod2[2], range_r.squared.mod3[2], range_r.squared.xgb[2]))

folds.tab %>% format_table(align = c("r", "c", "c", "c", "c", "c", "r"),
                           list(
                             Model = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                             `Average error` = color_tile(customGreen, "#E0EEEE"),
                             `Minimum error` = color_tile(customGreen, "#E0EEEE"),
                             `Maximum error` = color_tile(customGreen, "#E0EEEE"),
                             `Average r.squared` = color_tile("#E0EEEE", customGreen),
                             `Minimum r.squared` = color_tile("#E0EEEE", customGreen),
                             `Maximum r.squared` = color_tile("#E0EEEE", customGreen)
                           )) %>%
kable_styling(full_width = F) %>% add_header_above(c("Root mean squared error and r.squared statistics for three models" = 7)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(folds.tab), color = c("#474747"), background = c("#E0EEEE"))

