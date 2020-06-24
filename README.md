---
title: "Boxoffice Analysis"
author: "Graham Odell"
date: "6/23/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F, warning = F, message = F)
```


```{r libraries}
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
```
```{r custom_colors}
customGreen0 <- "#E0EEEE"
customGreen <- "#71CA97"
customRed <- "#ff7f7f"
customBlue <- "#00B2EE"
```

# Project Overview

This project utilizes a custom dataset to explore patterns in boxoffice performance of films released between 2014 and 2019. The dataset includes variables measuring, among other factors, boxoffice performance (dollars earned in the North American market), prevalence of releases (number of theaters at opening), audience and critic reactions (Rotten Tomatoes scores), studio (name and size), genre and time of release (month and year). In this project, I focus on patterns relating to studio, month of release and Rotten Tomatoes score. This focus is driven by recent film industry commentary on the remarkable success of certain studios (e.g., Disney), the changing calendar of film releases (e.g., big-budget blockbusters released in February and March) and the outsized influence of review aggregators such as Rotten Tomatoes.

I try to illuminate patterns in the data and connections between the variables through standard statistical techniques, such as correlation matrices. linear regression models and machine learning algorithms, and extensive data visualization. More specifically, I use an OLS approach to estimate the effects of various predictors on the outcome variable of logged total boxoffice receipts in the domestic (i.e., North American) market and then compare these models with one generated through the XGBoost algorithm.

In brief, my major findings are that the number of theaters at opening is the strongest predictor of total boxoffice performance. Rotten Tomatoes scores are also substantial predictors, but perhaps not as much as recent film industry commentary suggests. The time of year of a release matters to some extent, especially in months such as December. When including other predictors, genre does not appear to have a major impact on the boxoffice success of a film, whereas the size of the studio (measured in number of movies released) does. A number of these findings are not surprising, but it is nonetheless useful to have quantitative evidence that confirms one's subjective impressions.



```{r read_boxoffice, include = F}
boxoffice <- read_csv("boxoffice2.csv")
```

```{r new_variables, cache=TRUE}
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
```

# Dataset Exploration

As can be seen in the plot below, there was a general increase in average boxoffice performance per film between 2014 and 2018. This trend makes sense given the long-term increase in ticket prices, though this probably does not explain all of the increase observed. 2016 appears to have been a down year, though only slightly.

```{r boxoffice_year, cache=TRUE}

avg.boxoffice.byyear <- tapply(boxoffice$dom.box.mil, boxoffice$year, mean) %>% enframe(name = "year", value = "boxoffice")

ggplot(avg.boxoffice.byyear, aes(year, boxoffice, group = 1)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 100)) +
  ylab("Average boxoffice") +
  ggtitle("Average boxoffice performance (millions, USD) by year")
```


## Studio-level patterns

The following set of plots summarizes some key measures of film studios' performance and industry influence. Note that for the sake of visual clarity, I have excluded studios that released ten movies or less in the five year period covered by the dataset. There are a total of 41 studios represented in the dataset, of which 15 are displayed in the following plots.


```{r summary_plots_top15, cache=TRUE}
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
```

The plot below displays the total number of movies released by each studio (more than ten total movies released). The "Big Seven" - Warner Bros., Universal, Fox, Lionsgate, Disney, Paramount and Sony - not surprisngly dominated the film schedule between 2014 and 2018. Indeed, we see a significant jump in the number of movies released between Focus Features and Sony Pictures. I use this cutoff to distinguish between midrange studios (between 10 and 45 movies released) and majors (more than 45 studios released). This cutoff is less arbitrary than the one used to distinguish between midrange studios and minor ones (ten movies or less), and, as we will see in the quantitative analyses below, the distinction between major and mid is more significant than the one between mid and minor when predicting boxoffice performance.

```{r total_movies_top15, cache=TRUE}
g.d1
```

The second plot displays how well the typical movie released by each studio did at the North American boxoffice. Studios are ordered from lowest average performance to highest. Disney is the clear industry leader, with its releases on average earning about 225 million dollars. Universal is a distant second with about 110 million dollars earned per movie. Notice that excluding Disney, there are no truly significant drops in average performance from studio to studio. The most significant of these other drops is between Paramount and Lionsgate, of about 25 million dollars.

In retrospect, it might make sense to include an additional variable that distinguishes Disney from non-Disney films. However, this approach would encourage overfitting since we would be using an estimate of boxoffice performance to predict boxoffice performance. Instead, by focusing on the number of movies released by a given studio, I am relying on a predictor that, at least in principle, is conceptually independent from the outcome I am trying to predict. The argument could be made, though, that past boxoffice performance determines how much cash a studio has on hand to make more films in the future. This is certainly true, but given that the purpose of the project is to predict individual film performance, characteristics of a film's studio should be considered in any analysis. Rather than measure these characteristics directly (which would require some form of multilevel modeling), I instead collapse any potentially significant studio-level factors into the studio variable and any other variables derived from it. Future additions to this project can take a more sophisticated multilevel approach to further tease out the studio characteristics that explain why some studios release better performing movies than others.

```{r average_boxoffice_top15, cache=TRUE}
g.d2
```

Moving on, we can visually inspect the relationship between studio and Rotten Tomatoes score. We again see a different ranking of studios, with Fox Searchlight boasting the best average score and Relativity Media at the bottom. The "Big Seven" are roughly evenly distributed across the rankings, with Sony having the sixth lowest average score and Disney having the second highest. Films from boutique studios such as Fox Searchlight, The Weinstein Company and Focus Features, not surprisingly, do very well with the critics. The high average score of Disney is notable, since the typical major studio on average puts out rather middling films, judged by critic reception (see Sony, Universal, Warner Bros. and Paramount - all around the 50% mark). Indeed, Disney is the only major studio that releases films with average critic scores of greater than 70%. Given the relatively high number of movies Disney puts out (more than ten a year), this consistency is adjudged quality is rather impressive.

```{r average_RT_score_top15, cache=TRUE}
g.d3
```

It's worth noting here how Rotten Tomatoes scores work. They represent the percentage of aggregated reviews that are "positive", a classification based in part on Rotten Tomatoes' editors' judgments about the scales that reviewers use. If a movie's aggregate percentage of positive reviews is 60% or greater, then the movie is classified as "Fresh" and receives an appealing red tomato icon next to its name on the Rotten Tomatoes site. If the percentage is less than 60%, then the film is classified as "Rotten" and is branded with a rude green splat icon.

One may wonder if the "Fresh" rating has an effect on the boxoffice performance of a movie, independent of the percentage score. The following plot (which computes the relationship between the two variables separately for RT scores above and below the 60% threshold) suggests there is no special advantage a film receives for having a "Fresh" rating. A film with a 59% rating is expected to have a boxoffice performance about as good as one with a 60% rating. Note that the red line indicates the threshold for obtaining a "Fresh" rating.

```{r discontinuity_RT_score_boxoffice, cache=TRUE}

ggplot() +
  geom_smooth(data = filter(boxoffice, rt.critics < 60), mapping = aes(rt.critics, dom.box.mil), method = "lm") +
  geom_smooth(data = filter(boxoffice, rt.critics >= 60), mapping = aes(rt.critics, dom.box.mil), method = "lm") +
  geom_vline(xintercept = 60, color = "red") +
  xlab("Score") +
  ylab("Boxoffice") +
  ggtitle("Discontinuity plot of Rotten Tomatoes score and domestic boxoffice\n(millions, USD)") +
  coord_cartesian(expand=F)

```

The final plot that looks at studio performance visualizes the average number of theaters on a film's release. Not surprisingly, boutique studios like Fox Searchlight and Focus Features tend to not open their movies in wide releases. Six of the "Big Seven" occupy the top six ranks - again, not a surprising finding. Interestingly, despite Lionsgate being considered one of the "Big Seven" based on the number of films released per year, the studio has a middling average number of theaters on release, around 2500. Disney holds the top spot, with more than 3500 theaters showing one of its movies on opening day, on average.

```{r average_theaters_top15, cache=TRUE}
g.d4
```

Before moving to the next stage of the analysis, let's examine the correlations between these averaged characteristics of the top 15 studios. The average boxoffice performance of a studio's films is moderately correlated with the total numbers of movies it releases and the average number of theaters its movies are shown in on release. There is almost no correlation between a studio's average Rotten Tomatoes score and the number of films it releases (my proxy for studio size). The only negative correlation is between the average Rotten Tomatoes score and the average number of theaters on release. This finding suggests that critics tend to prefer "smaller" movies (i.e., films released less widely). This interpretation is supported when we look at the correlation between critics score and number of theaters on release, for movies rather than studios: `r round(cor(boxoffice$rt.critics, boxoffice$dom.theaters.open, use = "complete.obs"), 2)`. Though weaker than the correlation when our unit of analysis is studio, the correlation between the two variables on a film-unit basis is still negative. Interestingly, the correlation between the Rotten Tomatoes *audience* score and number of theaters on release is virtually the same: `r round(cor(boxoffice$rt.audience, boxoffice$dom.theaters.open, use = "complete.obs"), 2)`. One complaint that often appears on the internet is that critics and audience members have divergent attitudes on what makes a good movie. These findings suggest this concern may be misplaced. I will have more to say on this issue later in the analysis.

```{r correlation_matrix_top15, cache=TRUE}
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

```

It is clear from the above visualizations and analysis that there are substantial differences across studios when it comes to the kinds of movies that are made and how well they perform at the boxoffice. These findings justify including a predictor based on a movie's studio in any models that seek to predict a film's boxoffice performance. However, including a variable that has a label for every studio would be overly complicated. So instead I create a new variable with just three categories based on the total number of movies a studio released between 2014 and 2018. If a studio released ten movies or less, I classify it as "minor". If the studio released more than ten but less than 46, it is classified as "mid". All other studios are classified as "major". These major studios correspond with the "Big Seven" I listed above.

### Observed patterns by studio status

This section explores the data after collapsing the studio variable into three categories or statuses - minor, mid and major - as described above. As the table below shows, movies released by major studios on average earn about 105m USD, dramatically more than those of mid-sized or minor studios. Despite only releasing `r round(boxoffice %>% filter(studio.status.threecat == "minor") %>% nrow() / length(unique(boxoffice$distributor[boxoffice$studio.status.threecat == "minor"])) / length(unique(boxoffice$year)), 2)` films per year, the typical minor film studio can expect to earn not that much less per film than a mid-sized studio, which on average releases `r boxoffice %>% filter(studio.status.threecat == "mid") %>% nrow() / length(unique(boxoffice$distributor[boxoffice$studio.status.threecat == "mid"])) / length(unique(boxoffice$year))` per year.

```{r average_boxoffice_studio_status, cache=TRUE}
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

```

## Boxplots of domestic boxoffice results by studio status

The first plot shows the distributions for all three studio categories together. The median values (visualized by the black lines inside the boxes) are not too different from each other. However, the major studio category has far more outliers (defined as at least 1.5 times greater/smaller than the 75th/25th percentile of that distribution) and a much wider range. A movie appears much more likely to be a boxoffice smash if it is released by a major studio than by a mid-size or minor one.

```{r boxplot_boxoffice_all, cache=TRUE}
ggplot(boxoffice, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() + coord_flip()
```

The next three plots explore the distribution of domestic boxoffice performance (in millions of USD) for each studio status category - minor, mid-sized and major. Each plot displays the standard boxplot values (e.g., median, lower quartile, upper quartile) and names the outliers for each category. For the major-studio plot, I only label the "extreme" outliers ((defined as at least 3 times greater/smaller than the 75th/25th percentile of that distribution) so that the text labels are legible.

The first plot shows that fifty percent of major-studio films earn between `r dat.major <- boxoffice %>% filter(studio.status.threecat == "major") %>% summarize(median = round(median(dom.box.mil), 2), IQR = round(IQR(dom.box.mil), 2)); dplyr::pull(dat.major, 1) - dplyr::pull(dat.major, 2) / 2` and `r dplyr::pull(dat.major, 1) + dplyr::pull(dat.major, 2) / 2` million USD. However, there are many outliers that earn well above 250 million USD at the boxoffice. Star Wars and Marvel films dominate the group of outliers, with The Force Awakens earning the most money with over 875 million USD. In fact, virtually all of these outlier films were released by Disney.


```{r boxplot_majors, cache=TRUE}
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
```

The second boxplot (displayed below) visualizes the distribution of domestic boxoffice performance for mid-sized studios. Fifty percent of these studios' films earned between `r dat.mid <- boxoffice %>% filter(studio.status.threecat == "mid") %>% summarize(median = round(median(dom.box.mil), 2), IQR = round(IQR(dom.box.mil), 2)); dplyr::pull(dat.mid, 1) - dplyr::pull(dat.mid, 2) / 2` and `r dplyr::pull(dat.mid, 1) + dplyr::pull(dat.mid, 2) / 2` million USD. There are some outliers, but not nearly as many as for the major studios. Bad Moms was the highest performing mid-sized studio film between 2014 and 2018, earning more than 105 million USD.

```{r boxplot_mids, cache=TRUE}
dat.mid <- boxoffice %>% filter(studio.status.threecat == "mid") %>% mutate(outlier = ifelse(is_outlier(dom.box.mil, 1.5), name, ""))

ggplot(dat.mid, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() +
  geom_text_repel(mapping = aes(label = outlier), hjust = -0.1) +
  xlab("Studio status") +
  ylab("Boxoffice") +
  ggtitle("Boxplot of domestic boxoffice performance (millions, USD) for mid-sized studios")
```

The third and final boxplot (displayed below) visualizes the distribution of domestic boxoffice performance for minor studios. Fifty percent of these films earned between `r dat.min <- boxoffice %>% filter(studio.status.threecat == "minor") %>% summarize(median = round(median(dom.box.mil), 2), IQR = round(IQR(dom.box.mil), 2)); dplyr::pull(dat.min, 1) - dplyr::pull(dat.min, 2) / 2` and `r dplyr::pull(dat.min, 1) + dplyr::pull(dat.min, 2) / 2` million USD, a fairly narrow range. Very few of these movies made more than 45 million USD, with Creed II earning the most at almost 120 million.

```{r boxplot_minors, cache=TRUE}
dat.min <- boxoffice %>% filter(studio.status.threecat == "minor") %>% mutate(outlier = ifelse(is_outlier(dom.box.mil, 1.5), name, ""))

ggplot(dat.min, aes(studio.status.threecat, dom.box.mil)) +
  geom_boxplot() +
  geom_text_repel(mapping = aes(label = outlier), hjust = -0.1) +
  xlab("Studio status") +
  ylab("Boxoffice") +
  ggtitle("Boxplot of domestic boxoffice performance (millions, USD) for minor studios")
```

Based on the interquartile ranges (i.e., the range of the middle 50% of values, centered around the median value) for these three categories, it's clear that movies from all three categories are capable of making relatively little money (i.e., less than 20 million USD). However, the three categories have substantially different upper ends of their interquartile ranges, with the difference between that of the major studios and the minor studios the most dramatic. It appears, then, that movies released by major studios are much more likely to make a lot of money than those released by mid-size or minor ones. At the same time, though, major studio films frequently make as much as films from smaller studios do.

The next three (density) plots provide an alternative way of looking at the distribution of boxoffice performance by studio category. Density is a measure of proportion for a given value along the x-axis - in this case, domestic boxoffice performance. The higher the density at a given point, the more movies earned that much money. The red lines indicate the mean domestic boxoffice performance for that studio category.

The first plot shows that the distribution for major studios is highly skewed upward, with most movies earning less than the mean but a number of very large outliers pulling the mean to the right. A similar (though less extreme) pattern is apparent for minor studios, whereas mid-sized studios have a less skewed distribution.


```{r density_plots, cache=TRUE}
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
```


Indeed, the skewness scores of these three distributions are `r tab3 <- boxoffice %>% group_by(studio.status.threecat) %>% summarize(skewness = skewness(dom.box.mil)); round(dplyr::pull(tab3, 2)[1], 2)`, `r round(dplyr::pull(tab3, 2)[2], 2)` and `r round(dplyr::pull(tab3, 2)[3], 2)` for minor, mid-sized and major studios, respectively. A non-skewed distribution should have a score close to zero, and, as a rule of thumb, no more than 1 or less than -1. All three of these studio categories exhibit substantial skewness. Since we are using domestic boxoffice performance (in millions of USD) as the outcome variable to predict, we should address this skewness since it can interfere with our analysis. Specifically, films with very high boxoffice returns can excessively influence the estimation of the coefficients we use to develop predictive models. To mitigate this skewness, I log this outcome variable when using it in any models. For visualization purposes, however, I continue to use the original version of the domestic boxoffice performance variable since it is much more easily interpretable.

## Numeric predictors (summary)

The following table displays correlations between all numeric predictors in the dataset. The names of these variables should be self-explanatory, though I should note that "(adjusted)" means that the variable takes into account only those movies that came out in the years prior to the one in which the film was released. This adjustment seems justified since the correlation between the non-adjusted and adjusted average director RT critics scores is `r cor(boxoffice$director.avg.rt, boxoffice$director.avg.rt.adj, use = "complete.obs")` and the correlation between the two versions of the lead actor RT score is `r cor(boxoffice$lead.actor.avg.rt, boxoffice$lead.actor.avg.rt.adj, use = "complete.obs")`. These correlations are high, but not perfect, suggesting some degree of divergence between adjusted and non-adjusted RT scores. In addition, considering the reception of only those movies that were released prior to the film in question makes theoretical sense, as we would not expect the reception of future movies to influence how well a movie did in the past.

```{r correlations_all_numeric, cache=TRUE}
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
```

The correlation table reveals some interesting patterns. Total boxoffice is moderately correlated (i.e., absolute value is greater than 0.30) with the RT audience score, the number of theaters on release, and maximum number of theaters. Opening boxoffice is strongly correlated with total boxoffice (i.e., absolute value is greater than 0.7). When selecting numeric variables to include in my predictive models, I only want to include those variables that have at least a moderate correlation with my outcome variable: total boxoffice. However, I also want to only include variables that can be useful predictors, that is, prior to the film's release. Of course, the RT audience score does not fit this criterion, but if we relax this rule just a bit and say that any variables whose values are determined prior to the first evening of a film's release can be useful predictors. To see if final RT scores (which the two RT variables measure) are good proxies for the reception a movie receives (by both critics and audiences) before the movie's first evening showings, I took a random sample of 21 films from my dataset and used the Internet Wayback Machine to obtain RT scores the morning of the film's release (i.e., prior to the its first evening showings) and compared those to the final RT scores obtained in February 2020. The following two tables display correlations between the main RT variables and their respective alternatives, i.e., films' scores at different snapshots in time - the day of release, 3 days after release, 14 days after release and 28 days after release. The extremely high correlations suggest that final RT scores are on average good measures of a film's pre-release or release day reception.

(NOTE: the days after release markers are approximate. Most movies did not have archived pages for every relevant day. I picked the closest day to each desired interval when possible, though some movies did not have archived pages anywhere close to the desired interval. These intervals I have left blank.)

Finally, I relax my criteria to also allow for the inclusion of the RT critics score in predictive models, even though it has a correlation of just 0.26 with total boxoffice. Since one of this project's motivating questions is how much impact Rotten Tomatoes scores have on a film's success, I think including both variables in the models would be substantively interesting.

```{r RT_scores_robustness, cache = T}
rt.sample <- read_csv("rt.sample.csv")

critics.corrs <- rt.sample %>% dplyr::select(rt.critics, rt.critics.day1, rt.critics.day3, rt.critics.day14, rt.critics.day28) %>% correlate() %>% rename(` ` = rowname) %>% mutate_at(vars(-` `), funs(round(., 4)))

critics.corrs[is.na(critics.corrs)] <- ""

critics.corrs %>% kable() %>% kable_styling(full_width = F) %>% add_header_above(c("Correlations, Rotten Tomatoes scores (critics), day of release and times after" = 6))

audience.corrs <- rt.sample %>% dplyr::select(rt.audience, rt.audience.day1, rt.audience.day3, rt.audience.day14, rt.audience.day28) %>% correlate() %>% rename(` ` = rowname) %>% mutate_at(vars(-` `), funs(round(., 4)))

audience.corrs[is.na(audience.corrs)] <- ""

audience.corrs %>% kable() %>% kable_styling(full_width = F) %>% add_header_above(c("Correlations, Rotten Tomatoes scores (audience), day of release and times after" = 6))
```

## Rotten Tomatoes scores

This section further explores patterns relating to the two variables that measure Rotten Tomatoes scores. The first plot below displays how RT scores have varied over time within my dataset's scope, i.e., the years 2014 to 2018. The average audience score has stayed largely unchanged at slightly higher than 60%, meaning that the average movie has received a "Fresh" rating from audiences. The average critics score, on the other hand, has slightly but steadily increased from a little over 50% in 2014 to just over 60% in 2018. Across all years, the average critics score is lower than the average audience score.

```{r RT_scores_year, cache=TRUE}
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
```

The second plot displays average RT scores for each studio status category: minor, mid-sized and major. Once again, the average audience score is consistently higher than the average critics score. Notably, though, films from minor studios tend to be better received than their mid-sized and major studio counterparts and the gap between audience and critics scores is narrower for this group than the other two. There seems to be little difference in average reception between mid-sized and major studio films. 

```{r RT_scores_studio_status, cache=TRUE}
tab5 <- boxoffice %>% group_by(studio.status.threecat) %>% summarize(rt.critics = mean(rt.critics), rt.audience = mean(rt.audience))

tab5 %>% ggplot() +
  geom_point(aes(studio.status.threecat, rt.critics, color = "blue"), size = 2) +
  geom_point(aes(studio.status.threecat, rt.audience, color = "green"), size = 2) +
  coord_cartesian(ylim = c(0, 75)) +
  ylab("Score") +
  xlab("Studio status") +
  ggtitle("Average Rotten Tomatoes score (critics and audience)\nby studio status") +
  scale_color_manual(name = "Score type", values = c("blue" = "blue", "green" = "green"), labels = c("Critics", "Audience"), guide = "legend")
```

The third plot looks at the interaction between RT score and studio status in greater detail. This set of density plots shows a couple of interesting patterns. First, RT audience scores seem to be more densely distributed than critics scores, with the density curves higher in the middle of the distributions for audience scores and higher at the tails for critics scores. This suggests audiences tend to be not as strongly denigrating or acclamatory as critics are. Second, all three plots are to some extent double peaked. For instance, audiences seem more likely to rate a movie a 45% or 75% score than a 55% for mid-sized studio releases (bear in mind that these percentages represent the portion of audiences/critics who have favorably reviewed a movie). This double-peakedness is more of an issue for the critics score variable because the peaks are further apart. The relative similarity in density height between the peaks for the mid-sized and major studio categories is notable as well (for audience scores, only the mid-sized category has peaks with similar heights). The spread and roughly equal height of these peaks for the critics score variable suggests that it may be a weak predictor for other variables, a possibility that is supported by the weaker correlation between the critics score and boxoffice variables. The double-peakedness is also present for the audience score variable, but appears less severe.

In addition, the notable variations in RT score distribution across the three studio categories suggests that interacting these numeric variables with the categorical studio one may be sensible when building a predictive linear model.

```{r density_plots_RT_studio_status, cache = T}
ggplot(boxoffice) + geom_density(mapping = aes(rt.critics, fill = "blue", color = "blue"), alpha = 0.5) +
  geom_density(mapping = aes(rt.audience, fill = "green", color = "green"), alpha = 0.7) +
  facet_wrap(facets = vars(studio.status.threecat), nrow = 2) +
  xlab("Rotten Tomatoes score") +
  ggtitle("Density plots of Rotten Tomatoes scores (critics and audience)\nby studio status") +
  scale_color_identity() +
  scale_fill_identity(name = "Score type", breaks = c("blue", "green"), labels = c("Critics", "Audience"), guide = "legend") +
  coord_cartesian(expand=F)
```

## Number of theaters on release

The next two plots provide visual information on the third numeric variable I use when developing predictive models for boxoffice success: the number of theaters a film is shown in on release. The first plot shows the average number of theaters on release for each year between 2014 and 2018. There is a modest, but steady increase across these years. The second plot displays the density distribution of the theaters on release variable for each studio status category. The density plots for minor and mid-sized studio films are similarly shaped, though the highest peak for the mid-sized category is further to the left on the x-axis, indicating that mid-sized studio films tend to be released more widely than minor studio ones. There are still quite a few mid-sized studio films that are released in very few theaters, however. Films in the major studio category seem to be overwhelmingly released in a large number of theaters. These patterns suggest we should test the interaction between the theaters on release variable and the studio status variable when constructing predictive models.

```{r theaters_density, cache = T}

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

```

## Month of release

This subsection explores patterns relating to which month a film is released in. The first five plots visualize summary statistics for each of the twelve months.

```{r month_plots, cache=TRUE}
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
```

The plot below on the left shows the number of movies released in each month. We can see that the top three months are July, November and December. March, June, August and October are also popular months for movie releases. These results make intuitive sense as the summer and holiday seasons are considered to be the most popular times for people to go to the movies. October is known as a month for smaller movies with potential widespread appeal to be released and March is increasingly becoming a time for major studios to release tentpole movies.

The plot on the right shows the average number of theaters on release for each month. The widest average releases occur in February, May, June and July. The latter three months make intuitive sense because that is when major blockbusters are usually released. February is rather surprising, though, but can be explained by the large number of movies that released in very large numbers of theaters (i.e., more than 3710, or the 80th percentile for the theaters on release variable). Such films include The Lego Movie and its sequel The Lego Batman Movie, Black Panther, and the Fifty Shades movies. Of the six movies that released in February in more than 3700 theaters, five were released after 2016. A disproportionate number of movies that are released in February have wide releases, especially in recent years. Conversely, a disproportionate number of films that are released in December have very small releases. This finding likely captures the tendency for studios to release Oscar-bait films very late in the year, so as to release them as close to the awards ceremonies as possible.

```{r month_plots_num_theaters, cache = T}
g.m1 + g.m5
```

The next three plots visualize the relationship between the two Rotten Tomatoes score variables and boxoffice performance and month of release. The two Rotten Tomatoes plots are roughly similar, with the most highly rated movies on average releasing in June, November and December. Since the latter two months often have a lot of Oscar-ambitious films released, it is not surprising they would have two of the highest average scores across all months. June is an interesting result, since this is right in the middle of blockbuster season, when many tentpole movies that may do well at the boxoffice but not so well with critics are released. It is possible that studios tend to release the "best" films from their annual stables of potential blockbusters earlier in the summer season. Regardless of the reasons for why these variations across months are observed, these five plots suggest that it may be beneficial to interact the month variable with at least some of the numeric variables when building predictive models.

```{r month_plots_RT_boxoffice, cache = T}
(g.m3 + g.m4) / g.m2
```

## Genre

The last set of variables I want to explore before beginning the process of building predictive models is the one that measures genres. Unlike the categorical variables studio status and month of release, genre is not mutually exclusive. Therefore, I created a series of binary genre variables, corresponding to the genres used by Rotten Tomatoes. The genre classifications for each film were also gleaned from the RT website.

Four plots are displayed below that visualize the relationships between genre and number of films, boxoffice performance, RT critics score and RT audience score. Drama is the most frequent genre category, followed by action and comedy. However, scifi, animation and family films perform the best at the boxoffice, on average. Documentaries and art/foreign films tend to be the best reviewed, by both critics and audiences. The variation in average boxoffice performance by genre suggests that considering these genre variables when building predictive models could be advantageous. While there are differences across genre in terms of RT score, these differences seem fairly minimal, with the possible exceptions of documentaries and horror films. There does not seem to be much support for spending time on determining which interactions would be valuable for model-building.

```{r, plots_genre, cache=TRUE}
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
```

# Predicting domestic boxoffice performance

Given the above data exploration, I have decided to focus on the following predictors when constructing models for predicting boxoffice performance: Rotten Tomatoes critics score, Rotten Tomatoes audience score, number of theaters on release, year, studio status, month, and genre. Before working towards a final predictive model, however, I explore the linear relationship between RT scores and domestic boxoffice. Because of the extreme skew of the domestic boxoffice variable, I first log it before including it in any models.

## Bivariate models: Rotten Tomatoes scores

The following tables show the results of the OLS regression analysis of the relationship between the RT score variables and the outcome variable (i.e., logged domestic boxoffice performance in millions of USD). The RT critics model indicates that there is a positive linear relationship between the predictor and the outcome variable. For what it's worth, both the intercept and coefficient estimate are statistically significant by conventional standards. We obtain similar results for the model estimating the relationship between the RT audience predictor and the outcome. The coefficient for the RT audience variable is almost double in magnitude compared to that of the RT critics variable. Given that the two variables are measured on the same scale (0-100%), this indicates a stronger relationship between the audience score and the outcome than between the critics score and the outcome.

```{r lm_models_RT_scores, cache=TRUE}

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
```

The following scatter plots show, however, that neither variable has an especially strong relationship with boxoffice performance. There is quite a lot of variation around the regression line in both plots, indicating that neither is a good predictor of boxoffice performance.

```{r plots_log_RT_models, cache = T}
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
```

The next two plots use the appropriate bivariate regression model to compute predicted values for the outcome variable (i.e., boxoffice performance) and then transform those predictions into the original outcome scale (unlogged USD in millions). In both plots, we can see that the RT score variables underestimate the performance of dozens of films. Once again, we see that, at least on their own, these variables do not do a good job of predicting boxoffice performance.

```{r plots_reg_RT_models, cache = T}
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
```

## Multivariate models

Rather than make a bivariate model for each predictor, I instead present below a single model that includes all predictors I have selected. A second model includes the same predictors but with interaction terms. I tried a number of different interactions between numeric variables and categorical ones, and determined that three are worth including due to their effects on the model's r.squared and AIC statistics. R.squared represents the variation in the outcome variable that is explained by the set of predictors, whereas AIC (Akaike's Information Criterion) considers both the quality of the model in fitting the data and the number of predictors. The quality of a model is considered in relation to how much information about the data-generating process is lost or gained when adding or subtracting predictors. Generally speaking, a model that has the same level of quality as another but uses fewer predictors will have a lower (and thus better) AIC score.

The three interactions I settled on are: Rotten Tomatoes critics score and studio status, theaters on release and studio status, and theaters on release and month.

```{r lm_model_full, cache=TRUE}
m8d.2.1 <- lm(dom.box.mil.log ~ rt.critics + rt.audience + dom.theaters.open + year + month +
                genreAction + genreAnimation + genreArtForeign + genreClassic +
                genreComedy + genreDocumentary + genreDrama + genreHorror + genreFamily + genreMystery +
                genreRomance + genreSciFi + studio.status.threecat, data = boxoffice)

m8d.2.1.summary <- round(broom::glance(m8d.2.1), 2)
m8d.2.1.rmse <- round(modelr::rmse(m8d.2.1, boxoffice), 4)
```

```{r lm_model_full_interactions, cache=TRUE}
## final model (m8d.2.1) with interaction terms (rt.critics*studio.status.threecat, dom.theaters.open*studio.status.threecat, dom.theaters.open*month)

m8d.2.1.i <- lm(dom.box.mil.log ~ rt.critics*studio.status.threecat + rt.audience + dom.theaters.open*studio.status.threecat + year + dom.theaters.open*month +
                genreAction + genreAnimation + genreArtForeign + genreClassic +
                genreComedy + genreDocumentary + genreDrama + genreHorror + genreFamily + genreMystery +
                genreRomance + genreSciFi, data = boxoffice)

m8d.2.1.i.summary <- round(broom::glance(m8d.2.1.i), 2)
m8d.2.1.i.rmse <- round(modelr::rmse(m8d.2.1.i, boxoffice), 4)
```

The following table presents the two OLS models side-by-side. For each term in the regression, the coefficient (standard error) is provided. Stars indicate the degrees of statistical significance at conventional standards. The coefficients are not standardized and so must be interpreted on the scales of the respective variables. The difference in coefficient size between RT critics and RT audience is even more substantial than it was when we looked at the two predictors' relationships with the outcome variable independently. In model two, the inclusion of interaction terms seems to significantly weaken the relationship between the RT critics score variable and the outcome. Conversely, the coefficient for theaters on release (dom.theaters.open) changes relatively little when including interaction terms, suggesting this predictor has a more robust relationship with the outcome than RT critics does. Interestingly, the coefficient for the month of February changes sign when we include interaction effects, probably because, as noted above, February has a disproportionate number of films with very wide releases (greater than 3700 theaters on opening day). December has a substantial and robust effect in both models, as do the action and scifi genres. The major category of the studio status variable reverses sign (from positive to negative) when including interaction terms, perhaps because what that category is really capturing is how wide of a release a film has.

```{r both_models, cache = T, results = 'asis'}
htmlreg(list(m8d.2.1, m8d.2.1.i),
          digits = 6, star.symbol = "\\*", doctype = F, center = F, caption = "")
```

When looked at holistically, the two models both do a good job of predicting domestic boxoffice performance, with the first having an r.squared of around `r m8d.2.1.summary[1]` and the second having one of around `r m8d.2.1.i.summary[1]`. The root mean squared error (a measure of model accuracy) is also superior for the second model: `r m8d.2.1.i.rmse` versus `r m8d.2.1.rmse` for the first. The AIC for the second model is also superior to that of the first: `r m8d.2.1.i.summary[8]` versus `r m8d.2.1.summary[8]`.

## Visualizing the models

An alternative way of displaying the model results is to plot the point estimates of the variable coefficients along with their 95% confidence intervals. I do this just for model one, since the interaction terms in model 2 would make such a visualization overly complicated. Instead, I use other plots further below to display the most important features of model 2. Each bar represents the coefficient point estimate for each predictor included in model 1. The black bars represent 95% confidence intervals for these estimates. If the bar crosses 0, then the point estimate is not statistically significant according to conventional standards. More importantly, though, each black bar gives us the range of possible values that we could reasonably say encompasses the true value of the variable coefficient. Some "statistically significant" coefficients - like that associated with the month of December - have a fairly wide range of plausible values. Others, like the two Rotten Tomatoes score variables, mave much narrower ranges. We can be more confident in our assessment of the true impact of these latter variables on box office performance than for others. Some coefficients, like for the genre Documentary, have a very broad range of plausible values and are only barely "not statistically significant" by conventional standards.

```{r term_plot_model1, cache = T}
m8d.2.1.dat <- broom::tidy(m8d.2.1)

m8d.2.1.dat[-1, ] %>% ggplot(aes(term, estimate, fill = term)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_got_d(option = "Lannister") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2,
                position = position_dodge(0.9)) +
  ylab("Estimated effect on box office (in millions) (logged)") +
  ggtitle("Estimated coefficients of predictors, model 1")
```

The next few plots take an alternative approach to displaying the uncertainty surrounding Model 2's coefficients, specifically those associated with one or more interaction term. The first plot visualizes the interactive relationship between studio status and number of theaters on release and domestic boxoffice performance. In particular, it shows the changes in the linear relationship between number of theaters and boxoffice when moving from one studio category to another. Each line represents point predictions for boxoffice performance given a certain number of theaters on release and a certain studio category. The bands represent the 95% confidence intervals for these predictions.

It is clear that there isn't much difference in the relationship betrween number of theaters and boxoffice performance when comparing films made by minor studios and those made by mid-sized ones. Major studio films, on the other hand, exhibit a stronger relationship between number of theaters and boxoffice, especially for films released in more than 3000 theaters. Major studio films seem to better take advantage of large-scale releases than do those released by smaller studios. One possible reason may be that major studios have the marketing budgets to encourage large numbers of people to go fill the seats in the theaters their movies are shown in, whereas smaller studios lack such resources.

```{r plot_interaction_theaters_studio, cache=TRUE}
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
```

The next two plots visualizes the relationship between the two Rotten Tomatoes score variables and studio status. It is important to note that only the first plot is based on Model 2. The second plot illustrates what happens when we interact the RT audience variable, instead of the RT critics one, on studio status. In other words, the second plot displays predictions based on a different model. I am displaying this alternative specification to demonstrate the unique relationship the RT critics variable has with studio status. Specifically, for mid-sized and minor studio films, the relationship between RT critics scores and boxoffice performance is negligible and even negative for mid-sized studio films. The overlapping confidence bands for these two categories suggest that there is no real difference between them when it comes to predicting the relationship between RT critics scores and boxoffice performance. However, major studio films show a notably different pattern. For these films, there appears to be a very strong, positive relationship between critics score and boxoffice performance. It may be that moviegoers pay more attention to Rotten Tomatoes critics scores for major studio movies when deciding whether to go see a movie than for mid-sized/minor studio films. Given the potentially more niche nature of mid-sized/minor studio films, viewers of these movies may already make up their mind as to whether to go see them and are not as interested in what the critics say. Major studio films, though, are often intended to be tentpoles that appeal to many types of people and so there is no automatic market segment interested in seeing them. If this hypothesis is true, then the boxoffice success of major studio films can be considerable, but may be substantially tempered by a lukewarm reception by the critics.

Interestingly, we do not see the same pattern when substituting an interaction between the RT audience score variable and studio status. The slopes for all three categories and positive, and while the prediction line for the major studio category is significantly steeper than those for the other two categories, the difference is not as remarkable as it is for the RT critics score-studio status interaction. Indeed, the AIC score for the model decreases when we swap this new interaction in or include both interactions. Therefore, the RT critics score-studio status interaction does more predictive heavy-lifting and is worth including in Model 2.

```{r plot_interactions_RT_studio, cache=T}
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
```

The final plot visualizes the interaction between number of theaters on release and month when predicting boxoffice performance. Most of the monthly prediction lines are bunched fairly close to each other, with the major exception of December. In the final month of the year, films that are released very widely (i.e., more than 3700 theaters) seem to do much better than films with similar releases in other months. Given that this is a time of year when many people have ample holiday/vacation time, and most beaches are closed, it is not surprising that big movie releases do very well during December.

```{r plot_interactions_theaters_month, cache = T}
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
```

## Robustness tests

This section presents the results of a number of checks on the robustness of the models I have so far created. The following two plots display the relationship between the predicted values of the outcomes and their residuals, that is, the difference between the actual outcome for a given observation (i.e, the actual boxoffice performance of a given film) and its predicted outcome. Both plots are for Model 2 only, since this is the one I use for prediction purposes given its higher r.squared and lower AIC. The first plot displays this relationship directly, with residuals plotted on the y-axis and predicted values on the x-axis. Ideally, the residuals should be evenly distributed on either side of the zero line and should not display any noticeable pattern. Both criteria seem to be met by this plot, though there appears to be a noticeable line demarcating the bottom-left quadrant of the plot. This may be because I excluded films that made under ten million USD at the boxoffice since I did not want very small films to bias my analysis.

The second plot displays the same relationship, but rotated such that the y-axis now represents the actual values for the outcome variable. Again, there should be balance on either side of the red line and no clear patterns should be present, both of which we observe.

```{r robust_plots_lm_model_full_interactions, cache = TRUE}
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
```

```{r robustness tests, cache=TRUE}
outliers <- ols_test_outlier(m8d.2.1.i)

mod2.cor.test <- ols_test_correlation(m8d.2.1.i)
```

Next, I perform two robustness tests designed for OLS regression models: the Bonferroni Outlier Test and the Correlation Test for Normality. The outlier test indicates that a single observation is a significant enough outlier to have an outsized influence on the model: `r boxoffice[rownames(outliers), ]$name`. This film was released in just `r boxoffice[rownames(outliers), ]$dom.theaters.open` theaters on release but ended up making `r boxoffice[rownames(outliers), ]$dom.box.mil` million USD at the boxoffice. To be technical, this observation has a Bonferroni p-value of `r outliers[, 3]`. I am not concerned about a single outlier invalidating the model and I would never want to discount any professional duet between `boxoffice[rownames(outliers), ]$director` and `boxoffice[rownames(outliers)]$lead.actor`.

The correlation test for normality obtains the correlation between the model's residuals and the expected values of the residuals under an ideal normal distribution. Since OLS regression assumes that residuals are normally distributed, this is an appropriate test for determining if this criterion is met. the reported correlation is `r mod2.cor.test`, indicating the model's residuals are very nearly normally distributed.

Finally, we can measure variance inflation factors (VIFs) to identiy any collinearities between predictors that may bias their coefficients. In general, VIFs should not be above four and certainly not above ten. The following table displays the VIFs for model 2. We can see that the VIFs for many of the variables are very high. However, these are due to the fact that we have interaction terms in the model, which purposefully introduce collinearity into the model. Instead, a better approach would be to examine the VIFs for model 1, which is identical to model 2 except without the interaction terms.

```{r VIF_model2, cache = T}
m2.vif <- ols_vif_tol(m8d.2.1.i) %>% as.tibble()

m2.vif %>% format_table() %>%
kable_styling(full_width = F) %>% add_header_above(c("Robustness measures for model 2" = 3)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(m2.vif), color = c("#474747"), background = c("#E0EEEE"))
```

We can see in this table that the VIFs are all lower than four and some are quite close to one, which would include zero collinearity. We can conclude that collinearity between predictors is not biasing our coefficient estimates.

```{r VIF_model1, cache = T}
m1.vif <- ols_vif_tol(m8d.2.1) %>% as.tibble()

m1.vif %>% format_table() %>%
kable_styling(full_width = F) %>% add_header_above(c("Robustness measures for model 1" = 3)) %>%
  column_spec(1, "10em") %>% row_spec(1:nrow(m1.vif), color = c("#474747"), background = c("#E0EEEE"))
```

## Stepwise model

My final robustness check is to run a stepwise forward regression procedure to see which predictors should be included in a model according to standard criteria. Stepwise forward regression adds predictors to a model based on how much added accuracy they provide, given the existing set of predictors. The procedure starts by adding a single predictor that contributes the most to the model's accuracy in predicting the outcome, the adds a second one from the remaining pool of candidate predictors, and so forth until all predictors have been added. I use Mallow's Cp to decide which predictors to include in this version of the model. Mallow's Cp measures the contribution a particular predictor provides to a model, while attempting to avoid overfitting (i.e., adding so many predictors to a model, based on a particular sample of data, such that the model poorly predicts data from outside the sample). Mallow's Cp is thus similar to AIC. In principle, one can use P (the number of predictors included in the model up to that point in the stepwise procedure) as a threshold for inclusion in the final model, such that Mallow's Cp should be higher than P. A more conservative approach would be to use 2 times P as the threshold. Either way, the same set of predictors is recommended by stepwise forward regression and Mallow's Cp.

The table below highlights these predictors in green. Notably, not a single genre predictor is deemed worthy to be included in the model. In addition, the RT audience score variable contributes much more to the model's accuracy than does its critics counterpart. This may be due to the peculiar interaction the RT critics score has with studio status, as discussed above. We also see that the r.squared of the model flattens out after including the RT critics variable, increasing by less than 0.02 after including the remaining displayed variables. Recall that the r.squared of Model 2 is `r m8d.2.1.i.summary[1]`, only a bit higher than the r.squared for the stepwise model. For prediction purposes, Model 2 is superior. But the stepwise version is more parsimonious and may test better on new data as a result.


```{r stepwise_model, cache = T}
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
```

## xgboost model

The fourth and final model I construct is quite different in design than the first three. This model is built using the XGBoost (eXtreme Gradient Boosting) machine learning algorithm. XGBoost uses a series of decision trees to arrive at a model that best predicts the outcome variable (here, domestic boxoffice performance) while minimizing overfitting. In theory, a model that is not overfitted should not only be effective at explaining the data used to create it (the training data) but also good at predicting the outcome given new data (the test data). A variety of hyperparameters are defined when constructing an XGBoost model, which I present in the table below:



```{r xgboost_model_setup, cache = T}
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
```

The seed represents the value utilized by the random number generator whenever any arbitrary choices are made by the machine learning algorithm. Gamma (the tree complexity parameter) sets the threshold that the reduction in error the leaf node of a tree must meet in order to be kept in the tree model. Eta (the learning rate) represents the weight given to each new tree added by the algorithm and can be set lower to reduce the risk of overfitting. The lower eta is, the more cautious the algorithm is but also the longer the algorithm will take to arrive at the optimal model given the training data. Max depth is the maximum number of nodes permitted in each tree. The smaller the max depth, the less likely overfitting becomes. Subsample is the ratio of the training data that is sampled for growing trees. Lambda (a regularization paramter) represents the penalty added to the calculation of an individual leaf node's contribution to the tree's predictive accuracy and is designed to reduce the impact of individual observations on this calculation. In other words, gamma helps ensure that leaf nodes that distinguish fewer observations are penalized more than those that distinguish more observations. Nrounds is the total number of trees that are grown by the algorithm. When eta is low, nrounds should be high to give the algorithm time to converge on a final model.

In other words, my approach to building this XGBoost model is to allow for less pruning for each tree (low gamma) but reduced sensitivity to individual observations (high lambda) combined with many trees (high nrounds) incrementally combined (low eta). I tried other variations (not shown here) and determined that this approach results in the best predictions and similar average errors for the training and test sets.

(NOTE: I use the xgboost package for R and all other hyperparameters for the xgboost function are left at their defaults)

```{r xgboost_model_compute, cache = T}

xgb_penalty2 <- xgboost(data = predictors2[-test_idx, ],
                       label = label[-test_idx],
                       params = list(gamma = gamma, eta = eta, max_depth = max_depth, subsample = subsample, lambda = lambda),
                       objective = "reg:squarederror", nrounds = nrounds, verbose = 0)
pred_penalty2 <- predict(xgb_penalty2, predictors2[test_idx, ])

errors <- rbind(xgb_penalty2$evaluation_log)

```

The XGBoost model I construct includes all the variables from Model 1: the two Rotten Tomatoes score variables, month, genre, year, number of theaters on release and studio status category (minor, mid-sized and major). The root mean squared error (RMSE) for the training data is `r  xgb_penalty2$evaluation_log[nrounds, 2]`, while the RMSE for the test data is `r mltools::rmse(preds = pred_penalty2, actuals = label[test_idx])`. The RMSEs are pretty close to each other, which is desirable.

The following plot shoes the importance of each "feature" or variable included in the XGBoost model. Importance is essentially a measure of how often a variable is used by a leaf node, that is, to make decisions about how to minimize the gap between an observation's actual score for the outcome and its predicted score. We can see that the number of theaters on release is far more important than any other variable. Both Rotten Tomatoes score variables are next in importance, with the audience variable the more important of the two. Of the three studio status categories, the minor and major categories are substantially more important than the mid-sized one. Finally, December, October and September are the most important months when it comes to predicting boxoffice performance. No genre seems to play a major role nor does the year variable. These findings more or less match those we obtained from the OLS models presented above.

```{r xgboost_importance_plot, cache = T}
xgb_penalty2.imp <- xgb.importance(model = xgb_penalty2)
xgb.ggplot.importance(xgb_penalty2.imp, legend = F) + theme(legend.position = "none")
```

```{r xgb_fit_metrics, cache = T}
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
```

The above plot demonstrates that the residuals of the XGBoost model's predictions are fairly evenly spread out across the zero line, as we would hope. There does seem to be some bias in favor of above the line for values on the lower and upper ends of the x-axis, but this pattern seems fairly weak. No other patterns are apparent.

## Predictive performance

Now that we have several models to use for predicting boxoffice performance, let's compare them directly to see which is best at prediction. I use a folding method that divides the data into five groups (or folds) and trains each model on four of the five folds and then tests each model on the fifth fold This process is repeated five times such that each fold is used once for testing purposes. I do not include Model 1, since Model 2 is identical except for the inclusion of three interaction effects. The table below displays the results for each model, indicating average errors (specifically, root mean squared error) and r.squared statistics, as well as minimums and maximums for each.

```{r folds_model2, cache = T}

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

```

```{r folds_step_model, cache = T}

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
```

```{r xgb_folds, cache = T}

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
```

```{r folds_table, cache = T}

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
```

We can see that the XGBoost model performs the best across all seven measures. Model 2 performs second best across most, though there seems to be a somewhat wider range for RMSE compared to the stepwise model. This may be an artifact of the way in which observations were allocated to folds, however.

# Conclusion

The above analysis makes clear that the number of theaters on release and the receptivity of critics and audience play the greatest role in determining the success of a movie at the boxoffice. Of course, other variables not included in the dataset may also have substantial effects on the outcome of interest. Still, all three models assessed above performed quite well in predicting boxoffice success. The values for a number of the variables included in these models are determined well in advance of a film's debut. The number of theaters on release must be negotiated by studios and theater companies prior to the film's release. The month of the release is often decided years before. All of these factors are dictated by the studio producing and releasing the film, which is perhaps the factor determined furthest in advance. The fact that these variables all have meaningful impacts on a film's future success indicates that it is possible to predict that such success well in advance with reasonable accuracy. Admittedly, the Rotten Tomatoes scores a film receives are determined after its release. Still, I have demonstrated that these scores are highly stable between the day of release and later time frames. If we wait until early on opening day to make our prognostications, then the inclusion of the RT variables in our model would make our predictions that much more accurate.

All of that said, the results also show that there are still probably many idiosyncratic factors that play a role in boxoffice performance. For instance, sequels may perform better than originals, though only for certain movie series. While the Rotten Tomatoes critics average for director and lead actor seemed to have very weak relationships with boxoffice performance, perhaps certain actors and directors (like Will Smith or Christopher Nolan) have much greater attractive influence than others.

Future research could analyze the impact of IMDB ratings, expand the number of years included in the dataset and look at variations in variable relationships across time and look for underlying constructs that capture distinctive categories of films.
