
library(tidyverse)  # data wrangling and plotting

powerggplot = function(x) {
  data.frame(summary(x)) %>%
    ggplot(aes(y = mean, x = nlevels)) +
    # Draw 80% threshold
    geom_hline(yintercept = 0.8, color = 'gray70', lty = 2) +
    geom_line(color = 'black') + geom_point(color = 'black') +
    geom_errorbar(ymin = data.frame(summary(x))$lower, 
                  ymax = data.frame(summary(x))$upper, 
                  width = 50, color = 'black') +
    scale_x_continuous(name = 'Number of participants', breaks = x$nlevels) +
    scale_y_continuous(name = 'Power', limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1),
                       labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    theme_classic() + theme(axis.title = element_text(size = 16), 
                            axis.text = element_text(size = 14),
                            plot.title = element_text(size = 17, hjust = .5)) +
    ggtitle(x$text %>%
              # In title, leave only name of predictor
              str_remove_all("Power for predictor |'") %>%
              # Introduce space in interactions
              str_replace(':', ' : '))
}
