
library(readr)
df <- read_csv("example_data/2/signal.asc", col_names = F
                )

names(df) <- c("w", "int")

plot(df)
library(tidyverse)

library(Peaks)
library.dynam('Peaks', 'Peaks', lib.loc=NULL)   # see http://r.789695.n4.nabble.com/Problem-with-quot-Peaks-quot-package-followup-td4674949.html
p <- SpectrumSearch(df$int  %>% as.numeric, 
                    background = T, window = 5, 
                    sigma = 3, threshold = 10)

peak_w <- df[p$pos, "w"] %>% unlist
peak_int <- df[p$pos, "int"] %>% unlist

plot_peaks <- tibble(
  peak_w = peak_w, 
  peak_int = peak_int
)

plot(df)
#plot(sin(seq(1,10,.1)),type='l')
points(x = peak_w, y = peak_int, col = "red")

ggplot(df) +
  geom_point(aes(w, int)) +
  geom_point(data = plot_peaks, aes(peak_w, peak_int), 
             color = "red", size = 2)

############
ggplotRegression <- function (fit) {
#https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/  
require(ggplot2)

ggplot(fit$model, aes_string(x = names(fit$model)[2], 
                             y = names(fit$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))
}
#########################


df_new <- tibble(
  lambda = peak_w,
  rec_lambda = 1/ peak_w
  ) %>%
  arrange(-lambda) %>%
  mutate(peak_m = row_number())

mod <- lm(rec_lambda ~ peak_m, data = df_new)

ggplotRegression(mod) +
  #ylim(0, max(mod$model$rec_lambda)) +
  scale_y_continuous( sec.axis = sec_axis(~1/., name = "lambda"))

summary(mod)

slope <- mod$coefficients[2]
slope_sigma <- summary(mod)$coefficients[2, 2]
ind_refr <- 1.63
d <- 1/(2 * ind_refr * slope)
d / 1000 # now in um

#https://en.wikipedia.org/wiki/Propagation_of_uncertainty
#f = aA
# d = 1/(2 n slope)
d_sigma = slope_sigma / (2 * ind_refr * slope^2)
d_sigma

cat("Thickness is: (", d, " +/- ", d_sigma, ") nm\n", sep = "")


