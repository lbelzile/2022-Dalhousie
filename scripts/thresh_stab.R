library(longevity)
library(lubridate) # data manipulation
library(ggplot2)   # grammar of graphics
library(patchwork) # combine plots
library(tidybayes) # plots
library(tidyverse) # tidy data
library(tikzDevice)
options(tikzLatexPackages =
          c("\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage,psfixbb]{preview}\n",
            "\\usepackage{amsmath}",
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n",
            "\\usepackage{fourier}\n"
          )
)

load("/home/lbelzile/Documents/Dropbox/Papers/ARSIA_Longevity/scripts/dutch_qqplot.RData")
tstab_dutch <- tstab_cp$g2


data(dutch, package = "longevity")
yr_samp <- year(attr(x = dutch, which = "sampling_frame"))
dutch1 <- dutch %>%
  subset(!is.na(ndays)) %>%
  # Remove interval censored data
  mutate(
    time = ndays / 365.25,
    time2 = time,
    ltrunc = ltrunc / 365.25,
    rtrunc = rtrunc / 365.25,
    event = 1
  ) %>%
  subset(time > 98) %>%
  select(time, time2, ltrunc, rtrunc, event, gender, byear)
# Subset all interval-censored interval-truncated records
dutch2 <- dutch %>%
  subset(is.na(ndays)) %>%
  mutate(
    time2 = ceiling_date(dmy(paste("01-", dmonth, "-", dyear)), unit = "month") - 1 -
      dmy(paste("01-01-", byear)),
    time = dmy(paste("01-", dmonth, "-", dyear)) - dmy(paste("31-12-", byear)),
    ltrunc = dmy(paste("01-01-1986")) - dmy(paste("31-12-", byear)),
    rtrunc = dmy(paste("31-12-2015")) - dmy(paste("01-01-", byear))
  ) %>%
  select(time, time2, ltrunc, rtrunc, gender, byear) %>%
  mutate(
    time = as.numeric(time) / 365.25,
    time2 = as.numeric(time2) / 365.25,
    ltrunc = as.numeric(ltrunc) / 365.25,
    rtrunc = as.numeric(rtrunc) / 365.25,
    event = 3
  ) %>%
  subset(time > 98)
# Combine databases
dutch_data <- rbind(dutch1, dutch2)

tstab_dutch_low <- with(dutch_data,
                tstab(time = time,
                      time2 = time2,
                      event = event,
                      type = "interval",
                      thresh = 95:97,
                      ltrunc = ltrunc,
                      rtrunc = rtrunc,
                      family = "gp",
                      method = "wald",
                      which.plot = "shape",
                      plot = FALSE)
)
# Bundle these estimates
tstab_dutch$data <- rbind(with(tstab_dutch_low, cbind(thresh, shape)), tstab_dutch$data)


load("/home/lbelzile/Documents/Dropbox/Rpackage/longevity/data/italian.rda")
tstab_ital <- with(italian, tstab(time = ndays/365.25,
                                     ltrunc = ltrunc/365.25,
                                     event = event,
                                     type = "right",
                                     method = "profile",
                                     thresh = 105:110,
                                     family = "gp"))

load("/home/lbelzile/Documents/Dropbox/Rpackage/longevity/data/idl.rda")
France <- idl %>% filter(country == "FR")
tstab_fran <- with(France, tstab(time = ndays/365.25,
                                             ltrunc = cbind(ltrunc1, ltrunc2)/365.25,
                                             rtrunc = cbind(rtrunc1, rtrunc2)/365.25,
                                             thresh = 105:110,
                                             family = "gp", method = "profile"))

df <- as.data.frame(rbind(
            #tstab_dutch$data,
            cbind(thresh = tstab_fran$thresh,
                  tstab_fran$shape),
            cbind(thresh = tstab_ital$thresh,
                  tstab_ital$shape)))
df$country <- c(#rep("Nederland", nrow(tstab_dutch$data)),
                rep("France", nrow(tstab_fran$shape)),
                rep("Italia", nrow(tstab_ital$shape)))

library(patchwork)
library(ggplot2)
tikz("threshold_stability2.tex", width = 6, height = 3, standAlone = TRUE)
g1 <- ggplot(df, aes(x = thresh, y = estimate, col = country)) +
  geom_hline( alpha = 0.1, yintercept = 0) +
  geom_pointinterval(aes(ymin = lower, ymax = upper), position= position_dodge(width = 0.5),
                     size = 5, point_size = 1) +
 theme_bw() +
              theme(legend.position = "bottom",
                    axis.line = element_line(colour = "black"),
                    panel.border = element_blank(),
                    panel.background = element_blank()) +
  scale_y_continuous(
    breaks = seq(-0.5,0.5, length.out = 11),
    limits = c(-0.5,0.5),
    labels = paste0("$",sprintf(-5:5/10, fmt = "%.1f"),"$")) +
  scale_x_continuous(
    breaks = seq(105,110, by = 2L),
    minor_breaks = seq(99L, 107, by = 2L),
    limits = c(104.5,110.5), expand = c(0,0)) +
  theme(panel.grid = element_blank(),
        legend.position = c(0.2,0.8)) +
  xlab("threshold (in years)") +
  ylab("shape parameter")
g2 <-  ggplot(tstab_dutch$data,
              aes(x = thresh, y = estimate)) +
  geom_hline( alpha = 0.1, yintercept = 0) +
  geom_pointinterval(aes(ymin = lower, ymax = upper), position= position_dodge(width = 0.5),
                     size = 5, point_size = 1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(
    breaks = seq(-0.5,0.5, length.out = 11),
    limits = c(-0.5,0.5),
    labels = paste0("$",sprintf(-5:5/10, fmt = "%.1f"),"$")) +
  scale_x_continuous(
    breaks = seq(95,108, by = 2L),
    minor_breaks = seq(99L, 107, by = 2L),
    limits = c(94.5,109.5), expand = c(0,0)) +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.6)) +
  xlab("threshold (in years)") +
  ylab("shape parameter")
g1 + g2
dev.off()
