# Set directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
code_dir <- getwd()
fig_dir <- paste0(dirname(getwd()), "/figures")
library(tikzDevice)
options(tikzLatexPackages =
          c("\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage,psfixbb]{preview}\n",
            "\\usepackage{amsmath}",
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n",
            "\\usepackage{fourier}\n",
            "\\DeclareMathAlphabet{\\mathdis}{OT1}{pag}{m}{n}\n"
          )
)
setTikzDefaults(overwrite = FALSE)
library(lubridate)
library(xts)
library(tidyverse)
library(ggplot2)
library(patchwork)

theme_set(theme_bw() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()))
dwidth <- 4
dheight <- 4


n <- 100     # sample size

set.seed(101)
t.in <- rbeta(n, 2.3,1)*30 - 15  # increasing entry in (-15, 15)
hist(t.in)

A <- 0       # left calendar time
B <- 10      # right calendar time

# simulate Exponential life times
lambda <- 0.3
set.seed(1012)
life <- rexp(n, rate=lambda)
range(life)

hist(life, prob=TRUE)
1/mean(life)


t.out <- t.in + life   #  exit time
age.A <- A - t.in      #  age at time A
age.B <- B - t.in      #  age at time B

setwd(fig_dir)
tikz("lexis_selection.tex", width = 6, height = 3, standAlone = TRUE)
# windows(18, 7)
par(mar=c(4,4,0.3,0.3), pty="s",
    cex=1.1, bty = "l", mfrow = c(1,2), lwd = 1.2)
# Panel 1

plot(NULL, type="n",
     xlim=c(-5, 15),
     ylim=c(0, 19),
     xlab="calendar time",
     ylab="lifetime",
     yaxs="i",
     bty = "l",
     xaxt = "n",
     yaxt = "n"
     )
mtext(text = "full population",side = 3, adj = 0.5, line = -1, font.main=1, cex.main=0.9)
sub <- t.out > A & t.out < B

segments(t.in, rep(0, n), t.out, life, col=ifelse(t.out > A & t.out < B, "black","gray"))
segments(x0 = A, x1 = A, y0 = 0, y1 = 15, lty=2)
segments(x0 = B, x1 = B, y0 = 0, y1 = 15, lty=2)
axis(1, at=c(A,B), label=c("$c_1$", "$c_2$"), cex=.4, mgp=c(3, .5, 0))
axis(2, at=c(0,5,10,15), label=c(110,111,112,113), cex=.4, mgp=c(3, .5, 0))
plot(NULL, type="n",
     xlim=c(-5, 15),
     ylim=c(0, 19),
     xlab="calendar time",
     ylab="lifetime",
     yaxs="i",
     bty = "l",
     xaxt = "n",
     yaxt = "n"
)
mtext(text = "interval truncated data",side = 3, adj = 0.5, line = -1, font.main=1, cex.main=0.9)
segments(t.in[sub], rep(0, n)[sub], t.out[sub], life[sub])
segments(x0 = A, x1 = A, y0 = 0, y1 = 15, lty=2)
segments(x0 = B, x1 = B, y0 = 0, y1 = 15, lty=2)
axis(1, at=c(A,B), label=c("$c_1$", "$c_2$"), cex=.4, mgp=c(3, .5, 0))
axis(2, at=c(0,5,10,15), label=c(110,111,112,113), cex=.4, mgp=c(3, .5, 0))
dev.off()
setwd(code_dir)

# --------  LT & RT  ------------- #
in.sample <- (t.out >= A) & (t.out <= B)
sum(in.sample)

data1 <- data.frame(t.in = t.in,
                    # age.in=age.in,
                    t.out= t.out,
                    # age.out=age.out,
                    life=life) [in.sample,]
dim(data1)

# hist(data1$age.out, prob=T)
# 1/mean(data1$life)

# Panel 2
# plot(1,1, type="n", xlim=c(-5, 17), ylim=c(0, 10), xlab="Calendar time", ylab="Age")
# title(main="Sample, lifetimes truncated by c1 and c2", font.main=1, cex.main=0.9)
# segments(data1$t.in, rep(0, nrow(data1)), data1$t.out, data1$life, col="gray")
# abline(v=c(A,B), lty=2)
# axis(3, at=c(A,B), label=c("c1", "c2"), cex=.4, mgp=c(3, .5, 0))


# Panel 3
g1 <- ggplot(
  data = tibble(x = qexp(ppoints(nrow(data1)), rate=lambda),
                y = sort(data1$life)),
  mapping = aes(x = x, y = y)
) +
  geom_abline(slope = 1, intercept = 0, col = "grey") +
  geom_point(alpha = 0.8) +
  scale_x_continuous(
    name = "theoretical quantiles",
    breaks = seq(0L, 6L, by = 2L),
    limits = c(0,7),
    expand = c(0,0)) +
  scale_y_continuous(
    name = "sample quantiles",
    breaks = seq(0L, 6L, by = 2L),
    limits = c(0,7),expand = c(0,0)
  )

data(idl, package = "longevity")
french <- idl %>% filter(country == "FR" & ddate >= ymd("1987-01-01"))
ymax_fr <- with(french, apply.yearly(xts(x = ndays, order.by = bdate), FUN = max))
fr_tb <- tibble(
  country = factor("France"),
  age = as.numeric(ymax_fr)/365.25,
  year = lubridate::year(time(ymax_fr))
)

library(dplyr)
library(lubridate)

data(englandwales, package = "longevity")
englandwales <- englandwales %>% filter(ymd(ddate) >= dmy("01-01-2000") & ymd(ddate) <= dmy("31-12-2015"))
ymax_ew <- with(englandwales, apply.yearly(xts(x = ndays, order.by = bdate), FUN = max))
ew_tb <- tibble(
  country = factor("England and Wales"),
  age = as.numeric(ymax_ew)/365.25,
  year = lubridate::year(time(ymax_ew))
)
tb <- bind_rows(fr_tb, ew_tb)

g2 <- ggplot(data = tb, mapping = aes(y = age, x = year, col = country)) +
  scale_color_manual(values=c("black","grey")) +
  geom_point() +
  geom_abline(slope = -1, intercept = 2018) +
  geom_abline(slope = -1, intercept = 1986) +
  geom_abline(slope = -1, intercept = 2015, col = "grey") +
  geom_abline(slope = -1, intercept = 1999, col = "grey") +
  theme(legend.position = "top")


# Set directories
setwd(fig_dir)
tikz("figures/truncation_artefacts.tex",
     width = 8,
     height = 4,
     standAlone = TRUE)
g1 + g2
dev.off()
setwd(code_dir)
