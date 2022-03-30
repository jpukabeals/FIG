## To do for manuscript corrections

# Start-to do raw text
# Jesse - Could you please edit the regression equation in the top panel of
# Figure 3 (the yield panel) so that the R2 is a lower-case r2? Jesse - Could
# you please calculate the percent difference of 1) the average forage yield
# from the anthesis and dough cuts vs. 2) forage yield at the boot cut using the
# values in Figure 2? Please see my comment to you about this in both the
# response to reviewers document and the revised MS for additional details and
# context.
# End - to do raw text


# 1. import data ----------------------------------------------------------
# run code from fig_working_compiled.RMD - "run all code above", then start this file


# 2. change figure 3 yield panel R2 to t2 ------------------------------------

gdd.yield <- dat1.y1.df %>%
  ggplot(aes(y = yield.1cut, x = gdd.1cut, color = year)) +
  geom_point(aes(shape = year)) +
  geom_smooth(
    method = "lm",
    se = F,
    show.legend = F,
    color = "black",
    formula = y ~ x + I(x ^ 2)
  ) +
  annotate(
    "text",
    x = 1500,
    y = 9.5 * 1000,
    label = expression(
      paste("Marginal", ~ italic(r) ^ 2),
      ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
        ~  ~  ~  ~  ~  ~  ~  ~ " = 0.35"
    )
  ) +
  annotate(
    "text",
    x = 1540,
    y = 8.5 * 1000,
    label = expression(
      paste(italic("y = 14.5x - 0.005"), italic(x) ^ 2),
      ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
        ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~ italic("- 6673")
    )
  ) +
  scale_color_manual(values = grey.colors(n = 3,
                                          start = 0.2,
                                          end = .8)) +
  theme_bw() +
  labs(y = expression("Forage yield" ~ (kg ~ ha ^ {
    -1
  })),
  x = "") +
  theme(
    legend.position = c(.05, .96),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )
gdd.yield

gdd.rfq <- ggplot(dat1.q1.df,
                  aes(y = RFQ.1cut, x = gdd.1cut, color = year)) +
  geom_point(aes(shape = year)) +
  geom_smooth(
    method = "lm",
    se = F,
    show.legend = F,
    formula = y ~ x + I(x ^ 2),
    color = "black"
  ) +
  annotate(
    "text",
    x = 1500,
    y = 145,
    label = expression(
      paste("Marginal", ~ italic(r) ^ 2),
      ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
        ~  ~  ~  ~  ~  ~  ~  ~ " = 0.65"
    )
  ) +
  annotate(
    "text",
    x = 1507,
    y = 135,
    label = expression(
      paste(italic("y = -0.18x + 0.000044"), italic(x) ^ 2),
      ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
        ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
        ~  ~  ~  ~  ~ italic("  + 219")
    )
  ) +
  scale_color_manual(values = grey.colors(n = 3,
                                          start = 0.2,
                                          end = .8)) +
  theme_bw() +
  labs(y = "Relative feed value",
       x = expression(paste("Growing degree days (", degree, "C d)"))) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
gdd.rfq


ggarrange(gdd.yield,
          gdd.rfq,
          ncol = 1,
          nrow = 2,
          align = "v")
# ggsave("gdd.png")
ggsave(
  "fig3.png",
  width = 5.06,
  height = 5.64,
  units = "in",
  dpi = 500
)


# 3. calculate % difference in forage yields ------------------------------
#
# Forage yield for the first harvest was similar for the anthesis and dough
# stage, and both were XX % was higher for IWG harvested at anthesis or dough
# stage than when harvested at boot (Fig 2). 

# calculate percent differences
# calculating yield
# calculating between timing treatments across intensity
# selecting dat.y1.df
# looking at yield.1cut
library(tidyverse)
dat1.y1.df %>% 
  group_by(timing.1cut) %>% 
  summarise(yield.mean = mean(yield.1cut))

# boot to anthesis change
(3548-2285)/2285
# forage yield increased by 55.27% from boot to anthesis

# boot to dough change
(3661-2285)/2285
# forage yield increased by 60.22% from boot to dough
