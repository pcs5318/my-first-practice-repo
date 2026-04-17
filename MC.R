# MC Numeric Integration ----
## Use Monte Carlo Numeric Integration to Investigate the definite integral
## that I've been assigned.

# Step 1: Load Packages ----
# install.packages('patchwork')
library(tidyverse) # For wrangling and plotting
library(patchwork) # To combine different plots into one

# Step 2: Load my MC Function ----
## I will make use of my function to randomly generate the points
source('~/Documents/GitHub/Stat184/homework_solutions/MC_GenPoints.R')

# Step 3: Define PSU Palette ----
psuPalette <- c('#1E407C', '#BC204B', '#3EA39E', '#E98300',
                '#999999', '#AC8DCE', '#F2665E', '#99CC00')

# Step 4: Generate Points ----
## This example will use the beta distribution
set.seed(184) # Makes the results more reproducible
myPoints <- generatePoints(
  xbounds = c(0, 1),
  ybounds = c(0, 1.5),
  n = 10000
)

# Step 5: Indicate whether the point is above OR on/below the function ----
myPoints <- myPoints |>
  mutate(
    flag = case_when(
      y > dbeta(x = x, shape1 = 2, shape2 = 2) ~ 'above',
      .default = 'on/below'
    ),
    # Adept Only Wrangling
    numFlag = case_when(
      y > dbeta(x = x, shape1 = 2, shape2 = 2) ~ 0,
      .default = 1
    ),
    case = row_number(),
    prop = cumsum(numFlag) / case,
    numInt = prop*(1 - 0)*(1.5 - 0) # prop * x-width * y-height
  )

# Step 6: Make Plot ----
## Use a wrangling step so that we can re-use the code for each of the four
## plots
plot10000 <- myPoints |>
  # slice_head(n = 1000) |>
  ggplot(
    mapping = aes(x = x , y = y, shape = flag, color = flag)
  ) +
  stat_function(
    fun = dbeta,
    args = list(shape1 = 2, shape2 = 2),
    xlim = c(0, 1),
    color = 'black'
  ) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1), expand = expansion()) +
  scale_y_continuous(limits = c(0, 1.5), expand = expansion()) +
  scale_color_manual(values = psuPalette) +
  labs(
    title = 'Monte Carlo Numeric Integration Simulation',
    subtitle = 'Resolution n = 10000',
    # Adept Only
    caption = paste(
      'Est. Numeric Integration:',
      round(myPoints$numInt[10000], digits = 4)
    )
  ) +
  theme_bw() +
  theme(legend.position = 'none')

# Step 6: Make the Small Multiple
plot10 + plot100 + plot1000 + plot10000

# Step 7: Calculate Estimated Integration for each resolution ----
## Highly Developed and Lower Approach
flaggedPoints <- myPoints |>
  slice_head(n = 10000) |> # Iterate for each resolution
  group_by(flag) |>
  summarize(
    count = n()
  ) |>
  mutate(
    area = (count / 10000) * (1 - 0) * (1.5 - 0)
  )