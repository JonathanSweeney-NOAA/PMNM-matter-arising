#' Reproduce CPUE time series from Lynham et al 2020.
#'


library(devtools) # we want the development version of DBI for a few functions
library(tidyverse) # for data manipulations
library(lubridate)
library(feather)
library(gtable)
library(gridExtra)



# Load, transform, and merge catch data and gear config data
catch <- read_feather("data/catch_obs_dat.ftr") %>%
  # Filter for commercially landed species
  filter(ENGLISH_NAME %in% c("BIGEYE TUNA",
                             "SWORDFISH",
                             "YELLOWFIN TUNA",
                             "MAHIMAHI",
                             "OPAH (MOONFISH)",
                             "INDO_PACIFIC BLUE MARLIN",
                             "STRIPED MARLIN",
                             "SICKLE POMFRET",
                             "WAHOO",
                             "ESCOLAR",
                             "SKIPJACK TUNA",
                             "ALBACORE",
                             "SHORTBILL SPEARFISH")) %>%
  filter(KEPT_RETURN_CODE_VAL == "Kept") %>%
  mutate(count = 1) %>%
  group_by(T_TRIP_NUM, ENGLISH_NAME) %>%
  summarize(count = sum(count, na.rm = T)) %>%
  pivot_wider(names_from = ENGLISH_NAME, values_from = count)


gear <- read_feather("data/gear_obs_dat.ftr") %>%
  group_by(T_TRIP_NUM) %>%
  summarize(hooks = sum(NUM_HKS_SET, na.rm = T))

trip <- read_feather("data/trips_obs_dat.ftr") %>%
  select(TRIP_NUM, DEPART_DATETIME, ARRIVAL_DATETIME, TRIP_APPROVAL_STATUS, DECLARED_TRIP_TYPE_CODE, DECLARED_TRIP_TYPE_CODE_VAL) %>%
  rename(T_TRIP_NUM = TRIP_NUM) %>%
  mutate(arrival_mon = month(as_date(ARRIVAL_DATETIME))) %>%
  mutate(arrival_yr = year(as_date(ARRIVAL_DATETIME))) %>%
  filter(arrival_yr >= 2010) %>%
  filter(DECLARED_TRIP_TYPE_CODE == "D") %>%
  filter(str_detect(T_TRIP_NUM, "LL")) %>%
  filter(TRIP_APPROVAL_STATUS == "APPROVED")

obs_dat <- trip %>%
  left_join(gear, by = "T_TRIP_NUM") %>%
  left_join(catch, by = "T_TRIP_NUM") %>%
  mutate(ALB_CPUE = (ALBACORE / hooks) * 1000,
         BIG_CPUE = (`BIGEYE TUNA` / hooks) * 1000,
         ESC_CPUE = (ESCOLAR / hooks) * 1000,
         MAH_CPUE = (MAHIMAHI / hooks) * 1000,
         SKI_CPUE = (`SKIPJACK TUNA` / hooks) * 1000,
         WAH_CPUE = (WAHOO / hooks) * 1000,
         YEL_CPUE = (`YELLOWFIN TUNA` / hooks) * 1000,
         SIC_CPUE = (`SICKLE POMFRET` / hooks) * 1000,         
         SHO_CPUE = (`SHORTBILL SPEARFISH` / hooks) * 1000,
         OPA_CPUE = (`OPAH (MOONFISH)` / hooks) * 1000,
         STR_CPUE = (`STRIPED MARLIN` / hooks) * 1000)

mon_obs_dat <- obs_dat %>%
  group_by(month = floor_date(as_date(ARRIVAL_DATETIME), unit = "month")) %>%
  summarize(hooks = sum(hooks, na.rm = T),
            ALB = sum(ALBACORE, na.rm = T),
            BIG = sum(`BIGEYE TUNA`, na.rm = T),
            ESC = sum(ESCOLAR, na.rm = T),
            MAH = sum(MAHIMAHI, na.rm = T),
            SKI = sum(`SKIPJACK TUNA`, na.rm = T),
            WAH = sum(WAHOO, na.rm = T),
            YEL = sum(`YELLOWFIN TUNA`, na.rm = T),
            SIC = sum(`SICKLE POMFRET`, na.rm = T),
            SHO = sum(`SHORTBILL SPEARFISH`, na.rm = T),
            OPA = sum(`OPAH (MOONFISH)`, na.rm = T),
            STR = sum(`STRIPED MARLIN`, na.rm = T)) %>%
  ungroup(.) %>%
  mutate(BIG_CPUE = ((BIG) / hooks) * 1000) %>%
  mutate(YEL_CPUE = ((YEL) / hooks) * 1000) %>%
  mutate(ALB_CPUE = ((ALB) / hooks) * 1000) %>%
  mutate(ESC_CPUE = ((ESC) / hooks) * 1000) %>%
  mutate(MAH_CPUE = ((MAH) / hooks) * 1000) %>%
  mutate(SKI_CPUE = ((SKI) / hooks) * 1000) %>%
  mutate(WAH_CPUE = ((WAH) / hooks) * 1000) %>%
  mutate(SIC_CPUE = ((SIC) / hooks) * 1000) %>%
  mutate(SHO_CPUE = ((SHO) / hooks) * 1000) %>%
  mutate(OPA_CPUE = ((OPA) / hooks) * 1000) %>%
  mutate(STR_CPUE = ((STR) / hooks) * 1000) %>%
  mutate(BIG_YEL_CPUE = ((BIG + YEL) / hooks) * 1000) %>%
  mutate(ALL_CPUE = ((BIG + ALB + ESC + MAH + SKI + WAH + YEL + SIC + SHO + OPA + STR) / hooks) * 1000)

write_csv(mon_obs_dat, "data/fig_source_data.csv")

mon_log_dat <- read_feather("data/log_detail_dat.ftr") %>%
  filter(year(as_date(HAUL_DT)) >= 2010) %>%
  filter(CATCH_FISHERY == "LL_HI") %>%
  filter(SETDEPTH == "D") %>%
  mutate(HOOKSSET = as.numeric(HOOKSSET)) %>%
  group_by(HDR_LANDYR, HDR_TRIPNUM, HDR_SERIALNUM) %>%
  summarize(HOOKSSET = first(HOOKSSET),
            HAUL_DT = first(HAUL_DT),
            NUMKEPT = sum(NUMKEPT, na.rm = T)) %>%
  ungroup(.) %>%
  group_by(month = floor_date(as_date(HAUL_DT), unit = "month")) %>%
  summarize(NUMKEPT = sum(NUMKEPT, na.rm = T),
            HOOKSSET = sum(HOOKSSET, na.rm = T)) %>%
  mutate(CPUE = (NUMKEPT / HOOKSSET) * 1000)

# Create annotation data
ann_dat <- data.frame(x = c(mdy("02-20-2015"), mdy("12-20-2016")),
                      y = c(21, 21),
                      label = c("PRIMNM", "PMNM"))

ann_grp <- data.frame(x = c(mdy("12-01-2012"), mdy("12-01-2012")),
                      y = c(16, 6),
                      label = c("Aggregate Commercial", "Bigeye + Yellowfin"))

ann_spe <- data.frame(x = c(mdy("12-01-2012"), mdy("12-01-2012")),
                      y = c(5.2, 1.8),
                      label = c("Bigeye", "Yellowfin"))

# Figure comparing Lynham et al's preferred CPUE to aggregate commercial
p1 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = BIG_YEL_CPUE), color = "green") +
  geom_line(aes(x = month, y = ALL_CPUE), color = "blue") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$BIG_YEL_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$BIG_YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ALL_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ALL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ALL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ALL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$BIG_YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$BIG_YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend =max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ALL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ALL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$BIG_YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$BIG_YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$BIG_YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$BIG_YEL_CPUE)), linetype = "dashed", color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$ALL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$ALL_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 22)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = ann_dat, aes(x = x, y = y , label = label), size = 3) +
  geom_text(data = ann_grp, aes(x = x, y = y , label = label), size = 3) +
  theme_minimal()

ggsave("BIG_YELL_ALL_comp.jpg", plot = p1, width = 8, height = 3.5, units = "in")

# Figure comparing Bigeye and Yellowfin CPUE
p2 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = BIG_CPUE), color = "purple") +
  geom_line(aes(x = month, y = YEL_CPUE), color = "gold") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$BIG_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$BIG_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$YEL_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$BIG_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$BIG_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend =max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$BIG_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$BIG_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$YEL_CPUE)), linetype = "dashed", color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$BIG_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$BIG_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 22)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = ann_spe, aes(x = x, y = y , label = label), size = 3) +
  theme_minimal()

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

g <- rbind(g1, g2, size = "first")
g$widths <- grid::unit.pmax(g1$widths, g2$widths)
grid::grid.newpage()
grid::grid.draw(g)

ggsave("CPUE_timeseries_comparison.jpg", plot = g, width = 8, height = 7, units = "in")

# Figures of CPUE timeseries for 11 commonly caught species in the fishery

ann_dat2 <- data.frame(x = c(mdy("04-05-2015"), mdy("01-29-2017")),
                      y = c(10, 10),
                      label = c("PRIMNM", "PMNM"))

p3 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = BIG_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$BIG_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$BIG_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$BIG_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$BIG_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$BIG_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$BIG_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$BIG_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$BIG_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = ann_dat2, aes(x = x, y = y , label = label), size = 3) +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Bigeye"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p4 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = YEL_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$YEL_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$YEL_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$YEL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$YEL_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Yellowfin"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p5 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = ALB_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ALB_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ALB_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ALB_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ALB_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ALB_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ALB_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$ALB_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$ALB_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Albacore"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p6 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = ESC_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ESC_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ESC_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ESC_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ESC_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ESC_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ESC_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$ESC_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$ESC_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Escolar"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p7 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = MAH_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$MAH_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$MAH_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$MAH_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$MAH_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$MAH_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$MAH_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$MAH_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$MAH_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = ann_dat2, aes(x = x, y = y , label = label), size = 3) +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Mahimahi"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p8 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = SKI_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$SKI_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$SKI_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$SKI_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$SKI_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$SKI_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$SKI_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$SKI_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$SKI_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Skipjack"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p9 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = WAH_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$WAH_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$WAH_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$WAH_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$WAH_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$WAH_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$WAH_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$WAH_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$WAH_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Wahoo"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p10 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = SIC_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$SIC_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$SIC_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$SIC_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$SIC_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$SIC_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$SIC_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$SIC_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$SIC_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Sickle Pomfret"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p11 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = SHO_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$SHO_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$SHO_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$SHO_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$SHO_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$SHO_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$SHO_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$SHO_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$SHO_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Shortbill Spearfish"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p12 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = OPA_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$OPA_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$OPA_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$OPA_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$OPA_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$OPA_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$OPA_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$OPA_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$OPA_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Opah"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()

p13 <- ggplot(data = mon_obs_dat) +
  geom_line(aes(x = month, y = STR_CPUE), color = "purple") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$STR_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$STR_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$STR_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$STR_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$STR_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$STR_CPUE)), color = "darkgrey") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$STR_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016") & mon_obs_dat$month <= mdy("12-31-2017"),]$STR_CPUE)), linetype = "dashed", color = "darkgrey") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 11)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = data.frame(x = mdy("12-01-2018"), y = 10, label = "Striped Marlin"), aes(x = x, y = y , label = label), size = 5) +
  theme_minimal()


g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)
g6 <- ggplotGrob(p6)
g7 <- ggplotGrob(p7)
g8 <- ggplotGrob(p8)
g9 <- ggplotGrob(p9)
g10 <- ggplotGrob(p10)
g11 <- ggplotGrob(p11)
g12 <- ggplotGrob(p12)
g13 <- ggplotGrob(p13)

g <- grid.arrange(g3, g7, g10, g4, g8, g6, g12, g9, g11, g5, g13, ncol = 2)

ggsave("species_comparison.jpg", plot = g, width = 12, height = 12, units = "in")


# Figure comparing aggregate CPUE between Observer data and total data

p14 <- ggplot() +
  geom_line(data = mon_obs_dat, aes(x = month, y = ALL_CPUE), color = "cyan4") +
  geom_line(data = mon_log_dat, aes(x = month, y = CPUE), color = "darkorange1") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_log_dat[mon_log_dat$month < mdy("09-25-2014"),]$CPUE),
                   yend =mean(mon_log_dat[mon_log_dat$month < mdy("09-25-2014"),]$CPUE)), linetype = "dashed", color = "darkorange1") +
  geom_segment(aes(x = min(mon_obs_dat$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ALL_CPUE),
                   yend =mean(mon_obs_dat[mon_obs_dat$month < mdy("09-25-2014"),]$ALL_CPUE)), linetype = "dashed", color = "cyan4") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ALL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("09-25-2014") & mon_obs_dat$month < mdy("08-26-2016"),]$ALL_CPUE)), linetype = "dashed", color = "cyan4") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_log_dat[mon_log_dat$month > mdy("09-25-2014") & mon_log_dat$month < mdy("08-26-2016"),]$CPUE),
                   yend = mean(mon_log_dat[mon_log_dat$month > mdy("09-25-2014") & mon_log_dat$month < mdy("08-26-2016"),]$CPUE)), linetype = "dashed", color = "darkorange1") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend =max(mon_obs_dat$month), 
                   y = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ALL_CPUE),
                   yend = mean(mon_obs_dat[mon_obs_dat$month > mdy("08-26-2016"),]$ALL_CPUE)), linetype = "dashed", color = "cyan4") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_log_dat$month), 
                   y = mean(mon_log_dat[mon_log_dat$month > mdy("08-26-2016"),]$CPUE),
                   yend = mean(mon_log_dat[mon_log_dat$month > mdy("08-26-2016"),]$CPUE)), linetype = "dashed", color = "darkorange1") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 22)) +
  labs(x = "Date", y = "Catch-per-1000-hooks") +
  geom_text(data = ann_dat, aes(x = x, y = y , label = label)) +
  geom_text(data = data.frame(x = mdy("12-01-2012"), y = 17, label = "Observer aggregate CPUE"), aes(x = x, y = y , label = label), size = 3) +
  geom_text(data = data.frame(x = mdy("12-01-2012"), y = 7, label = "Logbook aggregate CPUE"), aes(x = x, y = y , label = label), size = 3) +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  theme_minimal()

ggsave("CPUE_obs_log_comp.jpg", plot = p14, width = 8, height = 3.5, units = "in")

# Calculate total average catch rates for each of the 11 commercial species
avg_CPUE <- mon_obs_dat %>%
  summarize(mean_BIG_CPUE = mean(BIG_CPUE),
            mean_YEL_CPUE = mean(YEL_CPUE),
            mean_ALB_CPUE = mean(ALB_CPUE),
            mean_ESC_CPUE = mean(ESC_CPUE),
            mean_MAH_CPUE = mean(MAH_CPUE),
            mean_SKI_CPUE = mean(SKI_CPUE),
            mean_WAH_CPUE = mean(WAH_CPUE),
            mean_SIC_CPUE = mean(SIC_CPUE),
            mean_SHO_CPUE = mean(SHO_CPUE),
            mean_OPA_CPUE = mean(OPA_CPUE),
            mean_STR_CPUE = mean(STR_CPUE)) %>%
  pivot_longer(cols = everything(), names_to = "SPE", values_to = "Mean_CPUE") %>%
  arrange(-Mean_CPUE)


# Generate revenue per unit effort figure

# Calculate average value per fish for each species
dealer_dat <- read_csv("data/dealer_dat.csv")

names <- dealer_dat %>%
  group_by(ORIG_SPECIES_NAME) %>%
  summarize(SPECIES_FK = first(SPECIES_FK))

# mahimahi = 13, bigeye = 6, yellowfin = 3, albacore = 4, escolar = 102, skipjack = 2, wahoo = 14, shortbilled spearfish = 107, striped marlin = 9, sickle pomfret = 118, opah = 106
value_spc <- dealer_dat %>%
  mutate(year = year(REPORT_DATE)) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(NUM_SOLD > 0) %>%
  mutate(rev_per_ind = SOLD_REVENUE / NUM_SOLD) %>%
  group_by(SPECIES_FK) %>%
  summarize(rev_per_ind = mean(rev_per_ind, na.rm = T),
            orig_name = first(ORIG_SPECIES_NAME)) %>%
  filter(SPECIES_FK %in% c(13, 6, 3, 4, 102, 2, 14, 107, 9, 118, 106))

mon_obs_dat_rev = mon_obs_dat %>%
  mutate(BIG_REV = BIG * value_spc[value_spc$SPECIES_FK == 6,]$rev_per_ind,
         YEL_REV = YEL * value_spc[value_spc$SPECIES_FK == 3,]$rev_per_ind,
         ALB_REV = ALB * value_spc[value_spc$SPECIES_FK == 4,]$rev_per_ind,
         ESC_REV = ESC * value_spc[value_spc$SPECIES_FK == 102,]$rev_per_ind,
         MAH_REV = MAH * value_spc[value_spc$SPECIES_FK == 13,]$rev_per_ind,
         SKI_REV = SKI * value_spc[value_spc$SPECIES_FK == 2,]$rev_per_ind,
         WAH_REV = WAH * value_spc[value_spc$SPECIES_FK == 14,]$rev_per_ind,
         SIC_REV = SIC * value_spc[value_spc$SPECIES_FK == 118,]$rev_per_ind,
         SHO_REV = SHO * value_spc[value_spc$SPECIES_FK == 107,]$rev_per_ind,
         OPA_REV = OPA * value_spc[value_spc$SPECIES_FK == 106,]$rev_per_ind,
         STR_REV = STR * value_spc[value_spc$SPECIES_FK == 9,]$rev_per_ind) %>%
  mutate(BIG_YEL_RPUE = ((BIG_REV + YEL_REV) / hooks) * 1000) %>%
  mutate(ALL_RPUE = ((BIG_REV + ALB_REV + ESC_REV + MAH_REV + SKI_REV + WAH_REV + YEL_REV + SIC_REV + SHO_REV + OPA_REV + STR_REV) / hooks) * 1000)

write_csv(mon_obs_dat_rev, "data/fig_source_data_rev.csv")

ann_dat_rev <- data.frame(x = c(mdy("02-20-2015"), mdy("12-20-2016")),
                      y = c(3080, 3080),
                      label = c("PRIMNM", "PMNM"))

ann_grp_rev <- data.frame(x = c(mdy("05-01-2012"), mdy("05-01-2012")),
                      y = c(2200, 900),
                      label = c("Aggregate Commercial", "Bigeye + Yellowfin"))

p1 <- ggplot(data = mon_obs_dat_rev) +
  geom_line(aes(x = month, y = BIG_YEL_RPUE), color = "green") +
  geom_line(aes(x = month, y = ALL_RPUE), color = "blue") +
  geom_vline(xintercept = mdy("09-25-2014"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mdy("08-26-2016"), linetype = "dashed", color = "red") +
  geom_segment(aes(x = min(mon_obs_dat_rev$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month < mdy("09-25-2014"),]$BIG_YEL_RPUE),
                   yend =mean(mon_obs_dat_rev[mon_obs_dat_rev$month < mdy("09-25-2014"),]$BIG_YEL_RPUE)), color = "darkgreen") +
  geom_segment(aes(x = min(mon_obs_dat_rev$month),
                   xend = mdy("09-25-2014"), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month < mdy("09-25-2014"),]$ALL_RPUE),
                   yend =mean(mon_obs_dat_rev[mon_obs_dat_rev$month < mdy("09-25-2014"),]$ALL_RPUE)), color = "darkblue") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("09-25-2014") & mon_obs_dat_rev$month < mdy("08-26-2016"),]$ALL_RPUE),
                   yend = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("09-25-2014") & mon_obs_dat_rev$month < mdy("08-26-2016"),]$ALL_RPUE)), color = "darkblue") +
  geom_segment(aes(x = mdy("09-25-2014"),
                   xend = mdy("08-26-2016"), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("09-25-2014") & mon_obs_dat_rev$month < mdy("08-26-2016"),]$BIG_YEL_RPUE),
                   yend = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("09-25-2014") & mon_obs_dat_rev$month < mdy("08-26-2016"),]$BIG_YEL_RPUE)), color = "darkgreen") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend =max(mon_obs_dat_rev$month), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016"),]$ALL_RPUE),
                   yend = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016"),]$ALL_RPUE)), color = "darkblue") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = max(mon_obs_dat_rev$month), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016"),]$BIG_YEL_RPUE),
                   yend = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016"),]$BIG_YEL_RPUE)), color = "darkgreen") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016") & mon_obs_dat_rev$month <= mdy("12-31-2017"),]$BIG_YEL_RPUE),
                   yend = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016") & mon_obs_dat_rev$month <= mdy("12-31-2017"),]$BIG_YEL_RPUE)), linetype = "dashed", color = "darkgreen") +
  geom_segment(aes(x = mdy("08-26-2016"),
                   xend = mdy("12-31-2017"), 
                   y = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016") & mon_obs_dat_rev$month <= mdy("12-31-2017"),]$ALL_RPUE),
                   yend = mean(mon_obs_dat_rev[mon_obs_dat_rev$month > mdy("08-26-2016") & mon_obs_dat_rev$month <= mdy("12-31-2017"),]$ALL_RPUE)), linetype = "dashed", color = "darkblue") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  #scale_y_continuous(limits = c(0, 22)) +
  labs(x = "Date", y = "Revenue-per-1000-hooks") +
  geom_text(data = ann_dat_rev, aes(x = x, y = y , label = label), size = 3) +
  geom_text(data = ann_grp_rev, aes(x = x, y = y , label = label), size = 3) +
  theme_minimal()

ggsave("BIG_YELL_ALL_comp_rev.jpg", plot = p1, width = 8, height = 3.5, units = "in")
