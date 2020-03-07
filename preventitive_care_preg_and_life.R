## Goal: create a graphic to display care during pregnancy 
## as a function of time

library(tidyverse)
library(here)
library(grid)
library(gridExtra)
trimesters <- tibble(week = c(0, 13, 27, 40), type="trimester")
visit_freq <- tibble(week = c(seq(from = 7, to = 28, by = 4), 
                              seq(from=29, to = 36, by = 2), 
                              seq(from=36, to = 42, by = 1)), type = "Visit frequency",
                    label = "First visit") %>% 
              mutate(label=ifelse(week == week[1], label, NA))
missed_period <- tibble(week = 4, type="First missed period")
tdap <- tibble(week = c(27,36), type="Tdap", clss="immunization")
flu <- tibble(week = c(7,40), type="Flu", clss = "immunization")
first_trim_screen <- tibble(week = c(9,13), type="FTS", clss = "screening") #aneuploidy
cell_free_fetal_dna <- tibble(week = c(10, 40), type = "Cell free Fetal DNA", clss = "screening")
sec_trim_screen <- tibble(week = c(15,22), type="STS", clss = "screening") #NT defects + aneuploidy
sec_trim_ultrasound <- tibble(week = c(18,20), type = "Second trimester ultrasound", clss = "screening")
amniocentesis <- tibble(week = c(15,20), type="Amniocentesis", clss = "screening") #definitive
cvs <- tibble(week = c(10,13), type="Chorionic Villus Sampling", clss = "screening")
cordocentesis <- tibble(week = c(18,40), type="Cordocentesis", clss = "screening")
rhogam <- tibble(week = c(28,32, 40), type="RhoGAM", clss = "immunization")  
ultrasound <- tibble(week = c(10,12, 18, 22), type = "Ultrasound", clss = "screening")
gestational_diabetes <- tibble(week = c(24, 28), type = "Gestational diabetes", clss = "screening")
antibody_screen_initial <- tibble(week=c(7,13), type = "Initial antibody screen", clss = "screening") #first prenatal visit
antibody_screen <- tibble(week = c(24,28), type = "Repeat antibody screen", clss = "screening")# (Rh(D) neg. only)") 
anemia_screen <- tibble(week = c(24,28), type = "Repeat anemia screen", clss = "screening") 
gbs_screen <- tibble(week=c(35,37), type= "Group B Strep", clss = "screening")
induction <- tibble(week = c(41), type = "Induction", clss = "intervention")
hellp_delivery <- tibble(week = 34, type = "Delivery - HELLP or sPEC or PPROM", clss = "intervention")
pec_delivery <- tibble(week=37, type = "Delivery - PEC", clss = "intervention")
cervical_cerclage <- tibble(week = c(14), type = "Cervical cerclage", clss = "intervention")
fetal_fibronectin <- tibble(week = c(22, 35), type = "Fetal fibronectin", clss = "intervention")
## to reduce risk of Premature labor, give 17-Hydroxyprogesterone weekly starting at week 16-20 until week 36
hydroxyprogesterone_17 <- tibble(week = c(16, 36), type = "17-Hydroxyprogesterone", clss = "intervention") 
betamethasone <- tibble(week = c(24, 37), type = "Betamethasone", clss = "intervention") # for fetal lung maturation
cerclage <- tibble(week = c(14), type = "Cervical cerclage", clss = "intervention") #for incompetent cervix
bacteriuria <- tibble(week = c(12,16), type = "UTI screen", clss = "screening")
hsv_prophylaxis <- tibble(week=c(36, 42), type = "HSV prophylaxis", clss = "intervention")
pid <- tibble(week = c(-6,13), type = "PID", clss = "disease") #pid? more common in first trimester and before conception (cervical mucous thickens after that)
mmrv <- tibble(week = -6, type = "MMRV", clss = "immunization")
intrahepatic_cholestatis_preg <- tibble(week = c(27,40), type = "Intrahepatic cholestasis of Preg.", clss = "disease") #delivery at 37 weeks
aflp <- tibble(week = c(30,40), type = "Acute Fatty Liver of Preg.", clss= "disease") #third trimester, typically
amniotic_fluid_embol <- tibble(week = 40, type = "Amniotic Fluid Embolism", clss = "disease") #occurs at delivery or immediately post-partum, May present with DIC.
external_cephalic_version <- tibble(week = c(37,40), type = "External Cephalic Version", clss = "intervention")
delivery_placenta_previa <- tibble(week = c(37, 40), type = "Delivery - Placenta Previa", clss = "intervention")
peripartum_cardiomyopathy <- tibble(week = c(36, 42), type = "Peripartum Cardiomyopathy", clss = "disease")
indomethacin_tocolysis <- tibble(week = c(24, 32), type = "Tocolysis - Indomethacin", clss = "intervention") # only give tocolysis to women at a gestational age (< 34 weeks) when delay in delivery for 48 hours would provide benefit to newborn (buys you time to give steroids for lung maturity)
nifedipine_tocolysis <- tibble(week = c(32, 34), type = "Tocolysis - Nifedipine", clss = "intervention")
magnesium_sulfate_neuroprotect <- tibble(week = c(24, 32), type = "MgSO4 - Neuroprotection", clss = "intervention") # give when pretern birth is anticipated in next 24 hours
hyperemesis_gravidarum <- tibble(week = c(6,15), type = "Hyperemesis gravidarum", clss = "disease")
weekly_BPPs_gest_htn <- tibble(week = c(32, 40), type = "Weekly BP monitoring", clss = "intervention")
puppp <- tibble(week = c(35,36), type = "PUPPP", clss = "disease")
preeclampsia <- tibble(week = c(20, 40), type = "Preeclampsia", clss="disease")
placental_abruption <- tibble(week = c(25), type = "Placental abruption", clss = "disease")
pprom_abx_ster <- tibble(week = c(24,34), type = "PPROM - Azith+amp+amox+ster", clss = "disease")
pprom_deliv <- tibble(week = c(34,37), type="PPROM - deliv", clss = "disease")
aspirin_pplx_pec <- tibble(week = c(12, 38), type = "Aspirin - PEC pplxs", clss = "intervention")

d <- bind_rows(visit_freq, missed_period, tdap, flu, 
               first_trim_screen, sec_trim_screen, cell_free_fetal_dna,
               sec_trim_ultrasound,
               amniocentesis, cvs,
               cordocentesis, rhogam, ultrasound,
               gestational_diabetes, antibody_screen,
               anemia_screen, gbs_screen, induction,
               hydroxyprogesterone_17, betamethasone,
               cerclage, hsv_prophylaxis, bacteriuria, antibody_screen_initial,
               hellp_delivery, pid, mmrv, intrahepatic_cholestatis_preg, 
               amniotic_fluid_embol, pec_delivery, aflp,
               external_cephalic_version, delivery_placenta_previa,
               peripartum_cardiomyopathy, indomethacin_tocolysis,
               magnesium_sulfate_neuroprotect, nifedipine_tocolysis,
               hyperemesis_gravidarum, weekly_BPPs_gest_htn, puppp,
               preeclampsia, placental_abruption,
               pprom_deliv, pprom_abx_ster, aspirin_pplx_pec) %>% 
    group_by(clss) %>% arrange(clss, week) %>% 
    ungroup() %>%   
  mutate(type = ordered(type, levels = rev(unique(type)))) %>%
  replace_na(list(clss = "other")) %>%
  mutate(clss=ordered(clss, levels = c("disease", "immunization", "intervention", "screening", "other")))
  
two_points <- d %>% select(week, type) %>% 
  group_by(type) %>% 
  summarise(count = n()) %>% 
  filter(count == 2) %>% 
  pull(type)

weeks_represented <- unique(d$week) %>% sort()

p <- ggplot(d, aes(x=week, y=type, fill=clss, label=label)) + 
  geom_vline(xintercept = trimesters$week, linetype="dotted") + 
  geom_point(pch=21, size=2) + 
  geom_line(data = subset(d, type %in% two_points)) + 
  geom_line(data = subset(d, type %in% "RhoGAM" & week <40)) +
  geom_line(data = subset(d, type %in% "Ultrasound" & week >=14)) +
  geom_line(data = subset(d, type %in% "Ultrasound" & week <14)) +
  geom_point(pch=21, size=2) + 
  theme_minimal() +
  ylab("") + xlab("Week of Pregnancy") +
  labs(fill="") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(-6, 44, 2)) + 
  scale_fill_brewer(palette = "Set2") +
  ggtitle("2019 Guidelines on Prenatal Care & Diseases in Pregnancy")
p2 <- grid.arrange(p, bottom = textGrob("© 2020 Daniel Piqué ", x = 1, 
                                    hjust = 1, gp = gpar(fontsize = 9)))

ggsave(filename = "ob_timeline.png", plot = p2,
       device = "png", path = here("images/"), 
       width = 6.4, height = 4.6, scale = 1.6 )

ggsave(filename = "ob_timeline.pdf", plot = p2,
       device = "pdf", path = here("images/"), 
       width = 6.4, height = 4.6, scale = 1.6 )

