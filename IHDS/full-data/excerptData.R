ihds_excerpt <- da36151.0002 %>% select(IDHH, INCOME, FU1, CGMOTORV, CG8, CG21, METRO, METRO6)

save(ihds_excerpt, file = "ihds_excerpt.rda")

