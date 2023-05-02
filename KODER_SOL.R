library(WDI)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(kableExtra)
library(modelsummary)
library(ggplot2)

#################################################
# Hadde ikke noe i mot koden som var lagt til oppgaven. Har gjort den om til min egen.

BNP_0<-WDI(
  country = "all",
  indicator = c('BNP_pr_innb'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


BNP_0 <- BNP_0 %>% 
  dplyr::select(year, country, BNP_pr_innb, iso2c, iso3c, region, income)

BNP_0<-BNP_0[!grepl("Aggregates", BNP_0$region),] # Fjerner variabler på aggregert nivå

BNP_1 <- BNP_0[complete.cases(BNP_0$BNP_pr_innb, BNP_0$iso3c),] %>% 
  mutate(year=as.numeric(year)) %>% 
  arrange(year)

BNP_1<-BNP_1[order(BNP_1$country),]

# Lager et datasett med nivå på BNP per innbyggere i år 2000
BNP_1_2000  <- BNP_1 %>%  
  arrange(iso3c, year) %>% 
  dplyr::group_by(iso3c) %>% #Beholder den første observasjonen for BNP per innbyggere (Y0)
  slice(1) %>%
  dplyr::ungroup()

BNP_1_2000<-  BNP_1_2000 %>% 
  dplyr::select(-year) 

BNP_2 <- left_join(BNP_1,BNP_1_2000, by=c("country", "iso2c", "iso3c", "region", "income")) 

names(BNP_2)[names(BNP_2) == "BNP_pr_innb.x"] <- "BNP_pr_innb"
names(BNP_2)[names(BNP_2) == "BNP_pr_innb.y"] <- "BNP_pr_innb_Y0"

BNP_2 <- BNP_2 %>% 
  relocate(country, region, income, iso2c, iso3c, year, BNP_pr_innb, BNP_pr_innb_Y0)

###########################################

# Utdanning BAR.SCHL.15UP 
Utdanning_0<-WDI(
  country = "all",
  indicator = c('Utdanning'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

Utdanning_0 <- Utdanning_0 %>% 
  dplyr::select(country, region, income, iso2c, iso3c, year, Utdanning) %>%  
  arrange(iso3c, year) 

Utdanning_0 <- Utdanning_0[complete.cases(Utdanning_0$Utdanning),] %>%  
  arrange(iso3c, year) 

Utdanning_0 <- Utdanning_0 %>%  
  arrange(iso3c, year) %>%   
  dplyr::mutate(Utdanning = as.numeric(Utdanning, na.rm = T)) %>% 
  dplyr::group_by(country) %>% 
  dplyr::mutate(Utdanning_snitt=mean(Utdanning)) %>% 
  dplyr::ungroup() 

Utdanning_1 <- Utdanning_0 %>% 
  dplyr::select(-c(year, Utdanning)) 

Utdanning_2 <- Utdanning_1[!duplicated(Utdanning_1[c("iso3c")]), ] %>% 
  
  ###########################################

BNI_0<-WDI(
  country = "all",
  indicator = c( 'NSY'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

BNI_0 <- BNI_0 %>% 
  dplyr::select(country, region, income, iso2c, iso3c, year, NSY) %>%  
  arrange(iso3c, year) 

BNI_0 <- BNI_0[complete.cases(BNI_0$NSY),] %>%  
  arrange(iso3c, year) 

BNI_1 = BNI_0 %>%   
  arrange(iso3c, year) %>%  
  dplyr::mutate(NSY = as.numeric(NSY, na.rm = TRUE)) %>% 
  ddply("iso3c",transform,
        NSY_snitt=mean(NSY)) 

BNI_1 <- BNI_1 %>% 
  dplyr::select(-year) 

BNI_2 <- BNI_1[!duplicated(BNI_1[c("iso3c")]), ]  %>%
  
  ################################################

################################################
# Vekst i arbeidskraften (n)
Arb_Kraft<-WDI(
  country = "all",
  indicator = c('Arb_kraft'="JI.TLF.TOTL"), 
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)

Arb_Kraft<-Arb_Kraft %>% 
  dplyr::select(country, region, income, iso2c, year, Arb_kraft) %>%  
  arrange(iso2c, year) %>% 
  dplyr::rename("iso3c" = "iso2c") %>% 
  dplyr::mutate(year= as.numeric(year))

Arb_Kraft[Arb_Kraft == 0]<-NA

Arb_Kraft_1 <- Arb_Kraft[complete.cases(Arb_Kraft$iso3c, Arb_Kraft$Arb_kraft),]

Arb_Kraft_1 <- Arb_Kraft_1[!duplicated(Arb_Kraft_1[c("iso3c", "year")]), ] %>%
  arrange(iso3c, year) 

# library(plyr)
Vekstrate_n <- Arb_Kraft_1 %>%  
  arrange(iso3c, year) %>%  
  plyr::ddply("iso3c",transform,
              t_år=c(NA,diff(year)),
              Arb_kraft_vekst=c(NA,diff(log(Arb_kraft)))) 

Vekstrate_n <- Vekstrate_n[complete.cases(Vekstrate_n$t_år, Vekstrate_n$Arb_kraft_vekst),] 

Vekstrate_n_1 <- Vekstrate_n %>%  
  dplyr::mutate(t_år = as.numeric(t_år),
                Arb_kraft_vekst = as.numeric(Arb_kraft_vekst)) %>% 
  dplyr::mutate(årlig_vekstrate_n=Arb_kraft_vekst/t_år*100)

# gjennomsnittlig vekstraten i arbeidskraften for hvert land
Vekstrate_n_1 <- Vekstrate_n_1 %>% # 
  dplyr::group_by(iso3c) %>% 
  dplyr::mutate(årlig_vekstrate_n_snitt=mean(årlig_vekstrate_n, na.rm = TRUE)) %>% 
  dplyr::ungroup()

Vekstrate_n_1 <- Vekstrate_n_1 %>% 
  dplyr::select(country, iso3c, årlig_vekstrate_n_snitt)

Vekstrate_n_1 <- Vekstrate_n_1[!duplicated(Vekstrate_n_1["iso3c"]), ]  %>%  
  arrange(iso3c) 

# setter sammen BNP, utdanning, sparing, og arbeidskraft til et komplett datasett.

df_1 <- left_join(BNP_2, Utdanning_2, by=c("country", "iso2c", "iso3c", "region", "income"))
df_2 <- left_join(df_1, BNI_2, by=c("country", "iso2c", "iso3c", "region", "income"))
df_3 <- left_join(df_2, Vekstrate_n_1, by="iso3c")

df_comp <- df_3 %>% 
  dplyr::select(country.x, region, income, iso2c, iso3c, year,BNP_pr_innb,
                BNP_pr_innb_Y0,Utdanning_snitt, NSY_snitt, årlig_vekstrate_n_snitt) %>%  
  dplyr::rename(country=country.x)

###########################################

Andre_faktorer<-WDI(
  country = "all",
  indicator = c('Total_pop'="SP.POP.TOTL", 
                'Vekstrate_årlig_invest'="NE.GDI.FTOT.KD.ZG", 
                'Vekstrate_årlig_export'="NE.EXP.GNFS.KD.ZG", 
                'Reduk_rate_naturres'="NY.ADJ.DRES.GN.ZS", 
                'Befolkningsvekstrate'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


Andre_faktorer_1 <- Andre_faktorer[complete.cases(Andre_faktorer$iso3c),] %>%
  arrange(iso3c) 

Andre_faktorer_2<-Andre_faktorer_1[!grepl("Aggregates", Andre_faktorer_1$region),]

Andre_faktorer_2<-Andre_faktorer_2[order(Andre_faktorer_2$country),]

Andre_faktorer_2 <- Andre_faktorer_2 %>% 
  dplyr::select("country", "region", "income", "iso3c", "iso2c", "year",
                "Total_pop", "Befolkningsvekstrate", "Reduk_rate_naturres",
                "Vekstrate_årlig_invest", "Vekstrate_årlig_export")

Bestemmelsesfaktorer <- left_join(df_comp, Andre_faktorer_2, by=c("country", "region", "income", 
                                                                  "iso2c", "iso3c", "year"))


Bestemmelsesfaktorer<-Bestemmelsesfaktorer %>% 
  relocate(country, region, income, iso2c, iso3c, year, BNP_pr_innb, BNP_pr_innb_Y0,
           Total_pop, Befolkningsvekstrate,årlig_vekstrate_n_snitt, NSY_snitt,
           Reduk_rate_naturres,Vekstrate_årlig_invest,Vekstrate_årlig_export, Utdanning_snitt) 

Bestemmelsesfaktorer <- Bestemmelsesfaktorer[complete.cases(Bestemmelsesfaktorer$iso3c),] %>%
  arrange(iso3c) 


# vekstraten og gjennomsnitt for resterende variabler
Vekst_Bestemfak <- Bestemmelsesfaktorer %>%  
  arrange(iso3c, year) %>%  # Sorter på år og land
  dplyr::filter(!country== c("High income", "Low income", "Lower middle income", "Upper middle income")) %>% 
  dplyr::group_by(country) %>% 
  plyr::ddply("iso3c",transform,
              vekst_BNP_pr_innb=c(NA,diff(log(BNP_pr_innb)))*100) %>%
  plyr::ddply("iso3c",transform,
              snitt_Befolkningsvekstrate=mean(Befolkningsvekstrate, na.rm = TRUE),
              vekst_BNP_pr_innb = as.numeric(vekst_BNP_pr_innb, na.rm = TRUE),
              snitt_vekst_BNP_pr_innb=mean(vekst_BNP_pr_innb, na.rm = TRUE),
              snitt_Vekstrate_årlig_invest=mean(Vekstrate_årlig_invest, na.rm = TRUE), 
              snitt_Reduk_rate_naturres=mean(Reduk_rate_naturres, na.rm = TRUE),
              snitt_Vekstrate_årlig_export=mean(Vekstrate_årlig_export, na.rm = TRUE)) %>%
  dplyr::ungroup()

Vekst_Bestemfak <- Vekst_Bestemfak[complete.cases(Vekst_Bestemfak$country, 
                                                  Vekst_Bestemfak$income,
                                                  Vekst_Bestemfak$iso3c, 
                                                  Vekst_Bestemfak$snitt_vekst_BNP_pr_innb, 
                                                  Vekst_Bestemfak$BNP_pr_innb_Y0, 
                                                  Vekst_Bestemfak$årlig_vekstrate_n_snitt, 
                                                  Vekst_Bestemfak$snitt_Befolkningsvekstrate, 
                                                  Vekst_Bestemfak$NSY_snitt,
                                                  Vekst_Bestemfak$snitt_Reduk_rate_naturres,
                                                  Vekst_Bestemfak$snitt_Vekstrate_årlig_invest, 
                                                  Vekst_Bestemfak$snitt_Vekstrate_årlig_export, 
                                                  Vekst_Bestemfak$Utdanning_snitt),] # Ta vekk land som mangler data 



#summary(Vekst_Bestemfak$country)

Vekst_Bestemfak <- Vekst_Bestemfak %>% 
  dplyr::select(country, region, income, iso3c, iso2c,year, Total_pop, BNP_pr_innb, BNP_pr_innb_Y0,
                snitt_vekst_BNP_pr_innb, årlig_vekstrate_n_snitt, snitt_Befolkningsvekstrate, NSY_snitt, snitt_Reduk_rate_naturres,
                snitt_Vekstrate_årlig_invest, 
                snitt_Vekstrate_årlig_export, Utdanning_snitt)

# Lager datasettet som er ment til å brukes i analysen
Vekst_Bestemfak2019  <- Vekst_Bestemfak %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  slice(n()) %>% # Beholder den siste observasjonen for hvert land
  ungroup()

# for å forenkle tolkningen, og presser sammen fordelingen
Vekst_Bestemfak2019$BNP_pr_innb <- as.numeric(Vekst_Bestemfak2019$BNP_pr_innb)
Vekst_Bestemfak2019$Nivå_BNP_pr_innb <- log(Vekst_Bestemfak2019$BNP_pr_innb) 
Vekst_Bestemfak2019$Nivå_BNP_pr_innb_Y0 <- log(Vekst_Bestemfak2019$BNP_pr_innb_Y0)

##################################################

# Lager diskriptiv statistisk tabell med bruk av modellsummary::datasummary() 
# og pakken kableExtra 

D_T <- Vekst_Bestemfak2019 %>%  
  dplyr::select(snitt_vekst_BNP_pr_innb,
                årlig_vekstrate_n_snitt, 
                snitt_Befolkningsvekstrate,
                NSY_snitt,
                snitt_Reduk_rate_naturres,
                snitt_Vekstrate_årlig_invest, 
                snitt_Vekstrate_årlig_export, 
                Utdanning_snitt) %>% 
  dplyr::rename("Gjennomsnitlig årlig vekstrate i BNP pc 2000-2019 (%)"="snitt_vekst_BNP_pr_innb",
                "Gjennomsnittlig årlig vekst i arbeidskraft (%)"="årlig_vekstrate_n_snitt",
                "Gjennomsnittlig årlig befolkningsvekst (%)"="snitt_Befolkningsvekstrate",
                "Gjennomsnittlig årlig sparerate (%)"="NSY_snitt",
                "Gjennomsnittlig årlig reduksjon i naturresurser (%)"="snitt_Reduk_rate_naturres",
                "Gjennomsnittlig årlig vekst i Investeringer (%)"="snitt_Vekstrate_årlig_invest", 
                "Gjennomsnittlig årlig vekst i Eksport (%)"="snitt_Vekstrate_årlig_export",
                "Gjennomsnittlig år på skole (år)"="Utdanning_snitt") %>% 
  as.data.frame()

# skalerer variabler med lapply for boxplot

D_T_list <- lapply(D_T, na.omit)
D_T_list <- lapply(D_T_list, scale)

# Denne er i bruker manualen til modelsummary
tomme_kol <- function(x) " "

# Legger inn hva jeg ønsker i tabellen.
datasummary(All(D_T) ~ N + Mean + Min + Max + SD + Heading("Boxplot") * tomme_kol,
            data = D_T, output = "kableExtra",
            title = 'Deskriptiv Statistisk') %>% 
  column_spec(column = 7, image = spec_boxplot(D_T_list)) %>%
  row_spec(row = 0, 
           font_size = 18) %>% 
  kable_paper(full_width = F) %>% 
  footnote("Inneholder kun land med data for perioden 2000-2019 (WDI)")

########################################################

# Før IQR grupperes det på "income"
# 

Vekst_Bestemfak2019<-Vekst_Bestemfak2019 %>% 
  group_by(income)

# Investeringer
Q1vekst_invest <- quantile(Vekst_Bestemfak2019$snitt_Vekstrate_årlig_invest, .25)
Q3vekst_invest <- quantile(Vekst_Bestemfak2019$snitt_Vekstrate_årlig_invest, .75)
IQRvekst_invest <- IQR(Vekst_Bestemfak2019$snitt_Vekstrate_årlig_invest)

upper <- Q3vekst_invest + 1.5 * IQRvekst_invest
lower <- Q1vekst_invest - 1.5 * IQRvekst_invest

Vekst_Bestemfak2019$uteliggere_invest <- Vekst_Bestemfak2019$snitt_Vekstrate_årlig_invest < lower | Vekst_Bestemfak2019$snitt_Vekstrate_årlig_invest > upper

p1<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = snitt_Vekstrate_årlig_invest)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Investeringer") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_invest == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Reduksjon naturressurser
Q1Reduk_rate_naturres <- quantile(Vekst_Bestemfak2019$snitt_Reduk_rate_naturres, .25)
Q3Reduk_rate_naturres <- quantile(Vekst_Bestemfak2019$snitt_Reduk_rate_naturres, .75)
IQRReduk_rate_naturres <- IQR(Vekst_Bestemfak2019$snitt_Reduk_rate_naturres)

upper <- Q3Reduk_rate_naturres + 1.5 * IQRReduk_rate_naturres
lower <- Q1Reduk_rate_naturres - 1.5 * IQRReduk_rate_naturres

Vekst_Bestemfak2019$uteliggere_naturres <- Vekst_Bestemfak2019$snitt_Reduk_rate_naturres < lower | Vekst_Bestemfak2019$snitt_Reduk_rate_naturres > upper

p2<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = snitt_Reduk_rate_naturres)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Reduk. Natur") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_naturres == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Export
Q1vekst_export <- quantile(Vekst_Bestemfak2019$snitt_Vekstrate_årlig_export, .25)
Q3vekst_export <- quantile(Vekst_Bestemfak2019$snitt_Vekstrate_årlig_export, .75)
IQRvekst_export <- IQR(Vekst_Bestemfak2019$snitt_Vekstrate_årlig_export)

upper <- Q3vekst_export + 1.5 * IQRvekst_export
lower <- Q1vekst_export - 1.5 * IQRvekst_export

Vekst_Bestemfak2019$uteliggere_export <- Vekst_Bestemfak2019$snitt_Vekstrate_årlig_export < lower | Vekst_Bestemfak2019$snitt_Vekstrate_årlig_export > upper

p3<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = snitt_Vekstrate_årlig_export)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Export vekst") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_export == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Arbeidskraft
Q1vekst_arb_n <- quantile(Vekst_Bestemfak2019$årlig_vekstrate_n_snitt, .25)
Q3vekst_arb_n <- quantile(Vekst_Bestemfak2019$årlig_vekstrate_n_snitt, .75)
IQRvekst_arb_n <- IQR(Vekst_Bestemfak2019$årlig_vekstrate_n_snitt)

upper <- Q3vekst_arb_n + 1.5 * IQRvekst_arb_n
lower <- Q1vekst_arb_n - 1.5 * IQRvekst_arb_n

Vekst_Bestemfak2019$uteliggere_arb_n <- Vekst_Bestemfak2019$årlig_vekstrate_n_snitt < lower | Vekst_Bestemfak2019$årlig_vekstrate_n_snitt > upper

p4<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = årlig_vekstrate_n_snitt)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Arbeidskraft vekst") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_arb_n == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Sparing
Q1vekst_spare <- quantile(Vekst_Bestemfak2019$NSY_snitt, .25)
Q3vekst_spare <- quantile(Vekst_Bestemfak2019$NSY_snitt, .75)
IQRvekst_spare <- IQR(Vekst_Bestemfak2019$NSY_snitt)

upper <- Q3vekst_spare + 1.5 * IQRvekst_spare
lower <- Q1vekst_spare - 1.5 * IQRvekst_spare

Vekst_Bestemfak2019$uteliggere_spare <- Vekst_Bestemfak2019$NSY_snitt < lower | Vekst_Bestemfak2019$NSY_snitt > upper

p5<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = NSY_snitt)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Sparerate") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_spare == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Befolkningvekstrate
Q1vekst_Befolkning <- quantile(Vekst_Bestemfak2019$snitt_Befolkningsvekstrate , .25)
Q3vekst_Befolkning <- quantile(Vekst_Bestemfak2019$snitt_Befolkningsvekstrate, .75)
IQRvekst_Befolkning <- IQR(Vekst_Bestemfak2019$snitt_Befolkningsvekstrate)

upper <- Q3vekst_Befolkning + 1.5 * IQRvekst_Befolkning
lower <- Q1vekst_Befolkning - 1.5 * IQRvekst_Befolkning

Vekst_Bestemfak2019$uteliggere_Befolkning <- Vekst_Bestemfak2019$snitt_Befolkningsvekstrate < lower | Vekst_Bestemfak2019$snitt_Befolkningsvekstrate > upper

p6<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = snitt_Befolkningsvekstrate)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Befolkningsvekstrate") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_Befolkning == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Utdanning
Q1Utdanning <- quantile(Vekst_Bestemfak2019$Utdanning_snitt , .25)
Q3Utdanning <- quantile(Vekst_Bestemfak2019$Utdanning_snitt, .75)
IQRUtdanning <- IQR(Vekst_Bestemfak2019$Utdanning_snitt)

upper <- Q3Utdanning + 1.5 * IQRUtdanning
lower <- Q1Utdanning - 1.5 * IQRUtdanning

Vekst_Bestemfak2019$uteliggere_Utdanning <- Vekst_Bestemfak2019$Utdanning_snitt < lower | Vekst_Bestemfak2019$Utdanning_snitt > upper

p7<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = Utdanning_snitt)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="Utdanning") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_Utdanning == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# BNP0
Q1BNP0 <- quantile(Vekst_Bestemfak2019$Nivå_BNP_pr_innb_Y0, .25)
Q3BNP0 <- quantile(Vekst_Bestemfak2019$Nivå_BNP_pr_innb_Y0, .75)
IQRBNP0 <- IQR(Vekst_Bestemfak2019$Nivå_BNP_pr_innb_Y0)

upper <- Q3BNP0 + 1.5 * IQRBNP0
lower <- Q1BNP0 - 1.5 * IQRBNP0

Vekst_Bestemfak2019$uteliggere_BNP0 <- Vekst_Bestemfak2019$Nivå_BNP_pr_innb_Y0 < lower | Vekst_Bestemfak2019$Nivå_BNP_pr_innb_Y0 > upper

p8<-Vekst_Bestemfak2019 %>%
  ggplot(aes(x = "", y = Nivå_BNP_pr_innb_Y0)) +
  geom_boxplot(alpha = 0.5, fill = "slateblue3") +
  labs(x="",
       y="BNPY0") +
  theme_light()+
  theme(axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  geom_text_repel(data = Vekst_Bestemfak2019[which(Vekst_Bestemfak2019$uteliggere_BNP0 == TRUE),],
                  aes(label = country), size = 3, family= "LM Roman 10")

# Plotter alle variablene med navn på country for å se hvilke land det er som skiller
# seg ut
figur<- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,
                  ncol=4,nrow=3) + theme_pubclean()
annotate_figure(figur,
                top=text_grob("Boxplot Outliers",color="black",face="italic",
                              family="LM Roman 10", size=12))


# fjerner grupperingen og landene som ikke skal være med videre
Vekst_Bestemfak2019<-Vekst_Bestemfak2019 %>% ungroup()

# Da blir dette mitt datasett til analysene.
Analyse <- Vekst_Bestemfak2019 %>% 
  filter(!country=="Sierra Leone" & !country=="Congo, Dem. Rep." & !country=="Congo, Rep." &
           !country=="Kazakhstan" & !country=="Ukraine" & !country=="Mozambique" )


# Ny deskriptiv statistisk tabell 

D_T2 <- Analyse %>%  
  dplyr::select(snitt_vekst_BNP_pr_innb,
                årlig_vekstrate_n_snitt, 
                snitt_Befolkningsvekstrate,
                NSY_snitt,
                snitt_Reduk_rate_naturres,
                snitt_Vekstrate_årlig_invest, 
                snitt_Vekstrate_årlig_export, 
                Utdanning_snitt) %>% 
  dplyr::rename("Gjennomsnitlig årlig vekstrate i BNP pc 2000-2019 (%)"="snitt_vekst_BNP_pr_innb",
                "Gjennomsnittlig årlig vekst i arbeidskraft (%)"="årlig_vekstrate_n_snitt",
                "Gjennomsnittlig årlig befolkningsvekst (%)"="snitt_Befolkningsvekstrate",
                "Gjennomsnittlig årlig sparerate (%)"="NSY_snitt",
                "Gjennomsnittlig årlig reduksjon i naturresurser (%)"="snitt_Reduk_rate_naturres",
                "Gjennomsnittlig årlig vekst i Investeringer (%)"="snitt_Vekstrate_årlig_invest", 
                "Gjennomsnittlig årlig vekst i Eksport (%)"="snitt_Vekstrate_årlig_export",
                "Gjennomsnittlig år på skole (år)"="Utdanning_snitt") %>% 
  as.data.frame()

# skalerer variabler med lapply for boxplot

D_T_list2 <- lapply(D_T2, na.omit)
D_T_list2 <- lapply(D_T_list2, scale)

tomme_kol <- function(x) " "

datasummary(All(D_T2) ~ N + Mean + Min + Max + SD + Heading("Boxplot") * tomme_kol,
            data = D_T2, output = "kableExtra",
            title = 'Deskriptiv Statistisk ') %>% 
  column_spec(column = 7, image = spec_boxplot(D_T_list2)) %>%
  row_spec(row = 0, 
           font_size = 18) %>% 
  kable_paper(full_width = F) %>% 
  footnote("Inneholder kun land med data for perioden 2000-2019 etter ekstremverdier er fjernet (WDI)")
 
######################################################

Analyse<-Analyse %>% 
  group_by(income)

Lin_reg_BNP_modeller <- list(
  Nivå_BNP = lm(data = Analyse, Nivå_BNP_pr_innb ~
                  snitt_Vekstrate_årlig_invest + 
                  NSY_snitt +
                  snitt_Befolkningsvekstrate),
  Åpen_BNP = lm(data = Analyse, snitt_vekst_BNP_pr_innb ~ 
                        NSY_snitt + 
                        årlig_vekstrate_n_snitt +
                        snitt_Befolkningsvekstrate +
                        Utdanning_snitt +
                        snitt_Vekstrate_årlig_invest +
                        snitt_Vekstrate_årlig_export -
                        snitt_Reduk_rate_naturres -
                        Nivå_BNP_pr_innb_Y0),
  Lukket_BNP <- lm(data = Analyse, snitt_vekst_BNP_pr_innb ~ 
                           NSY_snitt + 
                           årlig_vekstrate_n_snitt +
                           snitt_Befolkningsvekstrate +
                           Utdanning_snitt +
                           snitt_Vekstrate_årlig_invest +
                           snitt_Reduk_rate_naturres -
                           Nivå_BNP_pr_innb_Y0))

# Ved å sette vcov til "robust", vil modelsummary funksjonen kalkulere å vise robustheten av standard errors.
# R spesifiserer antall simuleringer brukt for å estimere robustheten til standard error, 
# jo flere simuleringer jo mer nøyaktig resultat.
# cluster spesifiserer at standard errorne skal blandes basert på "country".
# Dette kan være nyttig når observasjonene i hver gruppe er mer lik hverandre enn observasjoner i 
# andre grupper.

options(OutDec=",")
modelsummary(Lin_reg_BNP_modeller, output = "kableExtra",
             title = 'Resultat Analyse',
             vcov = "robust", R = 1000, cluster = "country",
             estimate = c("{estimate}{stars}"),
             statistic = c("conf.int",
                           "s.e. = {std.error}", 
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = c("nobs", "adj.r.squared"))

######################################################
################ PLOT NSY_snitt ######################
######################################################

lm_1_ln_plot<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, NSY_snitt, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "ln BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Årlig Sparerate (NSY)",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


lm_1_vekst_plot<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, NSY_snitt, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Årlig Sparerate (NSY)",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot1 <- textGrob("Nivå og gj.snittlig vekst i BNP per innbygger VS Gj.snittlig årlig Sparerate", gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot1<-ggarrange(lm_1_ln_plot, lm_1_vekst_plot, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot1, top = text_grob("ln og Gj.snittlig vekst i BNP per Innbygger VS Gj.snittlig Årlig Sparerate", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))

######################################################
################ PLOT UTDANNING ######################
######################################################

lm_1_ln_plot<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, Utdanning_snitt, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "ln BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="År Skole (gj.snitt)",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


lm_1_vekst_plot<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, Utdanning_snitt, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="År Skole (gj.snitt)",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot1 <- textGrob("ln og Gj.snittlig vekst i BNP per innbygger VS Gj.snittlig Årlig Sparerate", gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot1<-ggarrange(lm_1_ln_plot, lm_1_vekst_plot, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot1, top = text_grob("ln og Gj.snittlig vekst i BNP per Innbygger VS Gj.snittlig År på Skole", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))

############################################################
###################### PLOT NATURRESSURSER #################
############################################################
plot_natur<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, sqrt(snitt_Reduk_rate_naturres), color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Nivå på BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Reduksjon naturressurser",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


plot_natur_vekst<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, sqrt(snitt_Reduk_rate_naturres), color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Reduksjon Naturressurser",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot3 <- textGrob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig Reduksjon Naturressurser", 
                        gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot3<-ggarrange(plot_natur, plot_natur_vekst, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot3, top = text_grob("Nivå og Vekst i BNP per Innbygger VS Gj.snittlig Reduksjon Naturressurser", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))


############################################################
###################### PLOT INVESTERINGER ##################
############################################################
plot_invest<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, snitt_Vekstrate_årlig_invest, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Nivå på BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Investeringer (%)",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


plot_vekst_invest<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, snitt_Vekstrate_årlig_invest, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Investeringer (%)",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot4 <- textGrob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig Årlig Investeringer", 
                        gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot4<-ggarrange(plot_invest, plot_vekst_invest, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot4, top = text_grob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig Årlig Investeringer", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))


############################################################
###################### PLOT EKSPORT ########################
############################################################
plot_eksport<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, snitt_Vekstrate_årlig_export, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Nivå på BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Eksport (%)",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


plot_vekst_eksport<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, snitt_Vekstrate_årlig_export, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Eksport",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot5 <- textGrob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig Årlig Investeringer", 
                        gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot5<-ggarrange(plot_eksport, plot_vekst_eksport, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot5, top = text_grob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig Årlig Eksport", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))


###############################################################
###################### PLOT BEFOLKNINGSVEKST ##################
###############################################################
plot_bef<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, snitt_Befolkningsvekstrate, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Nivå på BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Befolkningsvekst",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


plot_vekst_bef<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, snitt_Befolkningsvekstrate, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Befolkningsvekst",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot6 <- textGrob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig Befolkningsvekstrate", 
                        gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot6<-ggarrange(plot_bef, plot_vekst_bef, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot6, top = text_grob("Nivå og Vekst i BNP per Innbygger VS Gj.snittlig Befolkningsvekstrate", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))


############################################################
###################### PLOT ARB.KRAFT ######################
############################################################
plot_n<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, årlig_vekstrate_n_snitt, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Nivå på BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Arbeidskraft",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


plot_vekst_n<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, årlig_vekstrate_n_snitt, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Arbeidskraft",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot7 <- textGrob("Nivå og Vekst i BNP per innbygger VS Gj.snittlig vekst i Arbeidskraft", 
                        gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot7<-ggarrange(plot_n, plot_vekst_n, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot7, top = text_grob("Nivå og Vekst i BNP per Innbygger VS Gj.snittlig vekst i Arbeidskraft", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))


##############################################################
###################### PLOT NIVÅ BNP Y0 ######################
##############################################################
plot_y0<-ggplot(Analyse, aes(Nivå_BNP_pr_innb, Nivå_BNP_pr_innb_Y0, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Nivå på BNP per innbygger",
       subtitle = "- Gruppert på region og total populasjon",
       y="Y0",
       x="BNP per innbygger",
       color="Region")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = -0.05,vjust = -1, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.position = "none",
        legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8, hjust = 0.3, family = "LM Roman 10", color = "gray25"))


plot_vekst_y0<-ggplot(Analyse, aes(snitt_vekst_BNP_pr_innb, Nivå_BNP_pr_innb_Y0, color=region))+
  geom_point(aes(size=Total_pop), alpha=0.6)+
  scale_color_manual(values = c("aquamarine2","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan","dodgerblue3","deeppink4")) +
  labs(title = "Vekst i BNP per Innbygger",
       subtitle = "",
       y="Y0",
       x="Vekst BNP per innbygger",
       color="")+
  scale_size_area(guide = "none", max_size = 14) +
  geom_smooth(method=lm, lwd = 0.8, linetype= 5, se = FALSE, fullrange = TRUE, color = alpha("gray25", 0.60))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=12),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "hotpink4", size=10),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4"))+
  theme(legend.text = element_text(size = 5, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=8,hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        legend.position = "bottom")

title_plot8 <- textGrob("Nivå og Vekst i BNP per innbygger VS Nivå BNP år 2000", 
                        gp=gpar(fontsize=12, fontface="italic", fontfamily="LM Roman 10", color ="black"))


plot8<-ggarrange(plot_y0, plot_vekst_y0, ncol = 2, common.legend = TRUE, legend = "bottom")

annotate_figure(plot8, top = text_grob("Nivå og Vekst i BNP per Innbygger VS Nivå BNP år 2000", 
                                       color = "black", face = "italic", size = 14, family = "LM Roman 10"))


