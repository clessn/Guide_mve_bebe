# CLEANING GUIDE_MVE_BEBE

# Packages --------------------------------------------------------------------

library(dplyr)
library(tidyverse)
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
# Raw data --------------------------------------------------------------------

Raw <- read.csv("_SharedFolder_Guide_mve/data/INSPQ-guide-mve_RAW.csv") %>% 
  slice(-c(1,2)) %>%
  filter(code == "complete") %>%
  select(-StartDate, -EndDate, -Status, -IPAddress, -Progress,
         -Duration..in.seconds., -Finished, -RecordedDate, -ResponseId,
         -RecipientLastName, -RecipientFirstName, -RecipientEmail,
         -ExternalReference, -LocationLatitude, -LocationLongitude,
         -DistributionChannel, -UserLanguage, -consent_2_1, -consent_3_1, -PID)

# Clean data ------------------------------------------------------------------

Clean <- data.frame(
 ID = c(1:nrow(Raw))
 ) 
# Names -----------------------------------------------------------------------

names(Raw)

# Block 1 - Filter ------------------------------------------------------------

table(Raw$filter_1)

Clean$filt_preg <- NA
Clean$filt_preg[Raw$filter_1 == "Non"] <- 0
Clean$filt_preg[Raw$filter_1 == "Oui"] <- 1
table(Clean$filt_preg)

table(Raw$ses_pregnant_weeks_1)

Clean$ses_week <- NA
Clean$ses_week <- Raw$ses_pregnant_weeks_1
table(Clean$filt_week)

table(Raw$filter_2)

Clean$filt_1stkid <- NA
Clean$filt_1stkid[Raw$filter_2 == "Non"] <- 0
Clean$filt_1stkid[Raw$filter_2 == "Oui"] <- 1
table(Clean$filt_1stkid)

table(Raw$filter_3)

Clean$filt_parent <- NA
Clean$filt_parent[Raw$filter_3 == "Non"] <- 0
Clean$filt_parent[Raw$filter_3 == "Oui"] <- 1
table(Clean$filt_parent)

# Block 2 - information -------------------------------------------------------

table(Raw$info_1)

Clean$info_1 <- NA
Clean$info_1[Raw$info_1 == "Je ne sais pas/je préfère ne pas répondre"] <- NA
Clean$info_1[Raw$info_1 == "Jamais"] <- 0
Clean$info_1[Raw$info_1 == "Rarement"] <- 0.25
Clean$info_1[Raw$info_1 == "Quelques fois"] <- 0.5
Clean$info_1[Raw$info_1 == "Souvent"] <- 0.75
Clean$info_1[Raw$info_1 == "Très souvent"] <- 1
table(Clean$info_1)

table(Raw$info_2)

Clean$info_2 <- NA
Clean$info_2[Raw$info_2 == "Je ne sais pas/je préfère ne pas répondre"] <- NA
Clean$info_2[Raw$info_2 == "Jamais"] <- 0
Clean$info_2[Raw$info_2 == "Rarement"] <- 0.25
Clean$info_2[Raw$info_2 == "Quelques fois"] <- 0.5
Clean$info_2[Raw$info_2 == "Souvent"] <- 0.75
Clean$info_2[Raw$info_2 == "Très souvent"] <- 1
table(Clean$info_2)

# !!!!!!! À FAIRE !!!!!!!
table(Raw$info_3_1)
table(Raw$info_3_2)
table(Raw$info_3_3)
# !!!!!!!         !!!!!!!

table(Raw$info_4)

Clean$info_4_accessible <- 0
Clean$info_4_accessible[Raw$info_4 == "Accessible en tout temps"] <- 1
Clean$info_4_audiovisuel <- 0
Clean$info_4_audiovisuel[Raw$info_4 == "Des éléments audiovisuels attrayants qui aident à la compréhension"] <- 1
Clean$info_4_comprendfacile <- 0
Clean$info_4_comprendfacile[Raw$info_4 == "Des informations utiles et faciles à comprendre"] <- 1
Clean$info_4_consultefacile <- 0
Clean$info_4_consultefacile[Raw$info_4 == "Facile à consulter"] <- 1
Clean$info_4_transportfacile <- 0
Clean$info_4_transportfacile[Raw$info_4 == "Facile à transporter"] <- 1

table(Raw$info_4_6_TEXT)

Clean$info_4_fiable <- 0
Clean$info_4_fiable[Raw$info_4_6_TEXT == "c'était le plus pratique et fiable pour moi"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Fiabilité"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Fiabilité des sources"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Fiable"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Informations fiables (surtout les 2 premiers)"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Source que je considère fiable"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Sources d’informations fiables et scientifiques"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Sources fiables"] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Source fiable "] <- 1
Clean$info_4_fiable[Raw$info_4_6_TEXT == "Source fiable"] <- 1
Clean$info_4_concrète <- 0
Clean$info_4_concrète[Raw$info_4_6_TEXT == "Besoin d’etendre des expériences concrètes"] <- 1
Clean$info_4_expert <- 0
Clean$info_4_expert[Raw$info_4_6_TEXT == "Expertise "] <- 1
Clean$info_4_spécif <- 0
Clean$info_4_spécif[Raw$info_4_6_TEXT == "Information supplémentaire spécifique pour grossesse gémélaire "] <- 1
Clean$info_4_connaissance <- 0
Clean$info_4_connaissance[Raw$info_4_6_TEXT == "Leur connaissance "] <- 1
Clean$info_4_rdv <- 0
Clean$info_4_rdv[Raw$info_4_6_TEXT == "Lors des rendez-vous de suivi "] <- 1
Clean$info_4_sur <- 0
Clean$info_4_sur[Raw$info_4_6_TEXT == "Sources sure"] <- 1
Clean$info_4_tous <- 0
Clean$info_4_tous[Raw$info_4_6_TEXT == "Tous"] <- 1

# !!!!!!! À FAIRE !!!!! 
table(Raw$info_5_1)
table(Raw$info_5_2)
table(Raw$info_5_3)
# !!!!!!! À FAIRE !!!!!

table(Raw$info_6)

Clean$info_6_accessible <- 0
Clean$info_6_accessible[Raw$info_6 == "Accessible en tout temps"] <- 1
Clean$info_6_audiovisuel <- 0
Clean$info_6_audiovisuel[Raw$info_6 == "Des éléments audiovisuels attrayants qui aident à la compréhension"] <- 1
Clean$info_6_comprendfacile <- 0
Clean$info_6_comprendfacile[Raw$info_6 == "Des informations utiles et faciles à comprendre"] <- 1
Clean$info_6_consultefacile <- 0
Clean$info_6_consultefacile[Raw$info_6 == "Facile à consulter"] <- 1
Clean$info_6_transportfacile <- 0
Clean$info_6_transportfacile[Raw$info_6 == "Facile à transporter"] <- 1

table(Raw$info_6_6_TEXT)

Clean$info_6_fiable <- 0
Clean$info_6_fiable[Raw$info_6_6_TEXT == "Source fiable"] <- 1

# Block 3 - Guide MVE ---------------------------------------------------------

table(Raw$Guide_1)

Clean$guide_connaitre <- NA
Clean$guide_connaitre[Raw$Guide_1 == "Non"] <- 0
Clean$guide_connaitre[Raw$Guide_1 == "Oui"] <- 1
table(Clean$guide_connaitre)

table(Raw$guide_format)

Clean$guide_formatpapierweb <- 0
Clean$guide_formatpapierweb[Raw$guide_format == "En papier et en web"] <- 1
Clean$guide_formatpapier <- 0
Clean$guide_formatpapier[Raw$guide_format == "Seulement le format papier"] <- 1
Clean$guide_formatweb <- 0
Clean$guide_formatweb[Raw$guide_format == "Seulement en web"] <- 1

table(Raw$guide_use_1)

Clean$guide_use_1 <- NA
Clean$guide_use_1[Raw$guide_use_1 == "Non"] <- 0
Clean$guide_use_1[Raw$guide_use_1 == "Oui"] <- 1

table(Raw$Guide_use_2)

Clean$guide_use_2tardif <- 0
Clean$guide_use_2tardif[Raw$Guide_use_2 == "Distribution trop tardive"] <- 1
Clean$guide_use_2inutile <- 0
Clean$guide_use_2inutile[Raw$Guide_use_2 == "Inutile ou pas besoin de ces informations"] <- 1
Clean$guide_use_2pasremarqué <- 0
Clean$guide_use_2pasremarqué[Raw$Guide_use_2 == "N’a pas remarqué qu’il y avait des informations qui pourraient être utiles"] <- 1
Clean$guide_use_2gros <- 0
Clean$guide_use_2gros[Raw$Guide_use_2 == "Trop gros format (décourageant)"] <- 1
Clean$guide_use_2compliqué <- 0
Clean$guide_use_2compliqué[Raw$Guide_use_2 == "Trop compliqué à comprendre"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$Guide_use_2_6_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_reception_1)

Clean$guide_reception_web <- 0
Clean$guide_reception_web[Raw$guide_reception_1 =="On m’a dirigé vers le site web du guide"] <- 1
Clean$guide_reception_papier <- 0
Clean$guide_reception_papier[Raw$guide_reception_1 =="On m’a remise le format papier du guide"] <- 1
Clean$guide_reception_pdf <- 0
Clean$guide_reception_pdf[Raw$guide_reception_1 =="On m’a transféré le PDF du guide"] <- 1
Clean$guide_reception_autonome <- 0
Clean$guide_reception_autonome[Raw$guide_reception_1 =="On ne me l’a pas remis mais j’y ai accédé par mes propres moyens"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_reception_1_6_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_use_format)

Clean$guide_use_formatpapierweb <- 0
Clean$guide_use_formatpapierweb[Raw$guide_use_format == "Les deux formats -papier et web- de façon égale"] <- 1
Clean$guide_use_formatsolopaper <- 0
Clean$guide_use_formatsolopaper[Raw$guide_use_format == "Uniquement le format papier"] <- 1
Clean$guide_use_formatmostlypaper <- 0
Clean$guide_use_formatmostlypaper[Raw$guide_use_format == "Principalement le format papier"] <- 1
Clean$guide_use_formatsoloweb <- 0
Clean$guide_use_formatsoloweb[Raw$guide_use_format == "Uniquement le format web"] <- 1
Clean$guide_use_formatmostlyweb <- 0
Clean$guide_use_formatmostlyweb[Raw$guide_use_format == "Principalement le format web"] <- 1

table(Raw$guide_use_format_why)

# !!!!! À FAIRE !!!!!
table(Raw$guide_use_format_why_1_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_paper)

Clean$guide_paperfrequency <- NA
Clean$guide_paperfrequency[Raw$guide_paper == "Je ne sais pas/je préfère ne pas répondre"] <- NA
Clean$guide_paperfrequency[Raw$guide_paper == "Jamais"] <- 0
Clean$guide_paperfrequency[Raw$guide_paper == "Rarement"] <- 0.25
Clean$guide_paperfrequency[Raw$guide_paper == "Quelques fois"] <- 0.5
Clean$guide_paperfrequency[Raw$guide_paper == "Souvent"] <- 0.75
Clean$guide_paperfrequency[Raw$guide_paper == "Très souvent"] <- 1

table(Raw$guide_paper_prop)

Clean$guide_paper_prop <- NA
Clean$guide_paper_prop[Raw$guide_paper_prop == "Je ne sais pas/je préfère ne pas répondre"] <- NA
Clean$guide_paper_prop[Raw$guide_paper_prop == "Rien (0%)"] <- 0
Clean$guide_paper_prop[Raw$guide_paper_prop == "Quelques pages du guide (1-25%)"] <- 0.16
Clean$guide_paper_prop[Raw$guide_paper_prop == "Moins de la moitié (25-50%)"] <- 0.33
Clean$guide_paper_prop[Raw$guide_paper_prop == "La moitié du guide (50%)"] <- 0.5
Clean$guide_paper_prop[Raw$guide_paper_prop == "Plus de la moitié (50-75%)"] <- 0.66
Clean$guide_paper_prop[Raw$guide_paper_prop == "Presque tout le guide (75-99%)"] <- 0.83
Clean$guide_paper_prop[Raw$guide_paper_prop == "Le guide au complet (100%)"] <- 1

table(Raw$guide_paper_consult)

Clean$guide_paper_consultencadrés <- 0
Clean$guide_paper_consultencadrés[Raw$guide_paper_consult == "Consultation des encadrés"] <- 1
Clean$guide_paper_consultimages <- 0
Clean$guide_paper_consultimages[Raw$guide_paper_consult == "Consultation des photos, illustrations et messages-clés"] <- 1
Clean$guide_paper_consultprécise <- 0
Clean$guide_paper_consultprécise[Raw$guide_paper_consult == "Consultation ponctuelle, pour répondre à une question précise"] <- 1
Clean$guide_paper_consultsection <- 0
Clean$guide_paper_consultsection[Raw$guide_paper_consult == "Lecture continue d’une section"] <- 1
Clean$guide_paper_consultpassages <- 0
Clean$guide_paper_consultpassages[Raw$guide_paper_consult == "Lecture continue de certains passages"] <- 1
Clean$guide_paper_consultnot <- 0
Clean$guide_paper_consultnot[Raw$guide_paper_consult == "Je n’ai jamais consulté le format papier du guide"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_paper_consult_6_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_web_2)

Clean$guide_web_2paper <- 0
Clean$guide_web_2paper[Raw$guide_web_2 == "Adresse du site internet sur le format papier du guide"] <- 1
Clean$guide_web_2santé <- 0
Clean$guide_web_2santé[Raw$guide_web_2 == "Professionnel(le) de la santé"] <- 1
Clean$guide_web_2internet <- 0
Clean$guide_web_2internet[Raw$guide_web_2 == "Recherche sur internet"] <- 1
Clean$guide_web_2amis <- 0
Clean$guide_web_2amis[Raw$guide_web_2 == "Proches, amis, connaissances"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_web_2_5_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_web_1)

Clean$guide_web_1not <- 0
Clean$guide_web_1not[Raw$guide_web_1 == "Je n’utilise pas le format Web"] <- 1
Clean$guide_web_1html <- 0
Clean$guide_web_1html[Raw$guide_web_1 == "HTML"] <- 1
Clean$guide_web_1pdf <- 0
Clean$guide_web_1pdf[Raw$guide_web_1 == "PDF"] <- 1
Clean$guide_web_1both <- 0
Clean$guide_web_1both[Raw$guide_web_1 == "Les deux formats (PDF et HTML) de façon égale"] <- 1

table(Raw$guide_web)

Clean$guide_web_freq <- NA
Clean$guide_web_freq[Raw$guide_web == "Je ne sais pas/je préfère ne pas répondre"] <- NA
Clean$guide_web_freq[Raw$guide_web == "Non applicable"] <- NA
Clean$guide_web_freq[Raw$guide_web == "Jamais"] <- 0
Clean$guide_web_freq[Raw$guide_web == "Rarement"] <- 0.25
Clean$guide_web_freq[Raw$guide_web == "Quelques fois"] <- 0.5
Clean$guide_web_freq[Raw$guide_web == "Souvent"] <- 0.75
Clean$guide_web_freq[Raw$guide_web == "Très souvent"] <- 1

table(Raw$guide_web_prop)

Clean$guide_web_prop <- NA
Clean$guide_web_prop[Raw$guide_web_prop == "Je ne sais pas/je préfère ne pas répondre"] <- NA
Clean$guide_web_prop[Raw$guide_web_prop == "Rien (0%)"] <- 0
Clean$guide_web_prop[Raw$guide_web_prop == "Quelques pages du guide (1-25%)"] <- 0.16
Clean$guide_web_prop[Raw$guide_web_prop == "Moins de la moitié (25-50%)"] <- 0.33
Clean$guide_web_prop[Raw$guide_web_prop == "La moitié du guide (50%)"] <- 0.5
Clean$guide_web_prop[Raw$guide_web_prop == "Plus de la moitié (50-75%)"] <- 0.66
Clean$guide_web_prop[Raw$guide_web_prop == "Presque tout le guide (75-99%)"] <- 0.83
Clean$guide_web_prop[Raw$guide_web_prop == "Le guide au complet (100%)"] <- 1

table(Raw$guide_web_consult_1)

Clean$guide_web_consult_1encadrés <- 0
Clean$guide_web_consult_1encadrés[Raw$guide_web_consult_1 == "Consultation des encadrés"] <- 1
Clean$guide_web_consult_1images <- 0
Clean$guide_web_consult_1images[Raw$guide_web_consult_1 == "Consultation des photos, illustrations et messages-clés"] <- 1
Clean$guide_web_consult_1précise <- 0
Clean$guide_web_consult_1précise[Raw$guide_web_consult_1 == "Consultation ponctuelle, pour répondre à une question précise"] <- 1
Clean$guide_web_consult_1section <- 0
Clean$guide_web_consult_1section[Raw$guide_web_consult_1 == "Lecture continue d’une section"] <- 1
Clean$guide_web_consult_1passages <- 0
Clean$guide_web_consult_1passages[Raw$guide_web_consult_1 == "Lecture continue de certains passages"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_web_consult_1_6_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_web_consult_2)
table(Raw$guide_web_consult_2_5_TEXT)

Clean$guide_web_consult_2ordi <- 0
Clean$guide_web_consult_2ordi[Raw$guide_web_consult_2 == "Sur un ordinateur"] <- 1
Clean$guide_web_consult_2cell <- 0
Clean$guide_web_consult_2cell[Raw$guide_web_consult_2 == "Sur un cellulaire"] <- 1
Clean$guide_web_consult_2tablette <- 0
Clean$guide_web_consult_2tablette[Raw$guide_web_consult_2 == "Sur une tablette"] <- 1
Clean$guide_web_consult_2print <- 0
Clean$guide_web_consult_2print[Raw$guide_web_consult_2 == "Je l'imprime"] <- 1
Clean$guide_web_consult_2ordicell <- 0
Clean$guide_web_consult_2ordicell[Raw$guide_web_consult_2_5_TEXT == "Ordinateur et cellulaire"] <- 1

table(Raw$relatives_guide)

Clean$relatives_guidepartner <- 0
Clean$relatives_guidepartner[Raw$relatives_guide == "Autre parent/partenaire"] <- 1
Clean$relatives_guidegarde <- 0
Clean$relatives_guidegarde[Raw$relatives_guide == "Personne qui a la garde de l’enfant"] <- 1
Clean$relatives_guidegrandsparent <- 0
Clean$relatives_guidegrandsparent[Raw$relatives_guide == "Grands-parents"] <- 1
Clean$relatives_guidenot <- 0
Clean$relatives_guidenot[Raw$relatives_guide == "Personne d’autre ne le consulte"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$relatives_guide_5_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$relatives_format)

Clean$relatives_formatpapierweb <- 0
Clean$relatives_formatpapierweb[Raw$relatives_format == "Les deux formats -papier et web- de façon égale"] <- 1
Clean$relatives_formatsolopaper <- 0
Clean$relatives_formatsolopaper[Raw$relatives_format == "Uniquement le format papier"] <- 1
Clean$relatives_formatmostlypaper <- 0
Clean$relatives_formatmostlypaper[Raw$relatives_format == "Principalement le format papier"] <- 1
Clean$relatives_formatsoloweb <- 0
Clean$relatives_formatsoloweb[Raw$relatives_format == "Uniquement le format web"] <- 1
Clean$relatives_formatmostlyweb <- 0
Clean$relatives_formatmostlyweb[Raw$relatives_format == "Principalement le format web"] <- 1

table(Raw$relatives_web_format)

Clean$relatives_web_formathtml <- 0
Clean$relatives_web_formathtml[Raw$relatives_web_format == "HTML"] <- 1
Clean$relatives_web_formatpdf <- 0
Clean$relatives_web_formatpdf[Raw$relatives_web_format == "PDF"] <- 1
Clean$relatives_web_formatboth <- 0
Clean$relatives_web_formatboth[Raw$relatives_web_format == "Les deux formats (PDF et HTML) de façon égale"] <- 1

table(Raw$guide_format_ess)

Clean$guide_format_esspapierweb <- 0
Clean$guide_format_esspapierweb[Raw$guide_format_ess == "Les deux formats -papier et web- de façon égale"] <- 1
Clean$guide_format_esssolopaper <- 0
Clean$guide_format_esssolopaper[Raw$guide_format_ess == "Uniquement le format papier"] <- 1
Clean$guide_format_essmostlypaper <- 0
Clean$guide_format_essmostlypaper[Raw$guide_format_ess == "Principalement le format papier"] <- 1
Clean$guide_format_esssolowebpdf <- 0
Clean$guide_format_esssolowebpdf[Raw$guide_format_ess == "Uniquement le format web PDF"] <- 1
Clean$guide_format_esssolowebhtml <- 0
Clean$guide_format_esssolowebhtml[Raw$guide_format_ess == "Uniquement le format web HTML"] <- 1
Clean$guide_format_essmostlywebpdf <- 0
Clean$guide_format_essmostlywebpdf[Raw$guide_format_ess == "Principalement le format web PDF"] <- 1
Clean$guide_format_essmostlywebhtml <- 0
Clean$guide_format_essmostlywebhtml[Raw$guide_format_ess == "Principalement le format web HTML"] <- 1

table(Raw$guide_satisfaction_1)

Clean$guide_satisfaction_1paper <- NA
Clean$guide_satisfaction_1paper <- Raw$guide_satisfaction_1

table(Raw$guide_satisfaction_2)

Clean$guide_satisfaction_2webpdf <- NA
Clean$guide_satisfaction_2webpdf <- Raw$guide_satisfaction_2

table(Raw$guide_satisfaction_3)

Clean$guide_satisfaction_3webhtml <- NA
Clean$guide_satisfaction_3webhtml <- Raw$guide_satisfaction_3

table(Raw$guide_change)
# !!!!! À FAIRE !!!!!
table(Raw$guide_change_1_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_format_access)

Clean$guide_format_accesshtml <- 0
Clean$guide_format_accesshtml[Raw$guide_format_access == "Web HTML"] <- 1
Clean$guide_format_accesspdf <- 0
Clean$guide_format_accesspdf[Raw$guide_format_access == "Web PDF"] <- 1
Clean$guide_format_accesspaper <- 0
Clean$guide_format_accesspaper[Raw$guide_format_access == "Papier"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_format_access_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_format_prac)

Clean$guide_format_prachtml <- 0
Clean$guide_format_prachtml[Raw$guide_format_prac == "Web HTML"] <- 1
Clean$guide_format_pracpdf <- 0
Clean$guide_format_pracpdf[Raw$guide_format_prac == "Web PDF"] <- 1
Clean$guide_format_pracpaper <- 0
Clean$guide_format_pracpaper[Raw$guide_format_prac == "Papier"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_format_prac_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_paper_prac)

Clean$guide_paper_pracuse <- 0
Clean$guide_paper_pracuse[Raw$guide_paper_prac == "Facile à consulter"] <- 1
Clean$guide_paper_practransport <- 0
Clean$guide_paper_practransport[Raw$guide_paper_prac == "Facile à transporter"] <- 1
Clean$guide_paper_pracacess <- 0
Clean$guide_paper_pracacess[Raw$guide_paper_prac == "Accessible en tout temps"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_paper_prac_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_paper_dis)

Clean$guide_paper_disuse <- 0
Clean$guide_paper_disuse[Raw$guide_paper_dis == "Difficile à utiliser"] <- 1
Clean$guide_paper_distransport <- 0
Clean$guide_paper_distransport[Raw$guide_paper_dis == "Difficile à transporter"] <- 1
Clean$guide_paper_disacess <- 0
Clean$guide_paper_disacess[Raw$guide_paper_dis == "Accès difficile"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_paper_dis_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_format_change)

Clean$guide_format_change <- NA
Clean$guide_format_change[Raw$guide_format_change == "Non"] <- 0
Clean$guide_format_change[Raw$guide_format_change == "Oui"] <- 1

table(Raw$guide_pdf_prac)

Clean$guide_pdf_pracuse <- 0
Clean$guide_pdf_pracuse[Raw$guide_pdf_prac == "Facile à consulter"] <- 1
Clean$guide_pdf_practransport <- 0
Clean$guide_pdf_practransport[Raw$guide_pdf_prac == "Facile à transporter"] <- 1
Clean$guide_pdf_pracacess <- 0
Clean$guide_pdf_pracacess[Raw$guide_pdf_prac == "Accessible en tout temps"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_pdf_prac_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_html_prac)

Clean$guide_html_pracuse <- 0
Clean$guide_html_pracuse[Raw$guide_html_prac == "Facile à consulter"] <- 1
Clean$guide_html_practransport <- 0
Clean$guide_html_practransport[Raw$guide_html_prac == "Facile à transporter"] <- 1
Clean$guide_html_pracacess <- 0
Clean$guide_html_pracacess[Raw$guide_html_prac == "Accessible en tout temps"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_html_prac_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_pdf_dis)

Clean$guide_pdf_disuse <- 0
Clean$guide_pdf_disuse[Raw$guide_pdf_dis == "Difficile à utiliser"] <- 1
Clean$guide_pdf_distransport <- 0
Clean$guide_pdf_distransport[Raw$guide_pdf_dis == "Difficile à transporter"] <- 1
Clean$guide_pdf_disacess <- 0
Clean$guide_pdf_disacess[Raw$guide_pdf_dis == "Accès difficile"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_pdf_dis_4_TEXT)
# !!!!! À FAIRE !!!!!

table(Raw$guide_html_dis)

Clean$guide_html_disuse <- 0
Clean$guide_html_disuse[Raw$guide_html_dis == "Difficile à utiliser"] <- 1
Clean$guide_html_distransport <- 0
Clean$guide_html_distransport[Raw$guide_html_dis == "Difficile à transporter"] <- 1
Clean$guide_html_disacess <- 0
Clean$guide_html_disacess[Raw$guide_html_dis == "Accès difficile"] <- 1

# !!!!! À FAIRE !!!!!
table(Raw$guide_html_dis_4_TEXT)
# !!!!! À FAIRE !!!!!

# !!!!! À FAIRE !!!!!
table(Raw$comments)
# !!!!! À FAIRE !!!!!


# Block 4 - SES ---------------------------------------------------------------