library(stringr)
library(dplyr)
library(rvest)

# initialisaiton du data.frame de sortie
fetch_l1_calendar=NULL

for (saison in c(19:84,100:102)) {
  #saison=20
  calendar=NULL
  
 
  # boucle sur les journee
  for (j in 1:38){
    #j=2
    #saison=100
    # creation de l'url de requete
   
    url <- paste0("http://www.lfp.fr/ligue1/competitionPluginCalendrierResultat/changeCalendrierJournee?sai=",saison,"&jour=",j)
    
    # extraction du code html
    site <- read_html(url)
    #save(site,file="site") 
    # nombre de jour dans la journee
    site %>% 
      html_node("body") %>%
      as.character() %>%
      str_count("<h4>") -> n_day
    
    if (n_day!=0){
      for (i in 1:n_day){
        
        # xpath match
        xpath<-paste0('//*[@id="tableaux_rencontres"]/div/table[',i,']')
        
        # xpath date
        xpath2<-paste0('//*[@id="tableaux_rencontres"]/div/h4[',i,']')
        
        # recuperation des matchs
        site %>% 
          html_node("body") %>%
          html_node(xpath=xpath) %>%
          html_table -> temp
        
        # recuperation de la date
        site %>% 
          html_node("body") %>%
          html_node(xpath=xpath2) %>%
          html_text -> temp2
        
        calendar <- rbind(calendar,cbind(j,temp2,temp))
        
      }
    }  
    } 
    
    # pour chaque jour recuperation de la date et des match
    
  
  # supression des variables inutiles
  calendar <- calendar[,c(1,2,3,4,6,8)]
  
  # renomage des colones
  colnames(calendar) <- c("journee","date","time","home","score","away")
  
  # separation du score en deux variables
  calendar <- cbind(calendar,str_split_fixed(calendar$score," - ",n = 2))
  
  # supression de l'ancienne colone de score
  #calendar <- calendar[,-5]
  
  # renomage des colones de scores
  colnames(calendar)[c(7,8)] <- c("home_score","away_score")
  calendar[,7]<-as.numeric(as.character(calendar[,7]))
  calendar[,8]<-as.numeric(as.character(calendar[,8]))
  calendar$saison<-saison
 
  
  fetch_l1_calendar=rbind(fetch_l1_calendar,calendar)
  print(saison)
}

#Rajout de la saison par année
ref_saison = fetch_l1_calendar %>%
  filter(journee==1) %>%
  group_by(saison) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(saison_annee=paste0(row_number()+1949,"/",row_number()+1950)) %>%
  select(saison,saison_annee)

#On joint et on remet les colonnes dans le bon ordre
fetch_l1_calendar_final= fetch_l1_calendar %>%
  left_join(ref_saison,by="saison") %>%
  select(saison_annee,saison,journee,date,time,home,score,away,home_score,away_score)

#On sauvegarde les fichiers
save(fetch_l1_calendar_final,file="/home/ldapusers/dflouriot/Analyse/Crawl_foot/Extraction_calendrier_L1.RData")
write.csv(fetch_l1_calendar_final,file="/home/ldapusers/dflouriot/Analyse/Crawl_foot/Extraction_calendrier_L1.csv")

