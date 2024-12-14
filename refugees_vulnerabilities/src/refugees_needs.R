## Loading libraries 

library(VIM)
library(mice)
library(corrplot)
library(ggplot2)
library(rio)
library(moments) # calculating statistics
library(forcats)  # editing factors
library(gridExtra) # plotting
library(RColorBrewer) # plotting
library(rsample)     # data splitting       
library(Metrics) # calculating RMSE
library(tidyverse)
#Loading data


data <- import("needs_analysis_refugees_chad.csv")


glimpse(data)
#summary(data)
View(data)



# Droping unused columns and creating factors

cleaned_data <- data %>% 
  mutate_if(is.character, ~ as.factor(tolower(.))) %>%  # Convertir en minuscules, puis en facteur
  mutate(
    Age = as.integer(as.character(Age)),                # Convertir Age en entier
    Revenu_Mensuel = as.integer(as.character(Revenu_Mensuel))  # Convertir Revenu_Mensuel en entier
  ) %>% 
  select(-c(ID))  # Supprimer la colonne ID


glimpse(cleaned_data)

# Columns containing ID, salary_currency and salary were omited. It is hard to compare salaries
# in different currencies, so that columns are not statistically important. Furthermore strings
# were transformed to factors which now are messy and hard to understand at first glance.


# Renaming and reordering factors 
cleaned_data <- cleaned_data %>% 
  mutate(Education = recode(Education, 
                                  "Primaire" = "prim", 
                                  "Secondaire" = "sec", 
                                  "Universitaire" = "univ")) %>% 
  mutate(Type_Logement = recode(Type_Logement, 
                                   "Appartement" = "appart", 
                                   "Sans_abri" = "sans_abri", 
                                   "Refuge" = "refuge"))
glimpse(cleaned_data)

#Levels of factors were changed to more user-friendly values which now are better to understand correctly.

##Statistical summary of data

#We can now look at summary of variables in data

#Summarizing data 

cleaned_data %>% 
  summary()


#Detection et suppression des doublons

cleaned_data <- cleaned_data[!duplicated(cleaned_data), ]

cleaned_data %>% 
  summary()

# Data contains record from years 2020-2022 with mostly senior and mid-level workers. Nearly 
# all of them are on full-time contract. Most job positions are cumulated in US medium 
# companies with remote workers. Top three job postitions are Data Scientist, Data Engineer and 
# Data Analyst. Only integer variable is now salary which mean is 112298, we can see from 1st and 3rd uanitile that
# this data will very likely have outliers on both sides. Let's take closer look at salary data.

#IMPUTATIONS DES VALEURS MANQUANTES
bw <- 2 * IQR(data_imp$Revenu_Mensuel) / length(data_imp$Revenu_Mensuel)^(1/3)

hist_data <- cleaned_data%>% 
  ggplot( mapping= aes(x = Revenu_Mensuel)) +   
  geom_histogram(binwidth = bw,color = "#000000", fill = "#0099F8", alpha = 0.6) +
  theme_bw()

box_data <- cleaned_data%>% 
  ggplot(mapping= aes(x = Revenu_Mensuel)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  theme_bw()

grid.arrange(hist_data, box_data, bar_data, ncol = 3, nrow = 2)

#Histogram bins were calculated from Freedman–Diaconis rule formula. We can 
#clearly see from both plots, that data does not have outliers which can influence our
#research. 


#Confirmation des visualisations avec des tests statistiques
shapiro.test(cleaned_data$Age)  
qqnorm(cleaned_data$Age)
qqline(cleaned_data$Age, col = "blue")

shapiro.test(cleaned_data$Revenu_Mensuel)  #It shows that Age is not normally distributed
qqnorm(cleaned_data$Revenu_Mensuel)
qqline(cleaned_data$Revenu_Mensuel, col = "red")

#Plot of Etat de sante
bar_data <- data_imp%>% 
  ggplot(mapping= aes(x = Etat_Sante)) + 
  geom_bar(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  theme_bw()
bar_data


#We can use pmm as method to fit the model
#Visualisation des distributions des valeurs manquantes
aggr(cleaned_data)

#Nombre des missings par variables
colSums(is.na(cleaned_data))

# The graphs show that missings are at random

#Verification des patterns concernant les missing
md.pattern(cleaned_data)

#imputation des valeurs manquantes avec mice

imp <- mice(cleaned_data, method = c("pmm", "polyreg", "polyreg", "polyreg", "pmm", "polyreg", "polyreg"), m = 5)
attributes(imp)
data_imp <- complete(imp)

#Verification des imputations faites avec succes
aggr(data_imp)

#Verifier qu'il n'y a plus de missing
colSums(is.na(data_imp))

#On verifie que les imputations ne deforment pas les distributions initiales

#Par graphique
par(mfrow =c(2,2))
boxplot(cleaned_data$Revenu_Mensuel, main="Revenu with NA")
boxplot(data_imp$Revenu_Mensuel, main="Revenu without NA")

plot(density(log(cleaned_data$Revenu_Mensuel), na.rm = TRUE), main= "Revenus: original vs imputed")
lines(density(log(data_imp$Revenu_Mensuel), na.rm = TRUE), col ="red", lty =3)

plot(density(cleaned_data$Age, na.rm = TRUE), main= "Age: original vs imputed")
lines(density(data_imp$Age, na.rm = TRUE), col ="red", lty =3)

#Confirmation avec une analyse statistique
t.test(cleaned_data$Revenu_Mensuel, data_imp$Revenu_Mensuel)
t.test(cleaned_data$Age, data_imp$Age)


#Visualisation des distributions des variables Age et Revenu_mensuel
#hist(data$Age)




#Harmoniser les valeurs: format, casse, espace

data_imp$Sexe <- trimws(tolower(data_imp$Sexe))
data_imp$Situation_Matrimoniale <- trimws(tolower(data_imp$Situation_Matrimoniale))
data_imp$Education <- trimws(tolower(data_imp$Education))
data_imp$Type_Logement <- trimws(tolower(data_imp$Type_Logement))
data_imp$Etat_Sante <- trimws(tolower(data_imp$Etat_Sante))

#sauvegarde des donnees
write.csv(data_imp, "data_refugees_cleaned.csv", row.names = FALSE)


####ANALYSE DES DONNEES################################################################


# Revenu mensuel en fonction du niveau d'education et du sexe
    #Boite a moustaches
 #dev.off()


#ANALYSE MULTIVARIEE


#Plot of salary year by year
data_imp%>% 
  group_by(work_year)%>% 
  summarize( 
    median_salary = median(salary_in_usd, na.rm = TRUE))%>% 
  ggplot(mapping = aes(x=work_year, y =median_salary ))+ 
  geom_line()+ 
  theme_bw()+ labs(y = "Salary Median (USD)", x = "Year", title = "Median of salary in Data related jobs in last three years")


  ggplot(data_imp, aes(x = Education, y = Revenu_Mensuel, fill = Sexe)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Revenu mensuel par niveau d'éducation",
       x = "Niveau d'éducation",
       y = "Revenu mensuel")
    
    # Violin
    ggplot(data_imp, aes(x = Education, y = Revenu_Mensuel, fill = Sexe)) +
    geom_violin() +
    theme_minimal() +
    labs(title = "Boxplot de Revenu mensuel par niveau d'éducation",
         x = "Niveau d'éducation",
         y = "Revenu mensuel")

# Relations entre les variables numeriques
    #Encodage de la variable Education pour l'inclure dans la correlation
     data_imp$Education_encoded <- as.integer(data_imp$Education)
     
    # Extraction uniquement les colonnes numériques
     numeric_data <- data_imp[, sapply(data_imp, is.numeric)]  
    
     # Tests de correlation entre les variables numeriques
     correlation_matrix <- cor(numeric_data, use = "complete.obs")  
    
      # Visualisation graphique des relations: heatmap

     corrplot(correlation_matrix, method = "circle", type = "lower", tl.cex = 0.8)



####ANALYSE DES VULNERABILITES
   
  # Definition des vulnerabilites
  # data_imp$vulnerable_revenu <- ifelse(data_imp$Revenu_Mensuel < 10000, 1, 0)
  # data_imp$vulnerable_logement <- ifelse(data_imp$Type_Logement %in% c("sans_abri", "refuge"), 1, 0)
  # data_imp$vulnerable_sante <- ifelse(data_imp$Etat_Sante %in% c("mauvais"), 1, 0)
  # data_imp$vulnerable_education <- ifelse(data_imp$Education %in% c("aucun", "primaire"), 1, 0)
     
     data_imp <- data_imp %>%
       mutate(
         vulnerable_revenu = if_else(Revenu_Mensuel < 10000, 1, 0),
         vulnerable_logement = if_else(Type_Logement %in% c("sans_abri", "refuge"), 1, 0),
         vulnerable_sante = if_else(Etat_Sante %in% c("mauvais"), 1, 0),
         vulnerable_education = if_else(Education %in% c("aucun", "primaire"), 1, 0)
       )
     
     
  
  #Calcul du score global de vulnerabilite
     data_imp <- data_imp %>%
       mutate(
         score_vulnerabilite = rowSums(select(., vulnerable_revenu, vulnerable_logement, vulnerable_sante, vulnerable_education), na.rm = TRUE)
       )
     
     shapiro.test(data_imp$score)  
     
  ## Créer une colonne 'vulnerable' qui marque les individus vulnérables
     data_imp <- data_imp %>%
       mutate(vulnerable = if_else(score_vulnerabilite >= 2 , "Vulnérable", "Non vulnérable"))
     
  #Résumez les scores par camp ou population étudiée
     aggregated_data <- data_imp %>%
       group_by(Sexe, Situation_Matrimoniale) %>%
       summarize(median_score_vulnerabilite = median(score_vulnerabilite, na.rm = TRUE), .groups = "drop")
     
  # Visualiser les vulnérabilités
     ggplot(data_imp, aes(x = Situation_Matrimoniale, fill = vulnerable)) +
       geom_bar(position = "dodge") +
       theme_minimal() +
       labs(title = "Vulnérabilité par situation matrimoniale",
            x = "Situation Matrimoniale", 
            y = "Nombre de personnes vulnérables")
  
  #observer la dispersion des scores de vulnérabilité.
     ggplot(data_imp, aes(x = Situation_Matrimoniale, y = score_vulnerabilite, fill = Sexe)) +
       geom_boxplot() +
       theme_minimal() +
       labs(title = "Distribution des scores de vulnérabilité par sexe et situation matrimoniale",
            x = "Situation Matrimoniale", 
            y = "Score de vulnérabilité")
     
  #visualiser les interactions entre Sexe, Situation_Matrimoniale et les scores de vulnérabilité
     ggplot(aggregated_data, aes(x = Situation_Matrimoniale, y = Sexe, fill = median_score_vulnerabilite)) +
       geom_tile() +
       scale_fill_gradient(low = "white", high = "red") +
       theme_minimal() +
       labs(title = "Heatmap de la vulnérabilité par sexe et situation matrimoniale",
            x = "Situation Matrimoniale",
            y = "Sexe",
            fill = "Score de vulnérabilité")
     
  
    
    #Analyser les priorités des camps
    
    table(data_imp$Type_Logement)
    prop.table(table(data_imp$Type_Logement)) * 100
       #visualisations

    
    # Table des fréquences
    freq_table <- as.data.frame(table(data_imp$Type_Logement))
    
    # Diagramme en barres
    ggplot(freq_table, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 4) +
      theme_minimal() +
      labs(title = "Distribution des types de logement",
           x = "Type de logement",
           y = "Fréquence") +
      theme(legend.position = "none")
    
    
    #visualizations comparatives
  
    ggplot(data_imp, aes(x = Type_Logement, fill = Etat_Sante)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Comparaison du type de logement et de l'état de santé",
           x = "Type de Logement", y = "Fréquence")
    
    
    #revenu vs état de santé: Analyse croisée entre les variables 
    
    table(data_imp$Revenu_Mensuel, data_imp$Etat_Sante)
    
    #Visualisation de corrélations (numériques uniquement) :
      library(ggcorrplot)
      corr_matrix <- cor(data_imp[, c("Age", "Revenu_Mensuel", "score_vulnerabilite")], use = "complete.obs")
      ggcorrplot(corr_matrix, lab = TRUE)
      
    #Visualisation de relations catégoriques :
      ggplot(data_imp, aes(x = Situation_Matrimoniale, fill = Type_Logement)) +
      geom_bar(position = "fill") +
      labs(title = "Distribution des types de logement par situation matrimoniale")
    
    #Multivariable Regression Models    
        #Identification des facteurs influençant la vulnérabilité :
        model <- lm(score_vulnerabilite ~ Age + Sexe + Education + Revenu_Mensuel + Type_Logement, data = data_imp)
        summary(model)
        
        
      