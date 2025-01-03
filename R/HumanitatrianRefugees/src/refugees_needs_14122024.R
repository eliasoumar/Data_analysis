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

# Droping unused columns and creating factors

cleaned_data <- data %>% 
  mutate_if(is.character, ~ as.factor(tolower(.))) %>%  # Convertir en minuscules, puis en facteur
  mutate(
    Age = as.integer(as.character(Age)),                # Convertir Age en entier
    Revenu_Mensuel = as.integer(as.character(Revenu_Mensuel))  # Convertir Revenu_Mensuel en entier
  ) %>% 
  select(-c(ID))  # Supprimer la colonne ID


glimpse(cleaned_data)

# Columns containing ID is omited. Furthermore strings were transformed to factors.


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

cleaned_data %>% 
  summary()


#Detect and delete duplicates

cleaned_data <- cleaned_data[!duplicated(cleaned_data), ]

cleaned_data %>% 
  summary()

#. Let's take closer look at salary data.

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


# Statistic test to confirm the visuals
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


#Visualize missings values
aggr(cleaned_data)

#Missins by variables
colSums(is.na(cleaned_data))

# The graphs show that missings are at random

#Verify patterns among missing
md.pattern(cleaned_data)

#Missing values imputations using Mice

imp <- mice(cleaned_data, method = c("pmm", "polyreg", "polyreg", "polyreg", "pmm", "polyreg", "polyreg"), m = 5)
attributes(imp)
data_imp <- complete(imp)

#Check imputations are done successfully
aggr(data_imp)

#Verifier qu'il n'y a plus de missing
colSums(is.na(data_imp))

#Compare initial data vs imputed data

#Par graphique
par(mfrow =c(2,2))
boxplot(cleaned_data$Revenu_Mensuel, main="Revenu with NA")
boxplot(data_imp$Revenu_Mensuel, main="Revenu without NA")

plot(density(log(cleaned_data$Revenu_Mensuel), na.rm = TRUE), main= "Revenus: original vs imputed")
lines(density(log(data_imp$Revenu_Mensuel), na.rm = TRUE), col ="red", lty =3)

plot(density(cleaned_data$Age, na.rm = TRUE), main= "Age: original vs imputed")
lines(density(data_imp$Age, na.rm = TRUE), col ="red", lty =3)

# Statistic test to confirm
t.test(cleaned_data$Revenu_Mensuel, data_imp$Revenu_Mensuel)
t.test(cleaned_data$Age, data_imp$Age)



# Saving data
write.csv(data_imp, "data_refugees_cleaned.csv", row.names = FALSE)


####DATA ANALYSIS################################################################


 dev.off()


#ANALYSE MULTIVARIEE


#Plot of Revenu by Education and gender

 p1 <- ggplot(data_imp, aes(x = Education, y = Revenu_Mensuel, fill = Sexe)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot de Revenu mensuel par niveau d'éducation",
             x = "Niveau d'éducation",
             y = "Revenu mensuel")

# Relations between  variables
 
     
    # Between numerical variables
  
     numeric_data <- data_imp %>%
       select(where(is.numeric))
       
    
     # Correlation test
     correlation_matrix <- cor(numeric_data, use = "complete.obs")  
    
      # plot

     corrplot(correlation_matrix, method = "circle", type = "lower", tl.cex = 0.8)



####Vulnerabilities analysis
 
     data_imp <- data_imp %>%
       mutate(
         vulnerable_revenu = if_else(Revenu_Mensuel < 10000, 1, 0),
         vulnerable_logement = if_else(Type_Logement %in% c("sans_abri", "refuge"), 1, 0),
         vulnerable_sante = if_else(Etat_Sante %in% c("mauvais"), 1, 0),
         vulnerable_education = if_else(Education %in% c("aucun", "primaire"), 1, 0)
       )
     
     
  
  #Vulnerability global score
     data_imp <- data_imp %>%
       mutate(
         score_vulnerabilite = rowSums(select(., vulnerable_revenu, vulnerable_logement, vulnerable_sante, vulnerable_education), na.rm = TRUE)
       )
     
     shapiro.test(data_imp$score)  
     
  ## Variable : vulnerable
     data_imp <- data_imp %>%
       mutate(vulnerable = if_else(score_vulnerabilite >= 2 , "Vulnérable", "Non vulnérable"))
     
  # Aggregate by sexe and marital status
     aggregated_data <- data_imp %>%
       group_by(Sexe, Situation_Matrimoniale) %>%
       summarize(median_score_vulnerabilite = median(score_vulnerabilite, na.rm = TRUE), .groups = "drop")
     
  # plot
     ggplot(data_imp, aes(x = Situation_Matrimoniale, fill = vulnerable)) +
       geom_bar(position = "dodge") +
       theme_minimal() +
       labs(title = "Vulnérabilité par situation matrimoniale",
            x = "Situation Matrimoniale", 
            y = "Nombre de personnes vulnérables")
  
  #plot of vulnerability
     ggplot(data_imp, aes(x = Situation_Matrimoniale, y = score_vulnerabilite, fill = Sexe)) +
       geom_boxplot() +
       theme_minimal() +
       labs(title = "Distribution des scores de vulnérabilité par sexe et situation matrimoniale",
            x = "Situation Matrimoniale", 
            y = "Score de vulnérabilité")
     
  # Relations between Sexe, Situation_Matrimoniale and  scores de vulnérabilité
     ggplot(aggregated_data, aes(x = Situation_Matrimoniale, y = Sexe, fill = median_score_vulnerabilite)) +
       geom_tile() +
       scale_fill_gradient(low = "white", high = "red") +
       theme_minimal() +
       labs(title = "Heatmap de la vulnérabilité par sexe et situation matrimoniale",
            x = "Situation Matrimoniale",
            y = "Sexe",
            fill = "Score de vulnérabilité")
     
    
    #priority analysis
    
    table(data_imp$Type_Logement)
    prop.table(table(data_imp$Type_Logement)) * 100

    
    # Frequence table
    freq_table <- as.data.frame(table(data_imp$Type_Logement))
    
    # Plot
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
        
        
      