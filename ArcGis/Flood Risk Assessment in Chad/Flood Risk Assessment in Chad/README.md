# Flood Risk Assessment in Chad Using ArcGIS Online

## Project Overview

Ce projet porte sur l'évaluation des risques d'inondation dans différentes régions du Tchad à l'aide d'ArcGIS Online. L'objectif est d'analyser l'étendue des inondations, l'impact sur la population et les infrastructures critiques telles que les routes, les écoles et les hôpitaux. Ce projet illustre l'utilisation des Systèmes d'Information Géographique (SIG) et de l'analyse des données pour soutenir les efforts humanitaires dans les zones touchées par des catastrophes naturelles.

👉 [Voir la carte des zones inondées au Tchad](https://www.arcgis.com/home/webmap/viewer.html?webmap=ef8534c2922f4d949ca350ceecf33728)

---

## Objectives

1. Cartographier les zones touchées par les inondations au Tchad.
2. Évaluer l'impact des inondations sur la population et les infrastructures critiques.
3. Utiliser des outils d'analyse spatiale pour visualiser et analyser les données.
4. Créer une carte interactive pour une meilleure compréhension et prise de décision.

---

## Dataset

Le jeu de données utilisé dans ce projet contient les champs suivants :

- **Location** : Nom de la région.
- **Latitude** : Coordonnée de latitude de la région.
- **Longitude** : Coordonnée de longitude de la région.
- **Flood_Extent** : Étendue des inondations en kilomètres carrés.
- **Population** : Population de la région.
- **Roads** : Nombre de routes dans la région.
- **Schools** : Nombre d'écoles dans la région.
- **Hospitals** : Nombre d'hôpitaux dans la région.

### Example Dataset

```csv
Location,Latitude,Longitude,Flood_Extent,Population,Roads,Schools,Hospitals
N'Djamena,12.1348,15.0557,1.5,1000000,50,20,15
Moundou,8.5674,16.0858,3.0,150000,25,10,5
Sarh,9.1429,18.3923,2.0,120000,30,12,8
Abéché,13.8299,20.8324,0.5,75000,15,5,3
Kélo,9.3083,15.8066,4.0,60000,20,8,4
