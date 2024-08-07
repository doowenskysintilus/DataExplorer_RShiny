# DataExplorer-
## Description

Cette application Shiny en R est conçue pour offrir un tableau de bord interactif pour l'exploration, la visualisation et l'analyse de données. Elle permet de charger des ensembles de données, de créer des visualisations interactives et d'appliquer des modèles d'apprentissage automatique.

## Fonctionnalités

- **Téléversement de Données** : Importez vos ensembles de données pour les analyser.
- **Visualisation Interactive** : Créez des graphiques interactifs avec `ggplot2` et `plotly`.
- **Analyse Statistique** : Réalisez des analyses statistiques avec des bibliothèques comme `psych` et visualisez les corrélations avec `corrplot`.
- **Apprentissage Automatique** : Entraînez et évaluez des modèles de machine learning en utilisant `randomForest`, `caret` et `e1071`.
- **Graphiques ROC et PDP** : Visualisez les courbes ROC et les graphiques de dépendance partielle avec `pROC` et `pdp`.

## Installation

Pour exécuter cette application, vous devez avoir R et les packages suivants installés. Vous pouvez les installer en exécutant :

```r
install.packages(c(
  "shiny", "DT", "dplyr", "ggplot2", "plotly", "e1071", "rpart", 
  "psych", "corrplot", "caret", "pROC", "ROCR", "pdp", "randomForest", 
  "shinyjs", "shinyWidgets", "shinydashboard"
))
