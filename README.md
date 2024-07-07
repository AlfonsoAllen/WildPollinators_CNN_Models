# PollinatorCNNModels

## Overview

This repository contains the code to obtain the results of the deep learning models for the technical paper entitled "Convolutional Neural Networks Can Improve Pollination Supply Models at Global Scale" by Alfonso Allen-Perkins, Ángel Giménez-García, Ainhoa Magrach, Javier Galeano, Ana María Tarquis, and Ignasi Bartomeus.

## Data Sources

The data used includes:
- Visitation rates: `Data/CropPol_field_level_data.csv`
- Extracts from the Copernicus maps at the analyzed sites: `Data/Copernicus_maps`
- Input parameters for the ML models by García-Giménez et al. (2023): `Data/ml_global.csv`
- Cross-validation partitions in García-Giménez et al. (2023): `Data/CV_folds.csv`

## Training and Validation of the CNN Model
To train the CNN model, run the following script: `Code/DL_models_and_features/1_Train_CNN_regressor_global_scale.ipynb`

To evaluate predictions at the validation points using the CNN model, run the following scripts: 
- `Code/DL_models_and_features/2_CNN_results_and_datasets_update.ipynb`
- `Code/DL_models_and_features/3_Suppl_figE1.ipynb`

## Modeling Pixel Importance
To explore the mechanistic rules used by the CNN model linking land cover composition and configuration to the pollination services of wild bees, run the following script: `R-scripts/compute_Lonsdorf_ecosistemas2023_JdA.R`

## Training and Validation of the NN Ensemble Model
To train the NN ensemble model, run the following script: `Code/DL_models_and_features/5_Train_Ensemble_regressor_global_scale.ipynb`

To evaluate predictions at the validation points using the NN ensemble, run the following script: `Code/DL_models_and_features/6_Plot_Fig1b.ipynb`

## References

Giménez-García, A., Allen-Perkins, A., Bartomeus, I., Balbi, S., Knapp, J. L., Hevia, V., Woodcock, B. A., Smagghe, G., Miñarro, M., Eeraerts, M., Colville, J. F., Hipólito, J., Cavigliasso, P., Nates-Parra, G., Herrera, J. M., Cusser, S., Simmons, B. I., Wolters, V., Jha, S., Freitas, B. M., Horgan, F. G., Artz, D. R., Sidhu, C. S., Otieno, M., Boreux, V., Biddinger, D. J., Klein, A.-M., Joshi, N. K., Stewart, R. I. A., Albrecht, M., Nicholson, C. C., O'Reilly, A. D., Crowder, D. W., Burns, K. L. W., Nabaes Jodar, D. N., Garibaldi, L. A., Sutter, L., Dupont, Y. L., Dalsgaard, B., da Encarnação Coutinho, J. G., Lázaro, A., Andersson, G. K. S., Raine, N. E., Krishnan, S., Dainese, M., van der Werf, W., Smith, H. G., and Magrach, A.: Pollination supply models from a local to global scale, Web Ecol., 23, 99–129, https://doi.org/10.5194/we-23-99-2023, 2023. [https://we.copernicus.org/articles/23/99/2023/](https://we.copernicus.org/articles/23/99/2023/).
