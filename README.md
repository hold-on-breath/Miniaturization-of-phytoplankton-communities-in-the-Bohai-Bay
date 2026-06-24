# Phytoplankton Miniaturization in Bohai Bay (2000–2020)

Code and data for the manuscript: **"Warming and Altered Nutrient Stoichiometry Drive Phytoplankton Miniaturization: A 20-Year Study in Bohai Bay, China"**

## Repository Structure

```
├── 代码/                              # R scripts (8 files)
│   ├── 01_environmental_trends.R      # Fig. 2: Temperature, nutrients, N/P, Si/N, Chl-a
│   ├── 02_phytoplankton_community.R   # Fig. 3 & 4: Community structure, dominance heatmap
│   ├── 03_esd_carbon_flux.R           # Fig. 5: ESD and carbon vertical flux
│   ├── 04_shap_analysis.R             # Fig. 6: SHAP importance analysis
│   ├── 05_correlation_heatmap.R       # Fig. 7: Pearson correlation heatmap
│   ├── 06_global_miniaturization.R    # Fig. 8: Global miniaturization map
│   ├── 07_alpha_diversity.R           # Fig. S1: Alpha diversity trends
│   └── 08_supplementary_models.R      # GAM, db-RDA, SEM, Random Forest
│
└── 数据/                              # Input data files (14 files)
    ├── raw_phytoplankton_counts.xlsx  # Raw species-by-station abundance matrix
    └── ...                            # Environmental & derived datasets
```

## Requirements

- R >= 4.5.0
- Required packages: readxl, ggplot2, dplyr, tidyr, patchwork, ggtext, scales, mgcv, xgboost, writexl, ggbeeswarm, pheatmap, vegan, zoo, ggrepel, randomForest, lavaan, sf, rnaturalearth

Install with:
```r
install.packages(c("readxl","ggplot2","dplyr","tidyr","patchwork","ggtext","scales","mgcv","xgboost","writexl","ggbeeswarm","pheatmap","vegan","zoo","ggrepel","randomForest","lavaan","sf","rnaturalearth"))
```

## Usage

Set working directory to the repository root, then run scripts in order:

```r
source("代码/01_environmental_trends.R")
source("代码/02_phytoplankton_community.R")
source("代码/03_esd_carbon_flux.R")
source("代码/04_shap_analysis.R")
source("代码/05_correlation_heatmap.R")
source("代码/06_global_miniaturization.R")
source("代码/07_alpha_diversity.R")
source("代码/08_supplementary_models.R")
```

## Data Availability

The raw phytoplankton count data (`raw_phytoplankton_counts.xlsx`) represents 189 species counted at 303 station visits across 13 summer cruises (2004–2018) in Bohai Bay. Environmental time series data span 2000–2020, compiled from cruise measurements and published literature.

## License

Data: CC-BY 4.0 | Code: MIT
