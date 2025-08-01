# Hematology ANOVA Analysis

This repository contains R codes for conducting a comprehensive statistical analysis of hematological parameters across different treatment groups using ANOVA and post-hoc tests. The analysis is designed for datasets typically derived from experimental trials involving animals or aquaculture species such as *Clarias gariepinus* (African catfish).

---

## 🔍 Project Overview

The goal of this project is to:
- Tidy hematological datasets using the `tidyverse` workflow
- Compute summary statistics (mean ± standard error)
- Perform one-way ANOVA to evaluate the effects of treatments on blood parameters
- Apply Tukey’s Honest Significant Difference (HSD) test to identify group differences
- Generate a clean, publication-ready table combining numerical results and HSD groupings

---

## 🧪 Hematological Parameters Analyzed

The pipeline currently processes the following common blood parameters:

- PLT (Platelets)
- HGB (Hemoglobin)
- MCH (Mean Corpuscular Hemoglobin)
- MCV (Mean Corpuscular Volume)
- PCT (Plateletcrit)
- RBC (Red Blood Cells)
- RDW-CV (%) (Red Cell Distribution Width – Coefficient of Variation)
- RDW-SD (Red Cell Distribution Width – Standard Deviation)
- WBC (White Blood Cells)
- MCHC (Mean Corpuscular Hemoglobin Concentration)
- HCT (Hematocrit)
