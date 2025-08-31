# House Price Prediction in Milan Using Data Mining Techniques

This project applies data mining techniques to predict house prices in the metropolitan city of Milan.  
The dataset (≈ 8,000 observations, 16 variables), contains property features such as size, number of rooms and bathrooms, floor, heating system, energy efficiency, and additional amenities.

---

## 📊 Dataset
- **Target variable**: `Selling Price` (house price in €).  
- **Main predictors**:
  - Square meters, number of rooms, number of bathrooms  
  - Floor and total floors in the building  
  - Lift availability  
  - Condition (excellent, good, new, to be refurbished)  
  - Heating system (centralized vs. independent)  
  - Energy efficiency class  
  - Parking availability  
  - Location (zone in Milan)  
  - Additional features (terrace, garden, concierge, swimming pool, etc.)  

Extensive **data preprocessing** was required:  
- Handling ≈ 4,000 missing values with domain-driven imputation strategies  
- Feature engineering (e.g., luxury index, distance from Duomo, optional amenities)  
- Outlier detection and correction (e.g., unrealistic surface area or condominium fees)  

---

## 🔧 Methods
Several predictive models were tested:  
- Linear regression (with fixed and random effects for `zone`)  
- Linear models with B-splines  
- Generalized Additive Models (GAMs) with thin plate regression splines  

Key methodological choices:  
- Logarithmic transformation of `Selling Price`  
- Shrinkage effects via mixed models (`zone` as a random intercept)  
- Exploration of non-linear relationships using splines  
- Interaction effects (e.g., bathrooms × surface, floor × luxury, Duomo distance × luxury)  

---

## 📈 Results
- The **GAM with random intercepts for `zone`** achieved the best trade-off between interpretability and predictive accuracy.  
- Mean Absolute Error (MAE) on validation set ≈ **78,000 €**.  
- Non-linearities in `square meters` and `year of construction` were significant.  
- Luxury features (pool, alarm system, concierge) showed strong positive effects on price.  
- Location (`zone`) remained the most impactful predictor.  

---

   git clone https://github.com/tommaasopozzi/House-Price-Prediction-Milan.git
