- Endpoints handled by primary-assay-fold-change
	* fold activation
	* fold decrease
	* fold increase
	* fold inhibition

- Endpoints handled by primary-assay-percentage-continous
  - percent control
  - percent growth
  - percent inhibition
  - percent DNA replicated cells
  - percent neurite outgrowth

- Endpoints handled by primary-assay-percentage-binomial
  - percent DNA replicated cells
  - percent G2 arrested cells
  - percent mitotic arrested cells
  - percent mitotic cells
  - percent non-arrested cells

- Endpoints handled by primary-assay-percentage-multinomial
  - percent interphase cells

- Endpoints handled by primary-assay-percentage-count
  - percent neurite count

- Endpoints handled by special primary-assay
  * MinED
    - when does the response start to deviate from control?
    - Method:
      * fit model (lm / glm)
      * perform contrasts vs control (e.g. trt.vs.ctrl)
      * determine smallest dose with significant effect

---

- Endpoints directly handled by dose response
  - AC10 absolute
  - AC26 absolute
  - AC35 absolute
  - AC40 absolute
  - AC50
  - CC50
  - EC10 / EC20 / EC25 / EC30 / EC50 / EC90
  - EC50
    - pEC50
  - GI50
  - IC20 / IC25 / IC30 / IC50 / IC75 / IC80 / IC90 / IC95  
  - IC50 absolute
  - pIC50
  - LC50
  - ED / ED25 / ED30 / ED50 / ED80 / ED90
  - ID50
  - LD10 / LD50

- Endpoints handled by dose response with prior normalization
  - IC50 relative

- Endpoints handled by special dose response
  - Endpoints handled by root solving
    - Method: inverse prediction / numerical root finding on fitted dose–response model
    - Note: only estimable if the target response lies within the fitted response range
    - AC1000 absolute  (implementation required)
    - AC500 absolute  (implementation required)
    - EC150
  - AC50 Ratio
    - Method:
      * fit two dose–response curves
      * estimate AC50 for each curve
      * report AC50_1 / AC50_2
  - EC 5 hour
    - Method:
      * subset data at time = 5h (or interpolate if necessary)
      * fit dose–response model
      * estimate ECx via standard dose–response methods
  - ECMax
    - Method:
        * fit dose–response model:
          y = c + (d - c) / (1 + (x/e)^b)
        * where:
          - c = lower asymptote
          - d = upper asymptote
        * ECMax = extremum of the fitted curve:
          - use d for increasing (agonist-like) responses
          - use c for decreasing (inhibitory) responses
    - ECMax_Agonist  (implementation required)
      * Method:
        1. fit dose–response model
        2. extract upper asymptote (d)
        3. report d as ECMax
    - ECMax_Tm  (implementation required)
      * Method:
        1. subset data at specified condition (e.g. time = Tm or temperature = Tm)
            or interpolate to Tm if not directly measured
        2. fit dose–response model on filtered data
        3. extract ECMax as asymptote (c or d depending on direction)
    - ECMax_fold increase  (implementation required)
      * Method:
        1. fit dose–response model
        2. extract ECMax (typically d)
        3. compute fold change relative to control or baseline:
            fold_increase = d / μ_control
            (μ_control = mean response at control condition)
    - ECMax_percent inhibition  (specific)
      * Method:
        1. fit dose–response model
        2. extract relevant asymptote:
            - use c for inhibitory responses
            - use d if inhibition is encoded inversely
        3. compute percent inhibition relative to control:
            inhibition = (μ_control - asymptote) / μ_control * 100
            (μ_control = mean response at control condition)

---

- Endpoints handled by optimization model
  - Ka
  - Ki
  - pA2 (Schild regression. Currently the user has to use the free formula and define it by hand)

---

- MIC / MIC51 / MIC80 / MIC90  (implementation required)
  - Method:
      * determine MIC per sample:
        - MIC = lowest concentration with no detectable growth
      * for multiple samples:
        - MIC51 = median MIC
        - MIC81 = 80th percentile of MIC values
        - MIC91 = 90th percentile of MIC values
      * doing something like: quantile(MIC, probs = 0.5)

---


- Endpoints handled by either dose-response or dose-response-binomial
  - This is decided based on the model defined linear or generalized linear binomial
  - TC50
  - TD50

- Endpoints handled by dose-response-binomial
  - TCID50
  - MTD

- TGI  (implementation required)
  - Method: growth curve modeling
