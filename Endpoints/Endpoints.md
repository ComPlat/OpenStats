## General Handling Logic

- If an endpoint has children --> ambiguous --> use default OpenStats workflow
  - user defines model manually (lm / GLM / optimization)
  - full UI (assumptions, tests, contrasts, etc.)

- If an endpoint has no children --> specific --> UI can be adapted
  - only relevant methods are exposed
  - predefined workflows possible

---

## Foldchange endpoints

- fold change  (ambiguous)
	* fold activation  (specific)
	* fold decrease  (specific)
	* fold increase  (specific)
	* fold inhibition  (specific)

- Method for all specific endpoints:
  * Model: lm (raw data, no normalization)
  * Inference: primary assay analysis (trt.vs.ctrl via emmeans)
  * Reporting:
    - estimated marginal means (emmeans)
    - fold change computed as (μ_trt / μ_ctrl) from model estimates

---

## Percent response endpoints

- percent response  (ambiguous)
	- percent activity  (ambiguous)
		- percent cell viability  (ambiguous)
			* percent DNA replicated cells  (specific)
			  - data type: binomial (counts) or continuous (%)
			* percent G2 arrested cells  (specific)
			  - data type: binomial
			* percent interphase cells  (implementation required)
			  - data type: multinomial (multiple states)
			* percent mitotic arrested cells  (specific)
			  - data type: binomial
			* percent mitotic cells  (specific)
			  - data type: binomial
			* percent neurite count  (implementation required)
			  - data type: count
			* percent neurite outgrowth  (implementation required)
			  - data type: continuous (trajectory / curve-derived)
			* percent non-arrested cells  (specific)
			  - data type: binomial

	- percent control  (specific)
	  - data type: continuous (normalized to control)

	- percent growth  (specific)
	  - data type: continuous

	- percent inhibition  (ambiguous)
		- percent cytotoxicity  (ambiguous)
			* percent growth inhibition  (specific)
			  - data type: continuous


- Method:

  * binomial response:
    - Model: GLM (family = binomial)
    - Inference: primary assay analysis (trt.vs.ctrl via emmeans)
    - Reporting:
      - estimated probabilities (emmeans, type = "response")
      - odds ratios (preferred)

  * multinomial response (implementation required):
    - Model: multinomial GLM (e.g. nnet::multinom or VGAM)
    - Inference: contrasts vs control per category
    - Note: emmeans support is limited --> additional integration required

  * count response:
    - Model: GLM (Poisson or negative binomial)
    - Inference: primary assay analysis (trt.vs.ctrl via emmeans)
    - Reporting:
      - rate ratios (type = "response")

  * continuous response:
    - Model: lm (raw or % scale, no additional transformation required)
    - Inference: primary assay analysis (trt.vs.ctrl via emmeans)
    - Reporting:
      - estimated means
      - differences vs control

---

## Concentration and dosage endpoints

- concentration endpoint  (ambiguous)

  - concentration response endpoint  (ambiguous)
      - AC10 absolute  (specific)
        - Method: dose–response
      - AC1000 absolute  (implementation required)
        - Method: inverse prediction / numerical root finding on fitted dose–response model
        - Note: only estimable if the target response lies within the fitted response range
      - AC26 absolute  (specific)
        - Method: dose–response
      - AC35 absolute  (specific)
        - Method: dose–response
      - AC40 absolute  (specific)
        - Method: dose–response
      - AC50  (specific)
        - Method: dose–response
      - AC50 Ratio  (implementation required)
        - Method:
          * fit two dose–response curves
          * estimate AC50 for each curve
          * report AC50_1 / AC50_2
      - AC500 absolute  (implementation required)
        - Method: see AC1000
      - CC50  (specific)
        - Method: dose–response
      - EC 5 hour  (specific)
        - Method:
          * subset data at time = 5h (or interpolate if necessary)
          * fit dose–response model
          * estimate ECx via standard dose–response methods
      - EC10 / EC20 / EC25 / EC30 / EC50 / EC90  (specific)
        - Method: dose–response
      - EC150  (implementation required)
        - Method: see AC1000
      - EC50
        - pEC50  (specific)
          - Method: dose–response. Maybe drop the original EC value.
      - ECMax  (ambiguous)
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
      - GI50  (specific)
        - Method: dose–response
      - IC20 / IC25 / IC30 / IC50 / IC75 / IC80 / IC90 / IC95  (specific)
        - Method: dose–response
      - IC50
        - IC50 absolute (specific)
          - Method: dose–response
        - IC50 relative  (specific)
          - Method: dose–response. Prior normalization.
        - definitive CYP IC50  (ambiguous)
        - pIC50  (specific)
          - Method: dose–response. Maybe drop the original value and show only the log value
      - Ka  (specific)
        - Method: optimization model
      - Ki  (specific)
        - Method: optimization model
      - LC50  (specific)
        - Method: dose–response
      - MIC / MIC50 / MIC80 / MIC90  (implementation required)
        - Method:
            * determine MIC per sample:
              - MIC = lowest concentration with no detectable growth
            * for multiple samples:
              - MIC50 = median MIC
              - MIC80 = 80th percentile of MIC values
              - MIC90 = 90th percentile of MIC values
            * doing something like: quantile(MIC, probs = 0.5, type = 1) 
      - TC50  (implementation required)
        - Method:
          * dose-response for continuous data
          * glm for binomial data
            * fit <- glm(cbind(dead, alive) ~ log10(conc), family = binomial, data = df)
            * b0 <- coef(fit)[1]
            * b1 <- coef(fit)[2]
            * log10_tc50 <- -b0 / b1
            * tc50 <- 10^log10_tc50
      - TCID50  (specific)
        - Method: logistic infection model
          * data: binomial (infected / non-infected per dilution)
          * Model:
            - fit GLM (family = binomial) on log10-transformed dilution:
              glm(cbind(infected, total - infected) ~ log10(dilution), family = binomial)
          * Estimation:
            - determine dilution where predicted infection probability = 0.5
            - solve:
              logit(p) = b0 + b1 * log10(dilution)
              --> log10(TCID50) = -b0 / b1
              --> TCID50 = 10^(-b0 / b1)
            - TGI  (implementation required)
              - Method: growth curve modeling
            - pA2  (implementation required)
              - Method: Schild regression
  - dosage endpoint  (ambiguous)
      * ED / ED25 / ED30 / ED50 / ED80 / ED90  (specific)
        - Method: dose–response
      * ID50  (implementation required)
        - Method: dose–response
      * LD10 / LD50  (specific)
        - Method: dose–response
      - MTD  (implementation required)
        - Method:
            * define acceptable toxicity threshold (e.g. 10% or 20%)
            * estimate toxicity as function of dose:
              - binary data: GLM (family = binomial)
              - continuous toxicity: regression model
            * determine maximum dose where predicted toxicity <= threshold
      * MinED  (implementation required)
        - when does the response start to deviate from control?
        - Method:
          * fit model (lm / glm)
          * perform contrasts vs control (e.g. trt.vs.ctrl)
          * determine smallest dose with significant effect
      - TD50  (specific)
        - Method:
            * binomial response:
              - fit GLM (family = binomial) for toxicity vs dose
              - estimate dose where predicted probability = 0.5
            * continuous response:
              - fit dose–response model
              - extract parameter e (half-max concentration)
