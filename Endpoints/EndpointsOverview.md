# Endpoint Handler Overview

Tabular overview of every endpoint, the handler/method that processes it, and its current implementation status.

Status legend:
- **specific** — endpoint has no children; UI/workflow can be predefined
- **ambiguous** — endpoint has children; default OpenStats workflow (user picks model manually)
- **implementation required** — handler not yet implemented

---

## Primary assay handlers

| Endpoint                       | Handler                                | Status                  | Notes                                                                 |
|--------------------------------|----------------------------------------|-------------------------|-----------------------------------------------------------------------|
| fold activation                | primary-assay-fold-change              | specific                | lm + emmeans (trt.vs.ctrl); fold = μ_trt / μ_ctrl                     |
| fold decrease                  | primary-assay-fold-change              | specific                | lm + emmeans (trt.vs.ctrl); fold = μ_trt / μ_ctrl                     |
| fold increase                  | primary-assay-fold-change              | specific                | lm + emmeans (trt.vs.ctrl); fold = μ_trt / μ_ctrl                     |
| fold inhibition                | primary-assay-fold-change              | specific                | lm + emmeans (trt.vs.ctrl); fold = μ_trt / μ_ctrl                     |
| percent control                | primary-assay-percentage-continuous    | specific                | lm on continuous % data                                               |
| percent growth                 | primary-assay-percentage-continuous    | specific                | lm on continuous % data                                               |
| percent inhibition             | primary-assay-percentage-continuous    | ambiguous               | lm on continuous % data                                               |
| percent DNA replicated cells   | primary-assay-percentage-continuous *or* binomial | specific     | continuous (%) or binomial (counts)                                   |
| percent neurite outgrowth      | primary-assay-percentage-continuous    | implementation required | continuous (trajectory / curve-derived)                               |
| percent G2 arrested cells      | primary-assay-percentage-binomial      | specific                | GLM (binomial) + emmeans                                              |
| percent mitotic arrested cells | primary-assay-percentage-binomial      | specific                | GLM (binomial) + emmeans                                              |
| percent mitotic cells          | primary-assay-percentage-binomial      | specific                | GLM (binomial) + emmeans                                              |
| percent non-arrested cells     | primary-assay-percentage-binomial      | specific                | GLM (binomial) + emmeans                                              |
| percent interphase cells       | primary-assay-percentage-multinomial   | implementation required | nnet::multinom or VGAM; emmeans support limited                       |
| percent neurite count          | primary-assay-percentage-count         | implementation required | GLM (Poisson / negative binomial) + emmeans                           |
| MinED                          | special primary-assay                  | implementation required | lm/glm + trt.vs.ctrl contrasts; smallest dose with significant effect |

---

## Dose–response handlers

| Endpoint        | Handler                                | Status                  | Notes                                              |
|-----------------|----------------------------------------|-------------------------|----------------------------------------------------|
| AC10 absolute   | dose response                          | specific                |                                                    |
| AC26 absolute   | dose response                          | specific                |                                                    |
| AC35 absolute   | dose response                          | specific                |                                                    |
| AC40 absolute   | dose response                          | specific                |                                                    |
| AC50            | dose response                          | specific                |                                                    |
| CC50            | dose response                          | specific                |                                                    |
| EC10            | dose response                          | specific                |                                                    |
| EC20            | dose response                          | specific                |                                                    |
| EC25            | dose response                          | specific                |                                                    |
| EC30            | dose response                          | specific                |                                                    |
| EC50            | dose response                          | specific                |                                                    |
| EC90            | dose response                          | specific                |                                                    |
| pEC50           | dose response                          | specific                | report log value; original may be dropped          |
| GI50            | dose response                          | specific                |                                                    |
| IC20            | dose response                          | specific                |                                                    |
| IC25            | dose response                          | specific                |                                                    |
| IC30            | dose response                          | specific                |                                                    |
| IC50            | dose response                          | specific                |                                                    |
| IC75            | dose response                          | specific                |                                                    |
| IC80            | dose response                          | specific                |                                                    |
| IC90            | dose response                          | specific                |                                                    |
| IC95            | dose response                          | specific                |                                                    |
| IC50 absolute   | dose response                          | specific                |                                                    |
| pIC50           | dose response                          | specific                | report log value; original may be dropped          |
| LC50            | dose response                          | specific                |                                                    |
| ED / ED25 / ED30 / ED50 / ED80 / ED90 | dose response          | specific                |                                                    |
| ID50            | dose response                          | implementation required |                                                    |
| LD10            | dose response                          | specific                |                                                    |
| LD50            | dose response                          | specific                |                                                    |
| IC50 relative   | dose response w/ prior normalization   | specific                | normalize before fitting                           |

---

## Special dose–response handlers

| Endpoint                  | Handler                          | Status                  | Notes                                                                 |
|---------------------------|----------------------------------|-------------------------|-----------------------------------------------------------------------|
| AC1000 absolute           | root solving on fitted curve     | implementation required | inverse prediction; only estimable inside fitted response range       |
| AC500 absolute            | root solving on fitted curve     | implementation required | as AC1000                                                             |
| EC150                     | root solving on fitted curve     | specific                | as AC1000                                                             |
| AC50 Ratio                | special dose response            | implementation required | fit two curves; ratio of AC50_1 / AC50_2                              |
| EC 5 hour                 | special dose response            | specific                | subset/interpolate at t=5h, then standard dose–response               |
| ECMax                     | special dose response            | ambiguous               | extremum of fitted 4-parameter logistic                               |
| ECMax_Agonist             | special dose response            | implementation required | upper asymptote (d)                                                   |
| ECMax_Tm                  | special dose response            | implementation required | subset/interpolate at Tm, then asymptote                              |
| ECMax_fold increase       | special dose response            | implementation required | d / μ_control                                                         |
| ECMax_percent inhibition  | special dose response            | specific                | (μ_control - asymptote) / μ_control × 100                             |

---

## Optimization model

| Endpoint | Handler            | Status                  | Notes                                                              |
|----------|--------------------|-------------------------|--------------------------------------------------------------------|
| Ka       | optimization model | specific                |                                                                    |
| Ki       | optimization model | specific                |                                                                    |
| pA2      | optimization model | implementation required | Schild regression; user currently uses free formula by hand        |

---

## Mixed binomial / continuous dose–response

| Endpoint | Handler                                  | Status                  | Notes                                                          |
|----------|------------------------------------------|-------------------------|----------------------------------------------------------------|
| TC50     | dose-response *or* dose-response-binomial | implementation required | choice driven by linear vs. GLM-binomial model                 |
| TD50     | dose-response *or* dose-response-binomial | specific                | binomial: GLM (logit), continuous: dose–response               |
| TCID50   | dose-response-binomial                   | specific                | logistic infection model on log10(dilution)                    |
| MTD      | dose-response-binomial                   | implementation required | dose where predicted toxicity ≤ threshold                      |

---

## Other handlers

| Endpoint                       | Handler              | Status                  | Notes                                                                 |
|--------------------------------|----------------------|-------------------------|-----------------------------------------------------------------------|
| MIC / MIC50 / MIC80 / MIC90    | MIC quantile         | implementation required | per-sample: lowest conc. with no growth; aggregate via quantile()     |
| TGI                            | growth curve modeling| implementation required |                                                                       |
