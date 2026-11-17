json_string <- '
[
  {
    "type": "ModelSummary",
    "formula": "uptake ~ conc",
    "R2": "R² = 0.235",
    "Result name": "1 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ conc"
  },
  {
    "type": "ModelSummary",
    "formula": "uptake ~ conc * Type",
    "R2": "R² = 0.604",
    "Result name": "3 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ conc * Type"
  }
]
'
