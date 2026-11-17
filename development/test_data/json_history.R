json_string <- '
[
  {
    "type": "CreateIntermediateVariable",
    "operation": "Max(conc)",
    "name": "max_conc"
  },
  {
    "type": "CreateNewColumn",
    "operation": "conc / max_conc",
    "column name": "conc_norm"
  },
  {
    "type": "Visualisation",
    "x": "conc_norm",
    "y": "uptake",
    "Plot-type": "box",
    "X axis label": "x label",
    "Y axis label": "y label",
    "Type of x": "factor",
    "Colour variable": "",
    "Legend title for colour": "Title colour",
    "Colour theme": "Accent",
    "Fill variable": "",
    "Legend title for fill": "Title fill",
    "Fill theme": "BuGn",
    "Split in subplots": "none",
    "Split by": "",
    "How to scale y in subplots": "free",
    "X-Range": ["0.0475", "1.25"],
    "Y-Range": ["7.315", "47.775"],
    "Width": 10,
    "Height": 10,
    "Resolution": 300,
    "Result name": "3 Visualization Boxplot"
  },
  {
    "type": "Visualisation",
    "x": "Treatment",
    "y": "uptake",
    "Plot-type": "box",
    "X axis label": "x label",
    "Y axis label": "y label",
    "Type of x": "factor",
    "Colour variable": "",
    "Legend title for colour": "Title colour",
    "Colour theme": "Accent",
    "Fill variable": "",
    "Legend title for fill": "Title fill",
    "Fill theme": "BuGn",
    "Split in subplots": "none",
    "Split by": "",
    "How to scale y in subplots": "free",
    "X-Range": ["nonchilled", "chilled"],
    "Y-Range": ["7.315", "47.775"],
    "Width": 10,
    "Height": 10,
    "Resolution": 300,
    "Result name": "4 Visualization Boxplot"
  },
  {
    "type": "ModelSummary",
    "formula": "uptake ~ conc",
    "R2": "R² = 0.235",
    "Result name": "5 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ conc"
  },
  {
    "type": "ShapiroOnData",
    "formula": "uptake ~ conc",
    "Result name": "ShapiroDataNr7"
  },
  {
    "type": "ShapiroOnResiduals",
    "formula": "uptake ~ conc",
    "Result name": "ShaprioResidualsNr8"
  },
  {
    "type": "ModelSummary",
    "formula": "uptake ~ Treatment",
    "R2": "R² = 0.102",
    "Result name": "9 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ Treatment"
  },
  {
    "type": "LeveneTest",
    "formula": "uptake ~ Treatment",
    "Data center": "mean",
    "Result name": "LeveneTestNr11"
  },
  {
    "type": "DiagnosticPlots",
    "formula": "uptake ~ Treatment",
    "Result name": "DiagnosticPlotNr12"
  },
  {
    "type": "ModelSummary",
    "formula": "uptake ~ conc_norm",
    "R2": "R² = 0.235",
    "Result name": "13 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ conc_norm"
  },
  {
    "type": "Correlation",
    "formula": "uptake ~ conc_norm",
    "Correlation method": "pearson",
    "Alternative hypothesis": "two.sided",
    "Confidence level of the interval": 0.95,
    "Result name": "15 Correlation Pearson"
  },
  {
    "type": "Correlation",
    "formula": "uptake ~ conc_norm",
    "Correlation method": "spearman",
    "Alternative hypothesis": "two.sided",
    "Confidence level of the interval": 0.95,
    "Result name": "16 Correlation Spearman"
  },
  {
    "type": "Correlation",
    "formula": "uptake ~ conc_norm",
    "Correlation method": "kendall",
    "Alternative hypothesis": "two.sided",
    "Confidence level of the interval": 0.95,
    "Result name": "17 Correlation Kendall"
  },
  {
    "type": "ModelSummary",
    "formula": "uptake ~ Type",
    "R2": "R² = 0.347",
    "Result name": "18 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ Type"
  },
  {
    "type": "TTest",
    "formula": "uptake ~ Type",
    "Confidence level of the interval": 0.95,
    "alternative hypothesis": "two.sided",
    "The two variances are": "eq",
    "Result name": "TTestNr20"
  },
  {
    "type": "ModelSummary",
    "formula": "uptake ~ conc",
    "R2": "R² = 0.235",
    "Result name": "21 Model plot"
  },
  {
    "type": "CreateFormula",
    "formula": "uptake ~ conc"
  },
  {
    "type": "ANOVA",
    "formula": "uptake ~ conc",
    "Result name": "Test_aovNr23"
  },
  {
    "type": "Kruskal-Wallis Test",
    "formula": "uptake ~ conc",
    "Result name": "Test_kruskalNr24"
  },
  {
    "type": "Tukey HSD",
    "formula": "uptake ~ conc",
    "Balanced design": false,
    "P-value": 0.05,
    "Result name": "Test_HSDNr25"
  },
  {
    "type": "Kruskal Wallis post hoc test",
    "formula": "uptake ~ conc",
    "Adjusted p value method": "holm",
    "P-value": 0.05,
    "Result name": "Test_kruskalTestNr26"
  },
  {
    "type": "Least significant difference test",
    "formula": "uptake ~ conc",
    "Adjusted p value method": "holm",
    "P-value": 0.05,
    "Result name": "Test_LSDNr27"
  },
  {
    "type": "Scheffe post hoc test",
    "formula": "uptake ~ conc",
    "P-value": 0.05,
    "Result name": "Test_scheffeNr28"
  },
  {
    "type": "REGW post hoc test",
    "formula": "uptake ~ conc",
    "P-value": 0.05,
    "Result name": "Test_REGWNr29"
  }
]
'
