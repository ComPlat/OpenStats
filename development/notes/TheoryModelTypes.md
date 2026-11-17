## Linear Models (`lm`)

**Typical Use:**
Continuous response, normally distributed errors.

## Generalized Linear Models (`glm`)

**Typical Use:**
Binary/Count data, e.g. logistic or Poisson regression.
Or Continuous response, and the distributed errors follow a non-gaussian distribution.

## Linear Mixed Models (`lmer`, `lmerTest`)

Typical Use:
* Repeated measures
* hierarchical data
* Split plot
* etc.

## Comparison Table

| Test Type        | `lm`                      | `glm`                        | `lmer` / `lmerTest`            |
|------------------|---------------------------|------------------------------|--------------------------------|
| Coeff. test      | `t-test`                  | `z-test`                     | `t-test` (via `lmerTest`)     |
| ANOVA            | `anova()`                 | `anova(test = "Chisq")`     | `anova()`                     |
| Likelihood test  | —                       | `anova(model1, model2)`      | same                          |
| Post-hoc         | `TukeyHSD()`, `agricolae` | `emmeans::contrast()`        | `emmeans::contrast()`         |
| Residual checks  | Yes                       | No (less meaningful)         | Partial (limited diagnostics) |

---

