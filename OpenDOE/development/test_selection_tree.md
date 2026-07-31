# Test-selection tree

```
Zufallseffekt-Struktur vorhanden? (Messwiederholung / Wurf / Käfig / ...)
├─ ja  → LMM
│
└─ nein → Anzahl Levels?
   ├─ 1-2
   │  └─ N (normalverteilt?)
   │     ├─ ja  → VH (Varianzen homogen?)
   │     │        ├─ ja   → t-Test (Student)
   │     │        └─ nein → Welch t-Test
   │     └─ nein → Wilcoxon Rangsummentest
   │
   └─ >2
      ├─ N (normalverteilt?)
      │  ├─ ja  → VH (Varianzen homogen?)
      │  │        ├─ ja   → ANOVA (Student)
      │  │        └─ nein → Welch-ANOVA
      │  └─ nein → Kruskal-Wallis Test
      │
      └─ MCC (paarweise Vergleiche gewünscht?)
         └─ N (normalverteilt?)
            ├─ ja  → VH (Varianzen homogen?)
            │        ├─ ja   → paarweiser t-Test   + Holm/Hochberg/Bonferroni
            │        └─ nein → paarweiser Welch-t  + Holm/Hochberg/Bonferroni
            └─ nein → paarweiser Wilcoxon-Test     + Holm/Hochberg/Bonferroni
```
