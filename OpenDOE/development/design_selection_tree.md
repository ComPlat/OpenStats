# Design-selection tree (Other-groups)

**Investigation-groups**: der/die Faktor(en) von wissenschaftlichem Interesse
(z.B. Drug mit Levels A/B/C) -- das, was Subjekten per Random Assignment
zugelost wird.

**Other-groups**: zusätzliche Gruppierungsvariable(n), kein wissenschaftliches
Interesse an sich, aber Teil der Datenstruktur (z.B. Standort/Käfig/Wurf).

```
Other-groups != NULL?
├─ nein → completely randomised (kein LMM nötig)
└─ ja
   ├─ Anzahl Other-groups = 1
   │  └─ Zweite Randomisierungsstufe? (Investigation-group wird
   │     innerhalb der Other-group nochmal randomisiert)
   │     ├─ ja   → split-plot   (→ LMM)
   │     └─ nein → blocked      (→ LMM)
   │
   └─ Anzahl Other-groups ≥ 2
      ├─ genau 2, systematische (nicht randomisierte) Zuordnung,
      │  keine Replikation pro Zelle
      │           → latin-square (→ ANOVA, fixed -- row/col nicht
      │             schätzbar als random ohne Replikation)
      └─ sonst (frei randomisiert je Other-group)
                  → split-block (→ LMM)
```

## Warum LMM/ANOVA jeweils so zugeordnet ist

**Blocked → LMM:** Um die Varianz zwischen den Blöcken (z.B. Käfigen)
überhaupt von der Residualvarianz zu trennen, braucht das Modell genug
Blöcke. Technisch geht's ab ~3 Blöcken (weniger ist nicht identifizierbar),
aber für eine einigermaßen stabile Schätzung will man eher ≥5-6, besser
mehr. Mit zu wenigen Blöcken bekommt man oft "singular fit"-Warnungen oder
die Varianzkomponente wird exakt auf 0 geschätzt (Boundary-Problem) -- dann
ist der klassische Ausweg, den Block als fixed statt random zu behandeln
und stattdessen eine normale ANOVA zu rechnen (genau wie beim Latin-Square).
Das ist also keine strikte Ja/Nein-Regel, sondern eine Frage von "genug
Blöcke vorhanden?".

**Split-plot → LMM:** Anderes Problem, und hier ist LMM nicht verhandelbar.
Der Investigation-group-Faktor, der *innerhalb* der Other-group nochmal
randomisiert wird (Sub-Plot-Faktor), und der Other-group-Faktor selbst
(Whole-Plot-Faktor) haben unterschiedliche Fehlerebenen -- der Whole-Plot-
Faktor wurde ja nur auf Whole-Plot-Ebene randomisiert, nicht auf Sub-Plot-
Ebene. Eine normale ANOVA würde für beide denselben (zu kleinen) Fehlerterm
benutzen -- das ist Pseudoreplikation und bläht die Typ-1-Fehlerrate für den
Whole-Plot-Faktor auf, komplett unabhängig davon, wie viele Whole-Plots man
hat. Die "genug Whole-Plots"-Frage von oben (Blocked) kommt hier zusätzlich
oben drauf (für eine stabile Varianzschätzung), ist aber ein zweites,
unabhängiges Problem -- nicht der Grund, warum LMM hier Pflicht ist.

**Split-block → LMM:** Kombination aus beidem, plus mehrere parallele
Zufallseffekt-Ebenen gleichzeitig. Anders als bei split-plot (eine Ebene
*geschachtelt* in der anderen) laufen hier mehrere Other-group-Dimensionen
unabhängig nebeneinander (z.B. Käfig **und** Tag, beide unabhängig
zugeteilt). Im LMM heißt das: ein eigener Zufallseffekt-Term pro
Other-group-Dimension, z.B. `(1|Käfig) + (1|Tag)` statt nur einem Term wie
bei blocked. Am fehleranfälligsten von den vieren, weil man leicht eine
Ebene vergisst oder falsch spezifiziert (genestet statt gekreuzt).

**Praktischer Check statt reiner Theorie:** Statt die "genug Blöcke/
Whole-Plots?"-Frage vorher analytisch abzuschätzen, kann die Monte-Carlo-
Sample-Size-Simulation (wie bei `determine_sample_size`: simulieren →
Modell fitten → Power schätzen) das selbst beantworten -- einfach bei jedem
simulierten Fit Konvergenz-/Singular-Fit-Warnungen mitzählen. Zu wenige
Other-group-Level zeigen sich dann von selbst als hohe
Nicht-Konvergenz-Rate im Simulationslauf.
