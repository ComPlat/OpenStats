# ELN ↔ OpenDOE: doe_definition

Stored per reaction in `reactions.doe_definition` (jsonb, new column).
Needed in both flows, so it is persisted (not a temporary token cache).

## Flows

```
Flow 1: start (LHS)
├── Reaction scheme → "Create variations (OpenDOE)" → DoE modal
│   └── user defines factors (→ doe_definition) + initial batch size
├── ELN: token → OpenDOE GET: scheme info + doe_definition
├── OpenDOE (interactive): LHS
└── OpenDOE POST rows → ELN creates the first variations

Flow 2: follow-up round (BO)
├── Variations table → "New variations (OpenDOE)"
├── ELN: token → OpenDOE GET: variations (incl. results) + doe_definition
├── OpenDOE (interactive): user picks response column(s) + max/min, n
│   └── BO
└── OpenDOE POST rows → ELN appends them to the variations table
```

Same pattern as variations → OpenStats: the ELN builds the JSON from
the DB at GET time (JWT token, third_party_app API).

## Factor kinds (`target.kind`)

| kind | type | meaning |
|---|---|---|
| `material_selection` | categorical | group of samples, exactly one active, amounts from the scheme; optional `"none"` |
| `material_amount` | continuous | one `entry` of one sample, `min … max` in `unit`; optional `allow_absent` |
| `property` | continuous | reaction property, temperature (°C) or duration (Hour(s)) |

- several `material_amount` factors (one per sample) are allowed
- on/off of a single sample with its scheme amount = `material_selection` with one sample + `allow_none`
- grouping is defined by the user in the modal
- `entry`: `equivalent` (no unit) | `mass` (mg, g, μg) | `amount` (mmol, mol) | `volume` (ml, l, μl),
  same as the material columns of the variations table; solvents only `volume`
- `material_amount.allow_absent` → OpenDOE gets two predictors: `<name> present` (yes/no) and `<name>`;
  when absent the amount predictor holds `min` (no meaning, the GP needs a number)
- `replicates`: each proposed experiment is repeated n times (LHS and BO batches)
- min/max are stored in the chosen unit; the ELN stores variations in g / mol / l / °C / Second(s)
- reference material can't be a factor
- definitions without `entry` (older) vary the equivalent

## Example

```json
{
  "version": 1,
  "initial_batch_size": 6,
  "replicates": 2,
  "factors": [
    { "name": "Catalyst", "type": "categorical",
      "target": { "kind": "material_selection",
                  "sample_ids": [11, 12, 13], "allow_none": false } },
    { "name": "Additive", "type": "continuous",
      "min": 5, "max": 20,
      "target": { "kind": "material_amount", "sample_id": 21,
                  "entry": "mass", "unit": "mg",
                  "allow_absent": true } },
    { "name": "Base loading", "type": "continuous",
      "min": 1.0, "max": 3.0,
      "target": { "kind": "material_amount", "sample_id": 31,
                  "entry": "equivalent", "unit": "",
                  "allow_absent": false } },
    { "name": "Temperature", "type": "continuous",
      "min": 40, "max": 90,
      "target": { "kind": "property", "field": "temperature",
                  "unit": "°C" } }
  ]
}
```

## Batch OpenDOE → ELN

JSON file uploaded to `public/third_party_app_doe/:token`, stored in
`reactions.doe_pending_batch` until the user adds it in the variations tab.

```json
{ "id": "68", "request_id": "…", "mode": "initial",
  "rows": [ { "Catalyst": 12, "Additive": null,
              "Base loading": 2.0, "Temperature": 55 } ] }
```

- `material_selection`: sample id, `null` = none
- `material_amount`: number, `null` = absent (only with `allow_absent`)
- replicates are already expanded (one row per experiment)

## Responses (flow 2)

Every product entry with at least one value:
`<product> yield | mass | amount | volume | TON | TOF | concentration` (last three: gas products);
the user picks the response column(s) in OpenDOE.

## Mapping rules

OpenDOE → ELN (new variation row):
- `material_selection = X`: sample X gets its amount **from the scheme**,
  the other samples of the group get 0; `none` → all 0
- `material_amount = v`: the sample's `entry` is set to `v` (in `unit`), the other
  entries follow via the variations table parsers; `null` → all entries 0
- `property = v`: property set to `v`

ELN → OpenDOE (existing variation rows, flow 2):
- `material_selection`: the group sample with amount > 0 is the level;
  none > 0 → `none` (if allowed), several > 0 → row invalid (flagged)
- `material_amount` with `allow_absent`: amount 0 → present `no`, amount predictor = `min`
- `material_amount` / `property`: value read directly
- bounds always come from `doe_definition`, never from the data

## Validation in the modal

- sample in a `material_selection` group without amount in the scheme → block,
  suggest `material_amount`
- a sample must not be used in two factors
- `<name> present` must not clash with another factor name
- continuous: `min < max`; batch size and replicates positive integers

## Later

- concentration (mol/L) as factor entry, once the ELN stores it for reactants
- covariates (measured, not designed; contextual BO)
- verify the unit columns available in the variations table (equivalent?)
