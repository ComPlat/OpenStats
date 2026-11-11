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
| `material_selection` | categorical | group of samples, exactly one active; optional `"none"` |
| `material_amount` | continuous | amount of one sample, `min … max` in equivalents (`min = 0` → absent) |
| `property` | continuous | reaction property, e.g. temperature, duration |

- on/off of a single sample = `material_selection` with one sample + `allow_none`
- grouping is defined by the user in the modal

## Example

```json
{
  "version": 1,
  "initial_batch_size": 6,
  "factors": [
    { "name": "Catalyst", "type": "categorical",
      "target": { "kind": "material_selection",
                  "sample_ids": [11, 12, 13], "allow_none": false } },
    { "name": "Additive", "type": "categorical",
      "target": { "kind": "material_selection",
                  "sample_ids": [21], "allow_none": true } },
    { "name": "Base loading", "type": "continuous",
      "min": 1.0, "max": 3.0,
      "target": { "kind": "material_amount",
                  "sample_id": 31, "unit": "equivalent" } },
    { "name": "Temperature", "type": "continuous",
      "min": 40, "max": 90,
      "target": { "kind": "property", "field": "temperature" } }
  ]
}
```

## Mapping rules

OpenDOE → ELN (new variation row):
- `material_selection = X`: sample X gets its amount **from the scheme**,
  the other samples of the group get 0; `none` → all 0
- `material_amount = v`: sample gets `v` equivalents
- `property = v`: property set to `v`

ELN → OpenDOE (existing variation rows, flow 2):
- `material_selection`: the group sample with amount > 0 is the level;
  none > 0 → `none` (if allowed), several > 0 → row invalid (flagged)
- `material_amount` / `property`: value read directly
- bounds always come from `doe_definition`, never from the data

## Validation in the modal

- sample in a `material_selection` group without amount in the scheme:
  - single sample → offer `material_amount` instead
  - group → block ("set amounts in the scheme first")
- a sample must not be in a group **and** a `material_amount` factor
- continuous: `min < max`

## Later

- conditional factor: "which catalyst" + "how much" (only for the active one)
- covariates (measured, not designed; contextual BO)
- verify the unit columns available in the variations table (equivalent?)
