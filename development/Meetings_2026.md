## 13.02.2026

- User has to define a measurement type in case method is set to "DoseResponse"
- This has to be set before conducting MTT assay is pressed:
  * choices: ANY, Absorbance, Fluorescence
- I get send the metadata from each generic element (Discuss with Ali how the json data layout changes)
  * within this metadata the choices of measurements are defined
  * and I also get the information which generic element has send the data
- Update generic Dose Response element.
  * add a Metadata layer and add here the measurement method
  * Christoph and I decided that the user should choose the assay (e.g. MTT).
    Moreover, the user should be capable to choose one or multiple the measurement methods (e.g. Absorbance)

## 09.01.2026

--> Until: 16.01
- talk with paggy and claire how to detect that I get data of a generic MTT element
- write import json data for MTT assay
- json data send back to ELN in case of MTT assay
  * containing the table with the IC50 value etc.
  * raw data to create the plots in the ELN

## 19.01.2026

- query parameter for detection: method:DoseResponse
- extract id and request_id from json send to OpenStats.
   * also send it back
- before the token the path is defined in the URL. public/third_party_app
  Use this instead of the hardcoded path
- Show the json format of the data I send back to the ELN to Paggy tomorrow
