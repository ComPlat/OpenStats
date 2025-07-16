# Backgrond R process Kanbas ToDo board

- [x] create global class instance of bg_process
- [x] Put the bg_process instance as reactive value to ResultsState
- [x] Move the two entries: running_status and is_running as own values into bg_process
- [ ] Within each eval call in the engine put the actual calculation into ResultsState$bg_task$start()
  - [ ] The update of ResultsState$all_data and ResultsState$state$history is not part of the background process. Seperate this if required
- [ ] Add everywhere cancel buttons see below an example:
- [ ] Display the status once in MainApp.R; See the second example below

