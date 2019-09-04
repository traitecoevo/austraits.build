---
name: New data submission
about: Default workflow for new data submissions
title: "dataset_id: Processing submission"
labels: data submissions
assignees: ''

---

**Source**: Smith et al (20XX) Title *Journal* http://doi.org/....
**Custodian**: ?Dataset custodian?
**Handled by**: ?Austraits team member?


- [ ] Contact made with contributor (label as `waiting on response` if waiting for files)
- [ ] Files received from contributor saved in google drive folder `austraits/data submissions/new`  ( label as `ready` )
- [ ] Started importing files into own Austraits branch  (mark label as `In progress` and assign issue to relevant person )
- [ ] Metadata complete, builds into Austraits
- [ ] Report reviewed by AusTraits team
- [ ] Report sent to contributor ( label as `waiting on response` )
- [ ] Response received from contributor
- [ ] Files updated & report finalised
- [ ] Final checks ( all tests pass )
- [ ] Branch merged into master branch (issue `closed` )


NB: 

- if you need to change the config files, e.g. to add a unit conversion or define a new trait. This should be done on the master branch (via pull or rebase)
