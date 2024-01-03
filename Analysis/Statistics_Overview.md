# Mixed models
Here I chose to use mixed effects models because it handles well highly dependent data (e.g. multiple recordings from the same person), and missing data. 


## Specific Model
OutcomeVariable ~ Condition + Task + Hour + Age + Group + (1|Participant) + (1|Participant:Session)

### Fixed effects:
The fixed effects are the ones that would theoretically re-appear in someone else's experiment. I change which ones I use in the model, depending on what is the effect I'm interested in, and how much that affect appeared significant in previous analyses.

For age, I center the values to the mean, because something something dependancy of intercept and slope?

### Random effects:
These are sources of variance that I can easily identify, but would not appear in anyone else's experiment. So that's participants, and the different sessions they were recorded.

For the sessions, I assigned a unique code to each session, so that it's clear to the model that this only happens once; then multiple recordings (several tasks, morning/evening) are recorded in that same session. My information on what to do came from here:
https://www.muscardinus.be/statistics/nested.html
