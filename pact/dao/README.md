# init DAO

This is the start of the KDA DAO, not the end.
It's not even something that should set a precedence.
It intentionally doesn't incentivize participation.
It doesn't even have a function to de-stake.
Instead a community approved(ish) contract upgrade is needed to add functionality.
The goal is to get KDA's guardians and ambassadors together, and make something better.
Guardians are the stakers, with governance rights to approve and apply upgrades.
Proposed upgrades have an APPROVAL_COOLDOWN days cooldown before they can be voted on and applied.
Proposals have APPROVAL_TIMEOUT days to be applied, and need to be re-submitted thereafter.
During this period, ambassadors can exercise their oversite:
a majority vote by the ambassadors can freeze the DAO for FREEZE_TIMEOUT days.
Guardians can add/deactivate/reactivate ambassadors.
A DEACTIVATE_COOLDOWN avoids a red wedding.

# Building (Javascript)

(TODO)

# Building (Pact)

1. [Install pact](https://github.com/kadena-io/pact#installing-pact). Note that dao.init requires at least Pact 3.7

2. (Optional) [Install Atom Pact IDE](https://github.com/kadena-io/pact#atom).

3. Tests are in `init.repl`.

4. (TODO installing on pact -s, testnet etc)
