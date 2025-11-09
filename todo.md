# Features

- Edit mode
  - Still just a means to perform the insertion and deletion rules
  - Localize commit mechanism for insertion:
    - Switching to Proof mode does not trigger a global commit anymore
    - Only the root judgment is marked as "grown" (inserted)
    - A `commit` button is added to grown judgments
