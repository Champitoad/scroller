# Features

- Proof mode
  - For now, we still restrict deiteration to atoms (do we?). This limits us to first-order
    functions without ADTs however. Later we will need to support deiteration on scrolls by either:
    - implementing matching of scroll structures up to permutations. Then every deiteration needs to
      either:
      - record the permutation it used, so that it can be used for propagating information during
        evaluation;
      - apply the permutation immediately.

      Also if we do not label every inloop and every judgment in outloops, there might be more than
      one solution for permutation matching, and it is not clear if this should be presented to the
      user (probably not).
    - or we don't do it up to permutation. Then it puts more load on the user, but makes things
      completely deterministic (and closer to traditional programming). We would still want to force
      constructor labels on every inloop to actually match the semantics of ADTs.
  - Support for DnD actions on inloops
  - Linear mode: when toggled, DnD will automatically insert a deletion on the source, and be
    disabled on nodes for which a (de)iteration has already been recorded

- Edit mode
  - Still just a means to perform the insertion and deletion rules
  <!-- - Localize commit mechanism for insertion:
    - Switching to Proof mode does not trigger a global commit anymore
    - Only the root judgment is marked as "grown" (inserted)
    - A `commit` button is added to grown judgments -->
  - Implement the `Renaming` `EditInteraction`:
    - When `goal.actionMode == EditMode (Renaming path) _`, the label of the value/inloop at `path`
      becomes an input field
    - Pressing the `Enter` (resp. `Esc`) key validates (resp. cancels) the renaming
    - Pressing a +atom/scroll/inloop button immediately inserts the corresponding node and starts a `Renaming` interaction
    - For +atom buttons, we can keep the input field for specifying the type name

- Navigation mode
  - Each context in the navigation stack has its own actions queue

- Type/Program aliases
  - Use the double-box notation in folded form
  - Clicking on the name unfolds
  - Usual type aliases are just aliases for programs of type $\vdash \mathbf{Type}$ built with the broken cut
  - Global environment of aliases, essentially a dictionary from names to scroll nets
  - Unfolding is implemented by lookup in the global env
  - Two types of folding:
    - Given an alias name, fold every occurrence of the alias body in the goal
    - Given a subnet in the goal, fold with the alias found by reverse lookup
      (assuming we enforce the global env to be bijective)


- Vertical generalization of the scroll, where inloops can be iterated inside adjacent inloops

# Brainstorming

- It seems we don't need to distinguish between judgment and constructor identifiers for inloops in the (bi-)intuitionistic case, since they will always be (de)iterated in the same scroll. Classical logic might require this distinction though, because inloops can become outloops (and vice versa); or we could just drop constructor identifiers altogether since there is no purpose in distinguishing between inloops and outloops, in the same way that one can go one-sided in classical sequent calculus (and thus drop the distinction between *term* and *continuation* variables).