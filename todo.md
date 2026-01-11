# Features

- Maintain uniqueness of same-scope bound variables in various operations
  - Function `bvu` (bound vars upward) that computes the bound variables available at a given `Path`
  - Function `bvd` (bound vars downward) that computes all the bound variables in scope of a given `Path`
  - All actions that introduce a new node (`Open`/`Insert`/`Iterate`):
    - can compute a fresh name automatically, or can ask the user for a name and deny it if it is already in `bvu ∪ bvd`
  - All actions that change the scope of a binder (`Close`)
    - can compute fresh names automatically for all bound vars in the inloop, or can ask the user to rename all those that are already in `bvd`
  - Renaming
    - check that the new name is not already in `bvu ∪ bvd`

- Maintain well-scopedness of justifications in various operations:
  - Function `jnd` (justified nodes downward) that computes the paths to all nodes that are justified from a given bound variable
  - All actions that eliminate a node (`Close`/`Delete`/`Deiterate`)
    - the action can only be executed if `jnd` is empty

- Execution of actions
  - "Step" button that dequeues and executes the next action
  - The action at the head of the queue should always be highlighted at the location where it occurs in the goal
  - **Free execution**:
    - the user can execute actions in the order she wants by clicking directly on them in the goal. However, only actions which are *executable* (and thus do not depend on other actions) can be executed:
      - `Open`: the parent is neither introduced nor eliminated
      - `Insert`: the parent is neither introduced nor eliminated
      - `Iterate`: the target's parent is neither introduced nor eliminated, and the source is not introduced
      - `Close`: the scroll has exactly one inloop and no values in its outloop, and it is neither introduced nor used in a (de)iteration
      - `Delete`: the node is neither introduced nor used in a (de)iteration
      - `Deiterate`: the target is neither introduced nor used in a (de)iteration, and the source is not introduced

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
    - OR we admit that scroll structures are *hierarchical imperative states*: they hold variable names, and order does not matter
  - Support for DnD actions on inloops

- Edit mode
  - Still just a means to perform the insertion and deletion rules
  <!-- - Localize commit mechanism for insertion:
    - Switching to Proof mode does not trigger a global commit anymore
    - Only the root node is marked as "grown" (inserted)
    - A `commit` button is added to grown nodes -->
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
  - Manipulating the global env
    - Search bar
    - Import (copy) a definition by dragging from the search results
        - If premiss non-empty, need to match it with some conclusion in the goal (should work similarly to deiteration)
        - Actually, import may be implemented by iteration once we implement evaluation

- Vertical generalization of the scroll
  - `ScrollData.inloops` becomes a tree instead of a list
  - Inloops can be iterated inside adjacent inloops

- Linear mode
  - When enabled, auto-performs self-justification of source after (de)iteration, and
    insertion/deletion only works when followed by (de)iteration through DnD.

# Brainstorming

- It seems we don't need to distinguish between judgment and constructor identifiers for inloops in the (bi-)intuitionistic case, since they will always be (de)iterated in the same scroll. Classical logic might require this distinction though, because inloops can become outloops (and vice versa); or we could just drop constructor identifiers altogether since there is no purpose in distinguishing between inloops and outloops, in the same way that one can go one-sided in classical sequent calculus (and thus drop the distinction between *term* and *continuation* variables).

- Maybe things would be simpler if there was only the cut construct, and inloops are just special cuts that are marked as "attached" or "continuations", i.e.:
  ```elm
  type alias Struct
    = List ONode
  
  type ONode
    = OForm Formula
    | OCut (List INode)
  
  type INode
    = IVal ONode
    | ICont Struct
  ```
  This gives $n$-scroll structures. To get generalized scroll structures:
  ```elm
  type alias Struct
    = List ONode
  
  type ONode
    = OForm Formula
    | OCut (List INode)
  
  type INode
    = IVal ONode
    | ICont (List INode)
  ```