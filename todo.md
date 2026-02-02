# Next

- `Deiteration` actions
- `Open` actions
- Fix `Close` actions
- Fix `Reorder` actions

# Features

- Execution of actions
  - "Step" button that dequeues and executes the next action
  - The action at the head of the queue should always be highlighted at the location where it occurs in the goal
  - **Free execution**:
    - the user can execute actions in the order she wants by clicking directly on them in the goal. However, only actions which are _executable_ (and thus do not depend on other actions) can be executed:
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
    - OR we admit that scroll structures are _hierarchical imperative states_: they hold variable names, and order does not matter

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

- It seems we don't need to distinguish between judgment and constructor identifiers for inloops in the (bi-)intuitionistic case, since they will always be (de)iterated in the same scroll. Classical logic might require this distinction though, because inloops can become outloops (and vice versa); or we could just drop constructor identifiers altogether since there is no purpose in distinguishing between inloops and outloops, in the same way that one can go one-sided in classical sequent calculus (and thus drop the distinction between _term_ and _continuation_ variables).

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

## Names

### Variables

- Two options for shadowing:
  1. **Yes:** name uniqueness is only required locally or "horizontally", i.e. in the same area/address space. Then in order to still allow every (well-scoped) iteration, we should use a named De Bruijn representation, where a variable $(x, n)$ refers to the variable $x$ occurring in the $n$-th outer cut area. **We also need to ensure operations that insert or remove nodes update indices accordingly**. Frontend-wise, the index should be visible but unobtrusive, maybe using an on-demand disambiguation mechanism like source hover on highlighting. Then there is no shadowing stricto sensu, but we can achieve an observationally indistinguishable user experience by hiding the index $n$ if there is no bound variable with name $x$ in the $i$-th outer cut area for every $i < n$.
  2. **No:** name uniqueness is required for every union of areas/address spaces in the same scope. Then we do not need De Bruijn indices, but the burden of maintaining index consistency becomes that of maintaining scope-wise uniqueness. While it may actually be simpler to implement correctly (e.g. no shifting), it does deprive the user from some naming freedom. But some people argue that shadowing is just bad practice, so it may be wiser to go for the simplest option and see if anyone complains later on.

- Maintain uniqueness of same-scope bound variables in various operations
  - Function `bvu` (bound vars upward) that computes the bound variables available at a given `Path`
  - Function `bvd` (bound vars downward) that computes all the bound variables in scope of a given `Path`
  - Notice that $\mathsf{bv} = \mathsf{bvu} \cap \mathsf{bvd}$, where `bv` is the set of variables bound precisely in the area of the `Path`
  - All actions that introduce a new node (`Open`/`Insert`/`Iterate`):
    - can compute a fresh name automatically, or can ask the user for a name and deny it if it is already in `bvu ∪ bvd`
  - All actions that change the scope of a binder (`Close`)
    - can compute fresh names automatically for all bound vars in the inloop, or can ask the user to rename all those that are already in `bvd`
  - Renaming
    - check that the new name is not already in `bvu ∪ bvd`

### Non-semantic names

I want a notion of « frontend » name for nodes, that is orthogonal to the notion of node identifier (NodeId) living purely in the backend and invisible to the user.

So by default, nodes do not have a name. But still, we do not want to display the argumentation with arrows as in the diagrammatic notation for scroll nets, because it would make the layouting algorithm way too difficult to implement (or even specify). I imagine an interactive display mechanism: when the user hovers some justified target node, its source becomes highlighted. This supposes that it is visible on screen at the same time as the target. In case it is not, we could rely on the shelf mechanism that I plan to implement later. In a nutshell, the shelf is a scrollable (in the UI sense) area of the UI which holds all the nodes that are available in the currently focused subnet. This assumes that we have implemented the Navigation mode allowing to navigate scroll nets by zooming in or out of subnets, which I imagine implementing ideally as a ZUI. Then the source would always be somewhere in the shelf, and would first be scrolled to if not visible yet, then highlighted. One could also directly focus the subnet holding the source in the main view, indeed a form of « jump to definition ».

But we’re getting led astray here. The purpose of names is to give a more traditional, symbolic mechanism for tracking provenance of data/justification, just like variables. However since it is independent from the actual proof object stored internally, these names do not really hold any semantic value. This gives the user the freedom to name nodes however they want in principle, and we shall indeed refrain from implementing any restriction at first for the sake of simplicity. For instance, variable shadowing can trivially be done in this context. Later on, one might want to impose (or at least suggest) some good naming practices to the user, the most basic one being to avoid giving the same name to different nodes in the same subnet.

## Modularity

In our first implementation, we will only have one big program that cannot interact neither with the external world (i.e. "side-effects" like I/O, network, etc), nor even with other Scroller programs.

Later on, we will want to have a notion of **module** allowing to export and import functionalities from other programs. The right way to do so is very non-obvious, as it would (at least traditionally) crucially depends on how we manage identifiers and names.

One inspiration is how it is actually done in Elm with `export/import ... exposing (...)`. If we stick to the intended separation between backend identifiers and frontend names, then the content that is exposed is really just the identifier and its content.

Then comes the question of what to do with dependencies of the exposed content. A priori they should not be exposed themselves, but still be linked somehow so that evaluation does not get stuck.

Now let's consider an example. I import the addition function `add` from some external module, and just iterate it once without applying it. There are (at least) two possibilities for the semantics of import:

1. The original copy of `add` is inlined in the current program. Then evaluation will perform the copy and remove the dependency to the inlined copy. The implementation of `add` is visible to the user in the inlined copy before evaluation, and in both copies after.
2. An iterated copy of `add` is inlined in the current program, with a special kind of external dependency so that the `from` ID is not a dangling pointer. Then evaluation gets stuck, except if we implement a mechanism to fetch external dependencies. In that case, the implementation of `add` is visible to the user in neither of the copies before evaluation, and in both copies after.

It seems that in both cases, the implementation of `add` is revealed after evaluation. But in a traditional (functional) language, the implementation of `add` would still be hidden after evaluation. So basically, it seems there is no obvious way to have a separation between evaluation and linking phases.

I wonder how this is actually handled in proof assistants. For instance, it seems to me that every imported definition in Rocq can have its implementation exposed with `Print`, although `Compute` will not unfold the definition.

A trivial workaround is if `add` is a primitive function of Scroller. Then by "definition" (i.e. it is hardcoded in Scroller), it only evaluates when both arguments are given and the scroll is closed.
