# Haskello

## Screencast

[![asciicast](https://asciinema.org/a/pMXqcFaFteXZ67dBjFNxl32Ws.svg)](https://asciinema.org/a/ZD10g59pEmoWC1zf4vb232rXY?loop=1&autoplay=1)

## Usage

In order to use `haskello` you need to pass API
key and token as environment variables. Key and
token may be obtained from official Trello
website: <https://trello.com/app-key>.

`haskello` expects that `TRELLO_KEY` and
`TRELLO_TOKEN` are both present in environment at
runtime.

For example:

    TRELLO_TOKEN=mySecretToken TRELLO_KEY=mySecretKey haskello

# Key bindings

For full list of bindings press `?` from within app.

Haskello implements quasi-vim bindings:

* `j/k`   - move down/up
* `h/l`   - enter/exit item
* `g/G`   - move top/bottom
* `J/K`   - drag item down/up
* `o/O`   - insert item below/above
* `i`     - edit item
* `s`     - save changes to `trello` server
* `d`     - delete item
* `<C-u>` - jump up
* `<C-d>` - jump down

# Code tour

## Dir tree

```
── src
    ├── Cursor.hs           - Datatype for screen (handles: new items, cursor position, items order etc.)
    ├── Event.hs            - Main dispatcher that handles inputs and acts on state
    ├── Haskello.hs         - Brick definitions, entrypoint to application
    ├── State               - Modules related to application state
    │   ├── AppState.hs     - Main AppState hadnling (init, operations on `LocalTrello`)
    │   ├── Changelist.hs   - Tracks changes done to items (new/edited/deleted items)
    │   ├── Editor.hs       - Text input for new/edited items
    │   └── Types.hs        - Essential state types
    ├── Trello              - All pieces needed for interaction with Trello API
    │   ├── Api.hs          - Wrapper around API endpoints
    │   ├── Types.hs        - Trello types
    └── UI.hs               - Rendering logic
```

## Design

Main idea is to store server state in `LocalTrello` datatype and changes
of state in `Changelist`. This way all changes are tracked and separated
from main storage.

Every view presents screen with related items (eg. list of boards or list of cards).
Screen is created on enter from `LocalTrello` storage type and `Changelist`.
Edited items are populated to `Changelist` and screen. `Changelist` is also used
as source of truth for server sync.
