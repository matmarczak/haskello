# Haskello

## Requirements

`GHC==8.8.4`

## Usage

In order to use `haskello` you need to pass API
key and token as environment variables. Key and
token may be obtained with official trello
website: <https://trello.com/app-key>.

`haskello` expects that `TRELLO_KEY` and
`TRELLO_TOKEN` are both present in environment at
runtime.

For example:

    TRELLO_TOKEN=mySecretToken TRELLO_KEY=mySecretKey haskello
