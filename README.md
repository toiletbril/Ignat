# Ignat

Ignat is a bot written in Haskell using [discord-haskell](https://hackage.haskell.org/package/discord-haskell) package.

## What does it do

- Bot prefix is `%`. Available commands:
    - `haskell`

- Markov Chains
    - Are not stored in memory yet. Meaning each restart will reset it's dictionary.
    - Try to @tag the bot and tell it something.

## Quickstart

- Create an `.env` file in the root folder with your bot's token on the first line.

- Then run it:
``` console
$ stack build
$ stack run
```
