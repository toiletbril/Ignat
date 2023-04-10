# Ignat

Ignat is a bot written in Haskell using [discord-haskell](https://hackage.haskell.org/package/discord-haskell) package. I have no idea what I am doing.

## What does it do

- Bot prefix is `%`. Available commands:
    - `haskell`

- Markov Chains
    - Are not persistent yet. Meaning each restart will reset It's dictionary.
    - Try to @tag the bot and tell it something.

## Quickstart

- Create an `.env` file in the root folder with your bot's token on the first line.

- Change `botId` and `botPrefix` in [App.hs](./src/App.hs) to your desired values.

- Then run it:
```console
$ stack build
$ stack run
```
