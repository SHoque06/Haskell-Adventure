# Haskell Adventure

A text-based adventure game engine written in Haskell, built for CM12003 (Programming 1) at the University of Bath, with an overall module mark: **75%** (coursework 50%, exam 50%), which equates to a first.

## What it does

The game models a small world of locations connected on a map, with characters (NPCs) who can join your party and travel with you between locations. Talking to characters (individually or in groups) triggers branching dialogues that can change based on the current game state, and can lead to game-changing events.

## Key components

- **World model** — a graph of connected locations, with functions to manage which characters are present where, and who's currently in your party
- **Dialogue system** — a branching tree of choices and conditions, driving conversations and game events, run in IO
- **Game loop** — parses free-form player input (travel or talk to one or more characters) and drives the game forward
- **Robust input handling** — invalid input is handled gracefully rather than crashing
- **Solver primitives (partial)** — implements the building blocks for automatically solving the game: finding all dialogue paths to a resolution (`talk`), enumerating possible character groupings (`select`), and computing shortest routes across the map (`travel`). Combining these into a full automated walkthrough (`allSteps`, `solve`) wasn't completed.

## Tech

Pure Haskell (GHC), using only the standard libraries permitted by the coursework spec — no external dependencies.

## How to run

```
ghci Coursework.hs
*Main> game
```
