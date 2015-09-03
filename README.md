# LoL Item Set Forge

Generate [Pareto optimal](https://en.wikipedia.org/wiki/Pareto_efficiency) League of Legend item sets.

Optimal item sets are an [multi-objective optimisation problem](https://en.wikipedia.org/wiki/Multi-objective_optimization), that we try to solve using [simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing).

## Installation

1. Install [Leiningen](http://leiningen.org/#install).
2. run `lein figwheel`
3. [Profit](http://localhost:3449)

## Usage

1. Select your champion
2. Select which objectives to optimise for
3. Select the champion level and number of items
4. Generate build
5. Check/modify/regenerate
6. Add to set
7. Go to #2
8. Download
9. Save in `LoL/Config/Champions/<champion>/Recommended/<name>.json`
10. Play!

## Objectives

### Simple

* Ability power
* Mana
* Movement speed

### Compound

* Physical damage per second (DPS): The amount of auto-attack damage per second, taking into account attack speed and chance to critical strike.
* Life stolen per second: The amount of life stolen given your DPS. Does not take into account enemy armour.
* Poke per minute: Champion defined damage of some ability, taking into account cool-down, mana (regen) and AP/AD scaling factor.
* Burst: Potential damage of a champion defined ability combo.
* Effective Health (AP/AD): A combination of health and armour/magic-resist.

Poke/burst are currently only implemented for

* Singed

## How to make good builds

**Item knowledge, item knowledge, item knowledge!** By building this tool, my builds have improved significantly. Not because this tool generates world-class builds, but because by building it, I learned a lot about item stats and their interactions.

A lot of item builds depend on your opponents and complex interactions between champions and item passives. This tool models none of that.

With that said, this tool is a great way to analyse and experiment with different items and custom/off-builds.

### Try a few times

This tool is fairly random, so clicking generate a few times will give you different trade-offs between your selected objectives.

### Generate small sets

While it’s fun to generate 6 agressive items, in most games you do not reach full-build and maybe build some defense too. Try to generate some early/mid game sets with two or three items.

### Generate specific sets

You could generate one set that contains all your objectives, but you need to be flexible in-game. Try to generate a few sets for different scenarios.

### Customise

This tool does not model everything and certainly not your specific play-style and taste. Change a few items in the generated build, and see how they influence the statistics.

## I'm bored!

Click the troll button. This will pick a random champion and objective, and generate a build.
All you have to do is play it. Best used with friends, to not upset your team mates with your AD Thresh.

## Disclaimer

LoL Item Set Forge isn’t endorsed by Riot Games and doesn’t reflect the views or opinions of Riot Games or anyone officially involved in producing or managing League of Legends. League of Legends and Riot Games are trademarks or registered trademarks of Riot Games, Inc. League of Legends © Riot Games, Inc.

## License

Copyright © 2015 Pepijn de Vos

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
