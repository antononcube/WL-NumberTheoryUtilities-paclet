# NumberTheoryUtilities paclet

Wolfram Language Number Theory utility functions.

----

## Installation

To install this paclet in your Wolfram Language environment, evaluate this code:

```mathematica
PacletInstall["AntonAntonov/NumberTheoryUtilities"]
```

To load the code after installation, evaluate this code:

```mathematica
Needs["AntonAntonov`NumberTheoryUtilities`"]
```

-----

## Usage

```mathematica
s = SpiralLattice[11, "top-right"];
s2 = s /. {x_?PrimeQ :> Style[x, Red, Bold]};
Grid[s2]
```