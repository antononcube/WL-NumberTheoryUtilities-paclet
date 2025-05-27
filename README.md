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

-----

## References

### Original Source References and Attributions

* [GitHub - antononcube/Raku-Math-NumberTheory: Raku package with number theory functions.](https://github.com/antononcube/Raku-Math-NumberTheory)

### Links

* Anton Antonov, ["Primitive roots generation trails"](https://community.wolfram.com/groups/-/m/t/3442027), (2025), Wolfram Community.

* [Number theory neat examples in Raku (Set 1) - YouTube](https://www.youtube.com/watch?v=wXXWyRAAPvc)

* [Number theory neat examples in Raku (Set 2) - YouTube](https://www.youtube.com/watch?v=sMwuGVvkLkU)

* [Number theory neat examples in Raku (Set 3) - YouTube](https://www.youtube.com/watch?v=6uCIoonlybk)