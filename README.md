# Spatial voter modeling in Haskell

This is an implementation of a spatial Mixture-of-Zipf-Gaussians voter model in
Haskell, as well as tools for simulation different voting systems and analyzing
their results on a reasonably realistic voting population.

## Example

The following command runs a single simulation involving 10 candidates and
100,000 voters.

```bash
cabal run exe:spatial-voting -- --candidates=10 --voters=100000
```

A visualization will be written showing the voting populationand the candidates
on a low-dimensional projection of the voter model.  Keep in mind that the full
voter model is 100-dimensional, so this is only a rough approximation, but the
dimensions of highest variation are, in general, included here.

![Voter model visualization](docs/points.svg)

The output will also contain a summary of the results:

```
Util: [[9],[10],[3],[6],[2],[4],[7],[8],[1],[5]]
Cond: [[2,10,3,9,6],[4],[7],[1],[8],[5]]
nIRV: [[10],[9],[2],[5],[6],[3],[7],[8],[1],[4]]
tIRV: [[10],[9],[2],[5],[3],[6],[1,7],[8],[4]]
nPlu: [[2],[10],[5],[6],[3],[9],[8],[7],[1],[4]]
tPlu: [[2],[10],[6],[9],[5],[3],[4],[8],[1],[7]]
nBrd: [[10],[9],[3],[2],[6],[4],[7],[8],[1],[5]]
tBrd: [[9],[10],[3],[6],[4],[2],[7],[8],[1],[5]]
nRng: [[9],[10],[3],[6],[2],[4],[7],[8],[1],[5]]
tRng: [[9],[10],[6],[4],[2],[7],[3],[8],[1],[5]]
nApp: [[9],[4],[3],[10],[6],[2],[7],[1],[8],[5]]
tApp: [[9],[10],[2],[3],[6],[5],[8],[7],[1],[4]]
nStr: [[10],[9],[3],[6],[2],[4],[7],[8],[1],[5]]
tStr: [[10],[4],[6],[9],[7],[2],[3],[8],[1],[5]]
```

These are various election systems, and the results of an election run by each
system on the simulated population.  The specific rankings are:

* `Util`: a measure of which candidates voters will be happiest with, according
  to the model.  It's not a voting system, but it's a useful baseline for
  comparing voting systems.
* `Cond`: the Condorcet ranking, which is the ranking of candidates that would
  win in a head-to-head election against every other candidate.  Note that ties
  are expected (though not extremely common) because of the nature of Condorcet
  elections.
* `nIRV` and `tIRV`: Instant runoff rankings, with naive and tactical voting.
* `nPlu` and `tPlu`: Plurality rankings, with naive and tactical voting.
* `nBrd` and `tBrd`: Borda rankings, with naive and tactical voting.
* `nRng` and `tRng`: Range voting rankings, with naive and tactical voting.
* `nApp` and `tApp`: Approval voting rankings, with naive and tactical voting.
* `nStr` and `tStr`: STAR voting rankings, with naive and tactical voting.

Finally, we have some measures of similarity between the voting systems: how
often they agree on a winner, and their Spearman rank correlation for the
entire ranking of candidates from start to end.  Since we only simulated a
single election, the winner agreement matrix contains only 0s and 1s.

```
Winner Agreement:
      Util  Cond  nIRV  tIRV  nPlu  tPlu  nBrd  tBrd  nRng  tRng  nApp  tApp  nStr  tStr
Util: 1.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000 1.000 1.000 1.000 0.000 0.000
Cond: 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
nIRV: 0.000 0.000 1.000 1.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000
tIRV: 0.000 0.000 1.000 1.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000
nPlu: 0.000 0.000 0.000 0.000 1.000 1.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
tPlu: 0.000 0.000 0.000 0.000 1.000 1.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
nBrd: 0.000 0.000 1.000 1.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000
tBrd: 1.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000 1.000 1.000 1.000 0.000 0.000
nRng: 1.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000 1.000 1.000 1.000 0.000 0.000
tRng: 1.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000 1.000 1.000 1.000 0.000 0.000
nApp: 1.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000 1.000 1.000 1.000 0.000 0.000
tApp: 1.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000 1.000 1.000 1.000 0.000 0.000
nStr: 0.000 0.000 1.000 1.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000
tStr: 0.000 0.000 1.000 1.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 1.000 1.000
```

However, the correlation matrix gives more detailed information.

```
Correlation Matrix:
      Util  Cond  nIRV  tIRV  nPlu  tPlu  nBrd  tBrd  nRng  tRng  nApp  tApp  nStr  tStr
Util: 1.000 0.867 0.588 0.588 0.321 0.576 0.976 0.988 1.000 0.867 0.855 0.758 0.988 0.709
Cond: 0.818 0.806 0.394 0.394 0.394 0.588 0.806 0.794 0.818 0.673 0.673 0.539 0.842 0.636
nIRV: 0.588 0.515 1.000 0.976 0.842 0.806 0.624 0.503 0.588 0.491 0.212 0.927 0.600 0.261
tIRV: 0.588 0.515 0.976 1.000 0.806 0.758 0.636 0.503 0.588 0.430 0.248 0.915 0.600 0.200
nPlu: 0.321 0.345 0.842 0.806 1.000 0.855 0.406 0.212 0.321 0.176 -0.079 0.758 0.370 0.042
tPlu: 0.576 0.636 0.806 0.758 0.855 1.000 0.624 0.503 0.576 0.539 0.321 0.782 0.600 0.382
nBrd: 0.976 0.830 0.624 0.636 0.406 0.624 1.000 0.952 0.976 0.830 0.806 0.770 0.988 0.709
tBrd: 0.988 0.818 0.503 0.503 0.212 0.503 0.952 1.000 0.988 0.879 0.903 0.673 0.976 0.758
nRng: 1.000 0.867 0.588 0.588 0.321 0.576 0.976 0.988 1.000 0.867 0.855 0.758 0.988 0.709
tRng: 0.867 0.794 0.491 0.430 0.176 0.539 0.830 0.879 0.867 1.000 0.806 0.552 0.855 0.903
nApp: 0.855 0.770 0.212 0.248 -0.079 0.321 0.806 0.903 0.855 0.806 1.000 0.394 0.818 0.733
tApp: 0.758 0.685 0.927 0.915 0.758 0.782 0.770 0.673 0.758 0.552 0.394 1.000 0.745 0.261
nStr: 0.988 0.818 0.600 0.600 0.370 0.600 0.988 0.976 0.988 0.855 0.818 0.745 1.000 0.745
tStr: 0.709 0.564 0.261 0.200 0.042 0.382 0.709 0.758 0.709 0.903 0.733 0.261 0.745 1.000
```
