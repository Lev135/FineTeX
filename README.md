<img src="https://user-images.githubusercontent.com/35816057/173631268-738c0e2d-2c27-4575-a6dd-577df144db88.png" alt="FineTeX" width="120"> &nbsp; language to be translated into LaTeX
===

FineTeX is just a simple (for the moment) language with more flexibility in 
commands definitions, then pure LaTeX.
However translator from FineTeX generates normal LaTeX code,
that can be used as you wish.

Features available at the moment
---
- macros-like commands consisted of arbitrary unicode symbols
- fine document structure, based on indentation rules

Not to be unfounded, here are some examples of how FineTeX and LaTeX code looks like:

> Source FineTeX code
```
The function `f` has a limit `y` when `x -> x_0` iff
  > ∀ϵ > 0 ∃δ > 0 : ∀x (|x - x_0| < δ => |f(x) - y| < ϵ)
```
> Produced LaTeX code
```tex
The function $f$ has a limit $y$ when $x \rightarrow x_0$ iff
\begin{align}
  \forall\epsilon > 0 \exists\delta > 0 : \forall x (|x - x_0| < \delta \Rightarrow |f(x) - y| < \epsilon)
\end{align}
```

Features to be realized in the very near future
---
- context-checks for commands: it will be possible to specify some commands to be used only with spaces
  or linebreaks surrounding them
- variables for more flexibility with commands. It will be possible to check every open bracket is closed
  using this feature

Getting started
---
See page on [Wiki](https://github.com/Lev135/latex-generator/wiki#getting-started)

Contributing
---
I'll be very glad if someone wants to contribute to this project.
All constructive ideas language features are welcome &mdash; you can open issue at 
[github](https://github.com/Lev135/latex-generator/issues).
Also the project really needs examples and guides. 

Source code can be cloned from [github repo](https://github.com/Lev135/latex-generator).
For running translation execute:
```
    cabal new-update
    cabal new-run texgen -- examples/example.ttex
```
