# Docstring Formats

_Modified from [Codox documentation][docstring formats]._

By default, docstrings are rendered by Lodox as Markdown via [pandoc][].

As such, installing [pandoc][] is a prerequisite for using Lodox.

In a future version, you will be able to override this behaviour by specifying
an explicit format for your docstrings.

Markdown docstrings also support wikilink-style relative links, for referencing
other definitions. Definitions in the current module will be matched first, and
then Lodox will try to find a best match out of all the definitions it's
documenting.

N.B. Module-less definitions in `.lfe` files in the `include` directory,
e.g. [lodox-macros][], will also be included in the search.

```commonlisp
(defun bar (x)
  "See [[foo/2]] and [[baz:square/1]] for other examples."
  ...)
```

[docstring formats]: https://github.com/weavejester/codox#docstring-formats
[pandoc]: http://pandoc.org
[lodox-macros]: include/lodox-macros.lfe
