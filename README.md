# Lodox

[![Travis CI][travis badge]][travis builds]
[![Release][tag badge]][latest release]
[![Erlang][erlang badge]][erlang downloads]
[![Documentation][doc badge]][docs]
[![MIT License][license badge]](LICENSE)

[travis builds]: https://travis-ci.org/lfe-rebar3/lodox
[travis badge]: https://travis-ci.org/lfe-rebar3/lodox.svg
[tag badge]: https://img.shields.io/github/tag/lfe-rebar3/lodox.svg
[latest release]: https://github.com/lfe-rebar3/lodox/releases/latest
[erlang badge]: https://img.shields.io/badge/erlang-R16B03%20%7C%20%E2%89%A517.5-red.svg
[erlang downloads]: http://www.erlang.org/downloads
[doc badge]: https://img.shields.io/badge/docs-100%25-green.svg
[docs]: http://lfe-rebar3.github.io/lodox
[license badge]: https://img.shields.io/badge/license-MIT-blue.svg


_Like [Codox](https://github.com/weavejester/codox) for [LFE](https://github.com/rvirding/lfe)._
Check out the [self-generated documentation](http://lfe-rebar3.github.io/lodox/).

Requires Erlang 18.x or later.

# Installation

First, make sure you have the [lfe-compile][] plugin as a dependency in your
project's `rebar.config`:

```erlang
{plugins, [
  %% ...
  {'lfe-compile', {git, "git://github.com/lfe-rebar3/compile", {tag, "0.5.0"}}}
]}.
```

Then in your project's `rebar.config`, include the [provider pre-hook][]:

```erlang
{provider_hooks, [{pre, [{compile, {lfe, compile}}]}]}
```

Finally, add Lodox to your `project_plugins` list.

```erlang
{project_plugins, [
  %% ...
  {lodox, {git, "git://github.com/lfe-rebar3/lodox.git", {tag, "0.15.0"}}}
]}.
```

[lfe-compile]: https://github.com/lfe-rebar3/compile
[provider post-hook]: https://www.rebar3.org/v3.0/docs/configuration#section-provider-hooks


# Usage

In order for Lodox to work, your project must first be compiled:

```sh
rebar3 compile
```

Then, to invoke Lodox, simply run:

```sh
rebar3 lfe lodox
```

Alternatively, you can `do` both at once:

```sh
rebar3 do compile, lfe lodox
```

If all goes well, the output will look something like:

    Generated lodox vX.Y.Z docs in /path/to/lodox/doc

And, as promised, [generated documentation][docs] will be in the `doc` (or
`{'output-path', "path/to/doc"}`) subdirectory of your project.


## Source Links

_Modified from [Codox documentation][Codox source links]._

If you have the source available at a URI and would like to have links to the
function/macro's source file in the documentation, you can set the
`​'source-uri'​` in your `rebar.config` as follows:

```erlang
{lodox, [
  {apps, [
    {my_app, [
      {'source-uri', "https://github.com/user/my_app/blob/{version}/{filepath}#L{line}"},
      {'output-path', "doc"}
    ]}
  ]}
]}.
```


The URI is a template that may contain the following keys:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">filepath</td>
<td class="org-left">the file path from the root of the repository</td>
</tr>


<tr>
<td class="org-left">line</td>
<td class="org-left">the line number of the source file</td>
</tr>


<tr>
<td class="org-left">version</td>
<td class="org-left">the version of the project</td>
</tr>
</tbody>
</table>

N.B. In order for `{version}` to work properly, you must add the corresponding
tag. For example, if your [`.app`][application resource file] file contains
`{vsn, "1.2.3"}` you must add the tag, `​"1.2.3"​`, to your repo.

[Codox source links]: https://github.com/weavejester/codox#source-links
[configuration parameter]: http://www.erlang.org/doc/design_principles/applications.html#id76014
[application resource file]: http://www.erlang.org/doc/design_principles/applications.html#id75484


## Docstring Formats

_Modified from [Codox documentation][docstring formats]._

By default, docstrings are rendered by Lodox as Markdown via [pandoc][].

As such, installing [pandoc][] is a prerequisite for using Lodox..

In a future version, you will be able to override this behaviour by specifying
an explicit format for your docstrings.

Markdown docstrings also support wikilink-style relative links, for referencing
other definitions. Definitions in the current module will be matched first, and
then Lodox will try to find a best match out of all the definitions it's
documenting.

N.B. Module-less definitions in `.lfe` files in the `include` directory,
e.g. [lodox-macros][], will also be included in the search.

```lfe
(defun bar (x)
  "See [[foo/2]] and [[baz:square/1]] for other examples."
  ...)
```

[docstring formats]: https://github.com/weavejester/codox#docstring-formats
[pandoc]: http://pandoc.org
[lodox-macros]: include/lodox-macros.lfe


# License

Lodox is licensed under [the MIT License](http://yurrriq.mit-license.org).

```text
The MIT License (MIT)
Copyright © 2015-2016 Eric Bailey <quasiquoting@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```

Significant code and inspiration from [Codox][]. Copyright © 2015 James Revees

Codox is distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

[Codox]: https://github.com/weavejester/codox
