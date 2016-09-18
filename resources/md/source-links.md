# Source Links

_Modified from [Codox documentation][Codox source links]._

If you have the source available at a URI and would like to have links to the
function/macro's source file in the documentation, you can set the
`'source-uri'` in your `rebar.config` as follows:

```erlang
{lodox, [
  {apps, [
    {my_app, [
      {'source-uri', "https://github.com/user/my_app/blob/{version}/{filepath}#L{line}"},
      {'output-path', "docs"}
    ]}
  ]}
]}.
```

The URI is a template that may contain the following keys:

| **Key**    | **Description**                               |
|------------|-----------------------------------------------|
| `filepath` | the file path from the root of the repository |
| `line`     | the number of the source file                 |
| `version`  | the version of the application                |

N.B. In order for `{version}` to work properly, you must add the corresponding
tag. For example, if your [`.app`][application resource file] file contains
`{vsn, "1.2.3"}` you must add the tag, `"1.2.3"`, to your repo.

[Codox source links]: https://github.com/weavejester/codox#source-links
[configuration parameter]: http://www.erlang.org/doc/design_principles/applications.html#id76014
[application resource file]: http://www.erlang.org/doc/design_principles/applications.html#id75484
