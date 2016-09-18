# Configuration

```erlang
{lodox, [
  {apps, [
    {my_app, [
      %% my_app config
    ]},
    {my_other_app, [
      %% my_other_app config
    ]}
  ]}
]}.
```

... where *config* consists of any number of key/value tuples with the following
valid keys:


| **Key**            | **Description**                   | **Default**     |
|--------------------|-----------------------------------|-----------------|
| `output-path`      | a project-relative path           | `"docs"`        |
| `excluded-modules` | a list of module names (atoms)    | `[]`            |
| `doc-files`        | a list of files or the atom `all` | `all`           |
| `doc-paths`        | a list of directories             | `["priv/docs"]` |

The documentation will be generated in `output-path`
and modules in `excluded-modules` will be excluded.

If `doc-files` is the atom `all`, Lodox will render files in supported
formats[<sup>1</sup>][1] in each directory in `doc-paths`. Otherwise, if
`doc-files` is a list of files, Lodox will render those that are in supported
formats[<sup>1</sup>][1], ignoring `doc-paths`.

<a id="footnote-supported-formats"></a>

<sup>1</sup>: Currently only Markdown, i.e. `*.m{d,arkdown}`.

[1]: #footnote-supported-formats
