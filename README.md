🍜 miso-from-html
===================
![Hackage](https://img.shields.io/hackage/v/miso-from-html.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
[![BSD3 LICENSE](https://img.shields.io/github/license/mashape/apistatus.svg)](https://github.com/dmjio/miso-from-html/blob/master/miso-from-html/LICENSE)

Convert HTML into [miso](https://github.com/dmjio/miso) `View` syntax.

This package uses [html-parse](https://github.com/bgamari/html-parse).

### Usage

Given some HTML

```html
<nav class="navbar" role="navigation">
  <div class="navbar-brand">
    <a class="navbar-item" href="https://bulma.io">
      <img src="https://bulma.io/images/bulma-logo.png" width="112" height="28">
      <a>ok<p>hey</p></a>
    </a>
  </div>
</nav>
```

Convert it to [miso](https://github.com/dmjio/miso) `View` syntax.

```bash
$ cabal run miso-from-html < index.html
```

Result

```haskell
nav_
    [ class_ "navbar"
    , role_ "navigation"
    ]
    [ div_ [ class_ "navbar-brand" ]
	[ a_
	    [ class_ "navbar-item"
	    , href_ "https://bulma.io"
	    ]
	    [ img_
		[ src_ "https://bulma.io/images/bulma-logo.png"
		, width_ "112"
		, height_ "28"
		]
	    , a_ []
		[ "ok"
		, p_ [][ "hey" ]
		]
	    ]
	]
    ]
```

### Limitations

Currently operates on a single top-level node, not on a list of nodes.

This is invalid since there is no single top-level parent node.

```html
<div>
    foo
</div>
<div>
   bar
</div>
```

This is valid

```html
<div>
  <div>
      foo
  </div>
  <div>
     bar
  </div>
</div>
```

Also, if your HTML isn't parsing, make sure it's valid, like `<img />` needs to be closed for example.

When in doubt, check the [W3C validation service](https://validator.w3.org/#validate_by_input).

### Test

```bash
$ nix-shell --run 'runghc Main.hs < index.html'
```
