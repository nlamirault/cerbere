# Cerbere

[![Build Status](https://travis-ci.org/nlamirault/cerbere.svg?branch=0.1.0)](https://travis-ci.org/nlamirault/cerbere)

*Cerbere* is a a global mode for [TDD](http://en.wikipedia.org/wiki/Test-driven_development) in Emacs. Some backends are available:
* [tox](https://pypi.python.org/pypi/tox)
* [go test](http://golang.org/pkg/testing/)
* [phpunit](http://phpunit.de/)

## Installation

The recommended way to install `cerbere` is via [MELPA](http://melpa.milkbox.net/):

    M-x package-install cerbere

or [Cask](https://github.com/cask/cask):

    (depends-on "cerbere")

In your `.emacs` file, add this :

```lisp
(add-hook 'python-mode-hook 'cerbere-mode)
(add-hook 'go-mode-hook 'cerbere-mode)
(add-hook 'web-mode-hook 'cerbere-mode)
```

## Keymap

Keybinding           | Description
---------------------|------------------------------------------------------------
<kbd>C-c c t</kbd>   | launch the current unit test
<kbd>C-c c f</kbd>   | launch unit tests of the current file
<kbd>C-c c p</kbd>   | launch all unit tests for current project


## Development

### Cask

`cerbere` use [Cask](https://github.com/cask/cask) (>=0.6) for
dependencies management. Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

Launch unit tests :

    $ make clean test


## Support / Contribute

See [here](CONTRIBUTING.md)


## License

Scame is released under the [MIT License](LICENSE)


## Changelog

A changelog is available [here](ChangeLog.md).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>
