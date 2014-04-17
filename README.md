# Cerbere

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
(cerbere-global-mode)
```

## Usage

These functions are available :
* `test-current-test`: launch unit tests for the current test in a file
* `test-current-file`: launch unit tests for the current file
* `test-current-project`: launch all unit tests

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
