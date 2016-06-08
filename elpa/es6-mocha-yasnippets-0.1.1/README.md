# Emacs yasnippets for mocha / es6

A few useful snippets for Mocha using ES6 style.

Chai matcher integration is included see below.

# Installation

```
wget https://github.com/jasonm23/es6-mocha-yasnippets/releases/download/0.1.1/es6-mocha-yasnippets-0.1.1.tar
```

If you downloaded to `~` you can install from Emacs:

```
M-x package-install-file
```

choose: `~/es6-mocha-yasnippets-0.1.1.tar`

## BDD snippets

- `desc` <kbd>TAB</kbd>

    describe block

- `befr` <kbd>TAB</kbd>

    before / beforeEach block

- `aftr` <kbd>TAB</kbd>

    after / afterEach block

- `it` <kbd>TAB</kbd>

    it block

## xUnit snippets

- `test` <kbd>TAB</kbd>

    xUnit style flat test

- `suite` <kbd>TAB</kbd>

    xUnit style test suite

## Expect & Chai Matchers/Chains snippets

- `ex` <kbd>TAB</kbd>

    expect

- `m` <kbd>TAB</kbd>

    all chai matchers/chains (select from the list, **ido/flx** is highly
    recommended as your yas dropdown style)

    - `.to`
    - `.be`
    - `.been`
    - `.is`
    - `.that`
    - `.which`
    - `.and`
    - `.has`
    - `.have`
    - `.with`
    - `.at`
    - `.of`
    - `.same`
    - `.not`
    - `.deep`
    - `.any`
    - `.all`
    - `.a(type)`
    - `.include(value)`
    - `.ok`
    - `.true`
    - `.false`
    - `.null`
    - `.undefined`
    - `.NaN`
    - `.exist`
    - `.empty`
    - `.arguments`
    - `.equal(value)`
    - `.eql(value)`
    - `.above(value)`
    - `.least(value)`
    - `.below(value)`
    - `.most(value)`
    - `.within(start, finish)`
    - `.instanceof(constructor)`
    - `.property(name, [value])`
    - `.ownProperty(name)`
    - `.ownPropertyDescriptor(name[, descriptor[, message]])`
    - `.length`
    - `.lengthOf(value[, message])`
    - `.match(regexp)`
    - `.string(string)`
    - `.keys(key1, [key2], […])`
    - `.throw(constructor)`
    - `.respondTo(method)`
    - `.itself`
    - `.satisfy(method)`
    - `.closeTo(expected, delta)`
    - `.members(set)`
    - `.oneOf(list)`
    - `.change(function)`
    - `.increase(function)`
    - `.decrease(function)`
    - `.extensible`
    - `.sealed`
    - `.frozen`

## Contributing

Your contributions are welcome, please submit a pull request to add
new snippets etc. or raise issues / corrections / ask questions via
Github issues.
