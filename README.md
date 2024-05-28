<h1 align="center">
  <img src="/assets/tie.svg" width="300" />
  <img referrerpolicy="no-referrer-when-downgrade" src="https://static.scarf.sh/a.png?x-pxid=bc48832f-871e-4165-8f5c-539748589cbe" />
</h1>

Tie allows generation of Haskell server stubs from
[OpenAPI (v 3.x) specifications](https://swagger.io/specification/).

If you are looking for a generator for Haskell client code, check out the
[Haskell-OpenAPI-Client-Code-Generator](https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator)
project.

## Usage

```
$ tie --help
tie - openapi3 server code generator

Usage: tie [-o|--output DIR] [--module-name MODULE] [--package-name PACKAGE]
           [--extra-package PACKAGE] FILE

  Generate a Haskell server from an OpenAPI3 specification

Available options:
  -o,--output DIR          The directory output (default: "out")
  --module-name MODULE     Name of the generated top level module
                           (default: "OpenAPI")
  --package-name PACKAGE   Name of the generated cabal project
                           (default: "open-api")
  --extra-package PACKAGE  Extra packages to include in the generated cabal
                           project
  FILE                     OpenAPI specification file
  -h,--help                Show this help text
```

## Example

See [`example/`](/example) for the ubiquitous OpenAPI Petstore example.

## Installation

### Building from source

Below are the steps to install Tie using the Cabal build tool.

First, you need to clone the repository

```bash
$ git clone https://github.com/scarf-sh/tie.git
$ cd tie
```

Then, you need to build it using cabal:

```bash
$ cabal build exe:tie
```

Y ou can install the executable with using:

```bash
$ cabal install .
```

## Formatting

To pass the lint workflow run

```
$ git ls-files | grep -v 'example/' | grep \.hs | xargs ./ormolu --mode=inplace
```

to format the Haskell files accordingly using Ormolu.

## Community & Contact

Feel free to join on us on our
[community Slack](https://tinyurl.com/scarf-community-slack) (`#tie` channel)!

## License

This program is under the terms of the [Apache License v2.0](/LICENSE).

## Authors

Tie is originally made and currently sponsored by [Scarf](https://scarf.sh)
among other [contributors](https://github.com/scarf-sh/tie/graphs/contributors).
