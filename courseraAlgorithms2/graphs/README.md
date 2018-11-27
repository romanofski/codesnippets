# Graphs

This repository implements a graph library and common algorithms. It produces two executables for testing the library.

## Building

To build the library simply use the Haskell tool [stack](https://docs.haskellstack.org/en/stable/README/):

```
$ stack build
```

## Calculating shortest path

One tool allows to calculate the shortest path on a map given by a JSON file.

To get a general usage help:

```
$ stack exec shortestpath -- --help
```

### Examples

You can simply calculate the shortest path by running:

```
$ stack exec shortestpath tests/data/map.json '{"start":"A", "end":"F" }'
"{\"distance\":360}"
```

More elaborate examples perform operations on the map itself before querying.
The following example, uses the map.json for initialisation, calculates the
shortest path from A to J, add additional points I and J from point F and
reduces the route weight from A to B to 20.

```
$ stack exec shortestpath tests/data/map.json '{"start":"A", "end":"J" }' '{ "map": [{ "F": {"I":70, "J":150} }]}' '{ "A": {"B":20} }'
```
