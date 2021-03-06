# GenAPLS
First Elm project implementing a small web tool to calculate
APLS formulas for a given patient age and/or weight.

The application can be viewed at: [GenAPLS](http://genapls.azurewebsites.net).

# Build
This project is extremely simple to build or test when Elm
is installed.

Just do `elm-reactor --port=8080` in the terminal and navigate to
`localhost:8080/src/Main.elm`.

Otherwise use `elm-make src/GenAPLS.elm --output=generated/index.html`.
Which will create an index.html in the generated dir.

# Conventions

Commit messages with:

- feat: new feature
- fix: fix a bug or problem
- docs: document
- refactor: refactoring
- perf: improve performance
- test: add test
- chore: do a chore (build, libs, etc..)
