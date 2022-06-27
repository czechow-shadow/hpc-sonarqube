# hpc-sonarqube

The ``hpc-sonarqube`` package contains an executable and library codes
for generating [SonarQube](https://https://www.sonarqube.org) XML
coverage report from ``.tix`` and ``.mix`` files made with
[hpc](https://hackage.haskell.org/package/hpc). The generated report
is ready to be uploaded to SonarQube with other tools such as [Sonar
scanner](https://docs.sonarqube.org/latest/analysis/scan/sonarscanner).

The ``hpc-sonarqube`` executable can search ``.tix`` and ``mix`` files
under the directories made by the
[cabal-install](http://hackage.haskell.org/package/cabal-install) and
[stack](https://docs.haskellstack.org/en/stable/README/) build tools.
The executable also has options to explicitly specify the file paths
and directories for ``.tix`` and ``mix`` files, to support generating
reports with test data made by other build tools than
``cabal-install`` and ``stack``.


Installing
----------

### From Package Repository

To install with ``cabal-install``, run:

```console
$ cabal install hpc-sonarqube
```

To install with ``stack``, run:

```console
$ stack install hpc-sonarqube
```

QuickStart
----------

To illustrate an example, initializing sample project named
``my-project`` with ``cabal-install``:

```console
$ cabal --version
cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library
$ cabal init --simple --tests --test-dir=test -p my-project
```

Directory contents look like below:

```console
.
├── app
│   └── Main.hs
├── CHANGELOG.md
├── my-project.cabal
├── src
│   └── MyLib.hs
└── tests
    └── MyLibTest.hs
```

Run tests with coverage option:

```console
$ cabal test --enable-coverage
```

Write SonarQube coverage report to ``coverage.xml``:

```console
$ hpc-sonarqube cabal:my-project-test -X my-project -o coverage.xml
```

Show coverage report contents:

```console
$ cat coverage.xml
<coverage version="1">
  <file path="test/data/project1/src/Lib.hs">
    <lineToCover lineNumber="7" covered="true" />
    <lineToCover lineNumber="8" covered="true" />
    <lineToCover lineNumber="9" covered="false" />
  </file>
</coverage>
```


Examples
--------

### Showing help

Show usage information:

```console
$ hpc-sonarqube --help
```

### Project using cabal-install

Search under directory made by ``cabal-install`` , generating a report
for test suite named ``my-project-test``. Skip searching under the
directories with base name ``my-project``, and exclude modules named
``Main`` and ``Paths_my_project`` from the report. Note the use of
comma to separate multiple values for the ``-x`` option:

```console
$ hpc-sonarqube -X my-project -x Main,Paths_my_project cabal:my-project-test
```

### Project using stack

Search under directory made by ``stack`` for test suite named
``my-project-test``, show verbose information, and write output to
``coverage.xml``:

```console
$ hpc-sonarqube --verbose -o coverage.xml stack:my-project-test
```

### Project using stack, with multiple packages

Search under directory made by ``stack`` for combined report of
multiple cabal packages, and write output to ``coverage.xml``:

```console
$ hpc-sonarqube stack:all -o coverage.xml
```


Low-level examples
------------------

The following shows two examples for generating a test coverage report
of the ``hpc-sonarqube`` package itself without specifying the build
tool. One with using the build artifacts made by ``cabal-install``
Nix-style local build commands, and another with ``stack``.

### With cabal-install

First, run the tests with coverage option to generate ``.tix`` and
``mix`` files:

```console
$ cabal --version
cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library
$ cabal v2-configure --enable-test --enable-coverage
$ cabal v2-test
```

Then generate a SonarQube XML coverage data from the ``.tix`` and
``.mix`` files:

```console
$ proj=hpc-sonarqube-0.3.0.0
$ tix=$(find ./dist-newstyle -name $proj.tix)
$ mix=$(find ./dist-newstyle -name vanilla -print -quit)/mix/$proj
$ hpc-sonarqube --mix=$mix --exclude=Paths_hpc_sonarqube --out=coverage.xml $tix
```

The ``--out`` option specifies the output file to write the XML
report.


### With stack

Build the package and run the tests with coverage option:

```console
$ stack --numeric-version
2.5.1
$ stack build --test --coverage
```

As done in ``cabal-install`` example, specify the path of ``.tix`` and
``.mix`` files. Using ``path`` sub-command to get the local hpc root
directory and dist directory:

```console
$ hpcroot=$(stack path --local-hpc-root)
$ tix=$(find $hpcroot -name 'test-main.tix')
$ mix=$(stack path --dist-dir)/hpc
$ hpc-sonarqube --mix=$mix --exclude=Paths_hpc_sonarqube -o coverage.xml $tix
```


References
----------

- [HPC publication](http://ittc.ku.edu/~andygill/papers/Hpc07.pdf)
- [SonarQube import coverage](https://docs.sonarqube.org/latest/analysis/test-coverage/generic-test)
