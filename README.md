# Small-Haskell

An implementation of the semantics of the language Small using Haskell.

## Build and Running

### Dependencies

First Git and Stack need to be installed. This can be done on Ubuntu with:
```sh
sudo apt install git haskell-stack
```

For other platforms see https://git-scm.com/downloads and https://docs.haskellstack.org/en/stable/install_and_upgrade/.

It might also be necessary to update Stack with:
```sh
stack upgrade
```

### Clone Repository

The git repository can be cloned with:
```sh
git clone https://github.com/Cameron27/small-haskell
cd small-haskell
```

### Build

The project can be build with Stack using the command:
```sh
stack build
```
This may take a while the first time as all the Haskell packages need to be downloaded and build.

### Run

If you wanted to run the Small program `example.sm` then you would use the command:
```sh
stack run example.sm
```
This will also rebuild the project if any changes have been made.

### Tests

The test suite can be run using the command:
```sh
stack test
```
This will also rebuild the project if any changes have been made.

The files that are being used for testing are all in the directory `./test/tests/`.