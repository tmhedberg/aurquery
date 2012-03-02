aurquery
========

aurquery is a small custom tool for checking locally installed Arch Linux AUR packages for available upgrades. It scans a directory in your local file system (by default, `$HOME/aur`) for directories where you have unpacked AUR tarballs. It then checks the installed version of these packages against the current version available in the AUR, alerting you to those which have an available upgrade.

Unlike a full-fledged "AUR helper", this tool will not automatically build or update any packages. It merely displays the status of available updates for you to manually build and install with Pacman.

Building
--------

    cabal configure
    cabal build
    cabal install

An AUR package for aurquery is on my to-do list.

Known Issues
------------

Currently the tool determines the local version of the packages by spawning a bunch of Pacman processes and parsing their output. This is pretty inefficient. A better method would be to write FFI bindings to libalpm and query the local package database directly.
