aurquery
========

aurquery is a small custom tool for checking locally installed Arch Linux AUR packages for available upgrades. It scans a directory in your local file system (by default, `$HOME/aur`) for directories where you have unpacked AUR tarballs. It then checks the installed version of these packages against the current version available in the AUR, alerting you to those which have an available upgrade.
