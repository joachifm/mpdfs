# About

A [FUSE] filesystem for the [Music Player Daemon].

[FUSE]: http://fuse.sourceforge.net
[Music Player Daemon]: http://www.musicpd.org

# Installing

First you'll need to install [cabal-install].

Then install a recent version of [libmpd-haskell]:

    $ git clone git://github.com/joachifm/libmpd-haskell.git libmpd-haskell
    $ cd libmpd-haskell
    $ cabal install

And finally:

    $ git clone git://github.com/joachifm/mpdfs.git mpdfs
    $ cd mpdfs
    $ cabal install

[libmpd-haskell]: http://projects.haskell.org/libmpd
[cabal-install]: http://hackage.haskell.org/package/cabal-install

# Usage

    $ mkdir mpd
    $ mpdfs mpd
    $ cd mpd
    $ cat Stats/albums
    $ cd ..
    $ fusermount -u mpd

# Status

Currently only a small subset of the program's features have actually been
implemented, and much of the code is very bad. Don't expect this to work at all.

# Development

To contribute changes to MPDFS either email a patch to one of the devlopers or
create a fork of the [repository] and send a pull request (instructions <a href="http://help.github.com">here</a>)

[repository]: http://github.com/joachifm/mpdfs
[here]: http://help.github.com

# File system layout

* /Music/
* /Playlists/
    * Current/
* /Outputs/
* /Stats/
* /Status/

# Licence

GPL version 2 (see COPYING)

# Authors

Joachim Fasting \<joachim.fasting@gmail.com\>

Ben Sinclair \<ben.d.sinclair@gmail.com\>
