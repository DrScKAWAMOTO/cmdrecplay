cmdrecplay
==============

'Cmdrecplay' is a tool for a C/C++ checker of emacs flycheck.
This tool consist of 'cmdrec' and `cmdplay`.
`cmdrec` examines makefile of your projects, then records compiler options for make
process, such that -D, -I, -W, -f, -std, etc., to a sqlite3 database ``~/.cmdrec.db''.
`cmdplay` works a part of C/C++ checker of emacs flycheck, then refers this database,
and invoke compiler such as clang with correct options.
Sqlite3 database ``~/.cmdrec.db'' remembers histories of compiling, so, if you remove
this database file, then informations on the correct option will disappear.

Thanks, ``cmdrecplay'' uses bear protocol written by L���szl��� Nagy.
Bear's concept is a approach system call hacking by LD_PRELOAD.
Bear uses the JSON compilation database.
For more detail, see https://github.com/rizsotto/Bear .

How to build
------------

cmdrec/cmdplay should be quite portable on UNIX OSs, it has been tested on FreeBSD,
Linux and OS X only.

### Prerequisites

1. ANSI **C compiler** to compile the sources.
2. **cmake** to configure the build process.
3. **pkg-config** to find dependencies during configure step.
4. **make** to run the build. Makefiles are generated by `cmake`.
5. **libconfig** to parse the config file. (Version shall be greater than 1.4)
6. **sqlite3** for link the executables.
6. **oniguruma4** for link the executables.

### Build commands

It could be the best to build it in a separate build directory.

    mkdir build
    cd build
	cmake ..
    make all
    make package # to make packages
    sudo make install # to install

You can configure the build process with passing arguments to cmake.
For example, debug configuration is the follows,

    mkdir build
    cd build
	cmake -DCMAKE_BUILD_TYPE=Debug ..
    make all


How to use
----------

After installation the usage is like this:

    cmdrec -- make

The `--` separate the parameters from the build command. The output sqlite3 file
``.cmdrec.db'' found in your home directory.

For more options you can check the man page or pass `-h` parameter.


Known issues
------------

Compiler wrappers like [ccache][CCACHE] and [distcc][DISTCC] could cause
duplicates or missing items in the compilation database. Make sure you have
been disabled before you run Bear.

  [CCACHE]: http://ccache.samba.org/
  [DISTCC]: http://code.google.com/p/distcc/


Problem reports
---------------

If you find a bug in this documentation or elsewhere in the program or would
like to propose an improvement, please use the project's [github issue
tracker][ISSUES]. Please describing the bug and where you found it. If you
have a suggestion how to fix it, include that as well. Patches are also
welcome.

  [ISSUES]: https://github.com/rizsotto/Bear/issues
