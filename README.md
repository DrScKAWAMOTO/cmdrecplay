for Japanese
============

日本語のドキュメントを `doc.ja` ディレクトリ内に用意しました。

There are prepared the Japanese documents too in the `doc.ja` directory.

Cmdrecplay
==========

`Cmdrecplay` is a tool for a C/C++ checker of emacs flycheck, and emacs auto-complete.
This tool consist of `cmdrec` and `cmdplay`.
`cmdrec` examines makefile of your projects, then records compiler options for make
process, such as -D, -I, -W, -f, -std, etc., to a sqlite3 database `~/.cmdrec.db`.
`cmdplay` works a part of C/C++ checker of emacs flycheck, then refers this database,
and invoke compiler such as clang with correct options.

`Cmdrec` uses auxiliary `cmdskin` program for finding compiler options.
`Cmdskin` works as a execution skin of such compilers.

Sqlite3 database `~/.cmdrec.db` remembers histories of compiling, so, if you remove
this database file, then informations on the correct option will disappear.

Based on the results reported by `cmdplay`, we also attached the elisp code
`cmdplay-flycheck.el` for emacs flycheck, and `cmdplay-clang-async.el` for emacs auto-completion.

Ifendif
=======

`Ifendif` is a tool for reporting valid lines by `#if #endif directives`, with C/C++
language source codes. It uses cmdrecplay and clang's pre-processor as the backend
then refers to the macro definitions at compile time,
such as -D, -I, -W, -f, -std, etc. on command line,
and at current time,
such as #define, #undef in your source codes,
to judge validity.

So, no matter how complex your macro is, how complex your `if directive` is,
or how complex your make file is, `ifendif` correctly determines whether a line
is valid or invalid, just like at compile time.

Based on the results reported by `ifendif`, we also attached the elisp code
`cmdplay-ifendif.el` for grayed out in emacs.

Thanks
------

Thanks bear protocol written by Laszlo Nagy.
Old `cmdrecplay` used bear protocol, but new `cmdrecplay` does not use it !!

How to build
------------

cmdrec/cmdplay should be quite portable on UNIX OSs, it has been tested on FreeBSD,
Linux and OS X only.

### Prerequisites

1. ANSI **C compiler** to compile the sources.
2. **cmake** to configure the build process.
3. **pkg-config** to find dependencies during configure step.
4. **make** to run the build. Makefiles are generated by `cmake`.
5. **libconfig/libconfig-dev** to parse the config file. (Version shall be greater than 1.4)
6. **sqlite3/libsqlite3-dev** to link the executables.
7. **oniguruma/libonig2/libonig-dev** to link the executables.

### Build commands

It could be the best to build it in a separate build directory.

    mkdir build
    cd build
    cmake ..
    make all
    sudo make install # to install

You can configure the build process with passing arguments to cmake.
For example, debug configuration is the follows,

    mkdir build
    cd build
    cmake -DCMAKE_BUILD_TYPE=Debug ..
    make all


How to use
----------

After installation, append the following lines in your emacs startup file:

    (setq cmdplay-use-flycheck t)
    (setq cmdplay-use-clang-async t)
    (setq cmdplay-use-ifendif t)
    (require 'cmdplay)

To associate with the `M-x compile` command, write follows instead.

    (setq cmdplay-use-flycheck t)
    (setq cmdplay-use-clang-async t)
    (setq cmdplay-use-ifendif t)
    (setq cmdplay-use-compile t)
    (setq cmdplay-compile-command "LANG=C cmdrec -- make -k ")
    (require 'cmdplay)

Usually, per your project:

    cd <your_project_directory_which_contains_your_makefile>
    cmdrec -- make

If your project is cmake, you should do as follows instead:

    cd <your_project_directory_which_contains_your_CMakeLists.txt>
    mkdir build
    cd build
    cmdrec -s ~/cmdskin -- cmake ..
    cmdrec -s ~/cmdskin -- make all

The `--` separates the options from the build command. You will find the output sqlite3
file `~/.cmdrec.db` in your home directory.

You may specify `-s` option, in this case it's directory is used for `cmdskin` searching PATH.
If this directory already exists, `cmdrec` shall not update it's contents.
So delete the directory when the compiler options were not recorded correctly.

For more options, you may check by passing `-h` option to `cmdrec` or `cmdplay` commands.

From now on, you can edit your project's C/C++ source files by emacs, without specifying include pathes or macro defines explicitly for flycheck, auto completion, and ifendif.
If your include pathes or macro defines are changed in your makefiles, then you need to executing `cmdrec -- make` by `M-x compile` again.

Known issues
------------

Problem reports
---------------

If you find a bug in this documentation or elsewhere in the program or would
like to propose an improvement, please use the project's [github issue
tracker][ISSUES]. Please describing the bug and where you found it. If you
have a suggestion how to fix it, include that as well. Patches are also
welcome.

  [ISSUES]: https://github.com/DrScKAWAMOTO/cmdrecplay


Ifendif Algorithm
-----------------

1. Examine lines which contain `#if #ifdef #ifndef #else #elif #endif directives` in source file. Consider that source file is consist of regions which is divided by `if directive` lines.

example: test.c

    /* sample */
    #if defined(DEBUG)
    #define OK
    #else
    int main(int argc,char *argv[])
    {
      return 0;
    }
    #endif
    /* end */

Consider to

    region 1
    #if defined(DEBUG)
    region 2
    #else
    region 3
    #endif
    region 4

2. Copy source file to dummy file and insert magic line (TK-region-<n>) into the head of regions which is divided by `if directive` lines.

dummy file

    TK-region-1
    /* sample */
    #if defined(DEBUG)
    TK-region-2
    #define OK
    #else
    TK-region-3
    int main(int argc,char *argv[])
    {
      return 0;
    }
    #endif
    TK-region-4
    /* end */

3. Process dummy file by pre-processer (clang cc1) using of options (-D -f -U etc.) which is specified in compile process.

example: pre-processing without DEBUG

    TK-region-1
    
    
    
    
    
    TK-region-3
    int main(int argc,char *argv[])
    {
      return 0;
    }
    
    TK-region-4

4. Examine to see which magic lines remain in the pre-processing result, then output line numbers where `if directive` exists.

example:
V,2,H,4,V,9,V

This example means that:

    There are `if directives` in second, fourth, and nineth lines.
    It does not matter which directive is specifically written on which line.
    The area from the first line to the second line is valid.
    The area consisted of the third line is invalid.
    The area from the fourth line to the nineth line is valid.
    The area from the nineth line to the last line of file is also valid.


Author
------

* Dr.Sc.KAWAMOTO,Takuji (Ext)
* 川本 琢二（Ｅｘｔ）
* System Architect (Panasonic Advanced Technology Development Co., Ltd.)
* システム・アーキテクト (パナソニック アドバンストテクノロジー(株))
* Doctor of Science (Mathematics)
* 理学博士 (数学)
* Jazz Pianist
* ジャズ・ピアニスト
* My Representative Freeware Works: Prolog-TK over LISP,TwentyOne,DEDIT,TeX previewer,fontman,WiZ backup tool,nm.x,sort.x,hounds,assembler,dis-assembler,TCP/IP over ITRON,FullereneViewer,cmdrecplay,ifendif
