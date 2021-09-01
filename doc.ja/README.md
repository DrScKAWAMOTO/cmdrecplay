cmdrecplay
==========

`cmdrecplay` は、`emacs` の `flycheck`, `auto-complete`, `company` に対応したツールです。
また、ifendif グレイアウト機能も持ち合わせています。
このツールには `cmdrec`, `cmdplay` コマンドが含まれています。

`cmdrec` ツールはプロジェクトの makefile から以下のコンパイル時オプションを `sqlite3` データベースに記憶します。

    1) マクロ定義(-Dmacro=valなど)
    2) インクルードディレクトリ指定(-Iincludeなど)
    3) ウォーニング指定(-Wallなど)
    4) その他(-f -std= など)


`cmdplay` は、`emacs` の「コード補完」「on the fly チェック」そして「`#if #endif` によるソースコードの無効表示」を実施する為のツールです。データベースから上記オプションを取得して `clang` などのツールを起動します。

対象コンパイラ

    gcc
    g++
    clang
    clang++

名前が `cc` である `gcc` や `c++` である `g++` も OKです。

`cmdrec` はコンパイラオプションを取得するために補助的なプログラムである `cmdskin` を使用します。`cmdskin` はコンパイラ類実行時の薄皮として働きます。

`sqlite3` データベースである `~/.cmdrec.db` にはコンパイルの履歴が記録されています。
従ってこのファイルを貴方が手動で削除してしまうと、これまでのコンパイル履歴がなくなります。

`cmdplay` の実行結果に基づいて `emacs` の `flycheck`, `auto-complete`, `company` と連携するための emacs lisp ファイルを添付しています。`cmdplay-flycheck.el`, `cmdplay-clang-async.el`, `company-cmdplay.el` です。

ifendif
=======

コンパイル時の -D, -I, -W, -f, -std などのオプションを使用して `#if #else #elsif #endif #ifdef #inde ディレクティブ` によってソースコードのどの行が無効化されているかを調べます。

従って貴方がどのように複雑にマクロを記述したとしても、`#if` などディレクティブがどのように複雑に書かれていても、また、貴方のメイクファイルがどのように複雑であっても、`ifendif` は、コンパイル時にどの行が無効であったかを正確に検出します。

`ifendif` の実行結果に基づいて `emacs` で無効行をグレイアウトするための emacs lisp ファイルを添付しています。`cmdplay-ifendif.el` です。

感謝
====

Laszlo Nagy さん作の `bear protocol` に感謝します。古い `cmdrecplay` は `bear protocol` を使用していました。しかし、既に最新の `cmdrecplay` は `bear protocol` を使用していません。

ビルド
======

`cmdrecplay` は汎用の UNIX OS で動作するように設計されています。
そして、FreeBSD, Linux, OS X のみでテストしました。

### 前提条件

1. ソースコードをコンパイルするために ANSI C コンパイラ
2. ビルドプロセスをコンフィギュレーションするために cmake
3. コンフィギュレーション中に依存関係を調べるために pkg-config
4. ビルドを実行するために make (cmake によって自動的に Makefile が生成されます)
5. コンフィギュレーションファイルを解析するために libconfig もしくは libconfig-dev (バージョンは 1.4 以上が必要です)
6. 実行ファイルをリンクするために sqlite3 もしくは libsqlite3-dev
7. 実行ファイルをリンクするために oniguruma, libonig2, もしくは libonig-dev
8. flycheck,company,auto-complete を使うために clang のバージョン 3.0 以上

### ビルド方法

ビルドするディレクトリは別に分けると良いです。

    mkdir build
    cd build
    cmake ..
    make all
    sudo make install # インストールする

`cmake` に引数を与えてコンフィギュレーションすることも出来ます。
例えば、debug コンフィギュレーションでビルドするには以下のようにします。

    mkdir build
    cd build
    cmake -DCMAKE_BUILD_TYPE=Debug ..
    make all

使用方法
--------

インストールが終わったら、次の4行を貴方の emacs スタートアップファイルに追加してください。
    (setq cmdplay-use-flycheck t)
    (setq cmdplay-use-clang-async t)
    (setq cmdplay-use-ifendif t)
    (require 'cmdplay)

もし、`M-x compile` と連携したければ代わりに以下の6行をスタートアップファイルに追加します。

    (setq cmdplay-use-flycheck t)
    (setq cmdplay-use-clang-async t)
    (setq cmdplay-use-ifendif t)
    (setq cmdplay-use-compile t)
    (setq cmdplay-compile-command "LANG=C cmdrec -- make -k ")
    (require 'cmdplay)

`auto-complete` ではなくて `company` を使う場合は、

    (setq cmdplay-use-flycheck t)
    (setq cmdplay-use-company)
    (setq cmdplay-use-ifendif t)
    (require 'cmdplay)

の4行もしくは

    (setq cmdplay-use-flycheck t)
    (setq cmdplay-use-company)
    (setq cmdplay-use-ifendif t)
    (setq cmdplay-use-compile t)
    (setq cmdplay-compile-command "LANG=C cmdrec -- make -k ")
    (require 'cmdplay)

の6行です。

後は、プロジェクトごとに以下を実行します。

    cd <makefile を含んだプロジェクトディレクトリ>
    cmdrec -- make

cmake プロジェクトの場合は、以下のようにします。

    cd <CMakeLists.txt を含んだプロジェクトディレクトリ>
    mkdir build
    cd build
    cmdrec -s ~/cmdskin -- cmake ..
    cmdrec -s ~/cmdskin -- make all

`--` セパレータの後に、ビルドコマンドラインを続けて書きます。`cmdrec` を起動すると、貴方のホームディレクトリの下に `~/.cmdrec.db` というファイルが出来上がります。

`-s` オプションを指定することで、指定したディレクトリを `cmdskin` のサーチパスに利用することが出来ます。このディレクトリが既に存在する場合は、`cmdrec` はその中身を更新しません。
従って、中身が古くなって正しく動作しなくなった場合は、予めこのディレクトリを削除してから `cmdrec` を再起動して下さい。

他のオプションについては `cmdrec` や `cmdplay` に `-h` オプションを付けて起動してみてください。

この後は、特に `cmdrec` を起動しなくても、emacs でプロジェクトの C/C++ ソースコードを編集するのに応じて `flycheck`, `auto completion`, `company`, `ifendif` に反映されます。
makefile に書かれたインクルードパスやマクロを修正した場合に限ってはこれは反映されないため、`M-x compile` を使って `cmdrec -- make` を起動し直してください。

