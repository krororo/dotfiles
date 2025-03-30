# dotfiles

## Installation

```console
$ git clone https://github.com/krororo/dotfiles.git ~/dotfiles
$ cd ~/dotfiles
$ ./install.sh
```

## git のユーザ名、メールアドレスを設定

```console
$ git config -f ~/.config/git/config.local user.name <name>
$ git config -f ~/.config/git/config.local user.email <email>
```

## Ubuntu

### emacs で xim を無効化

ランチャーの設定で `env XMODIFIERS=@im=none` を追加する。

### 背面ウィンドウのスクロールをできるようにする

[ウィンドウマネージャー（詳細）] -> [アクセシビリティ] の
[いずれかのボタンが押されている時にウィンドウを前面に出す]
からチェックを外す。

### Alt+Space を Emacs で使えるようにする

[ウィンドウマネージャー] -> [キーボード] で [ウィンドウ操作メニュー] の
ショートカットキーをクリアする。

### Nixインストール

```shell-session
nix profile install .#default
```

### ホームのディレクトリ名を英語化

以下のコマンドを実行。中身の移動やディレクトリの削除はされないので手動で対応する。

```shell-session
LANG=C xdg-user-dirs-update --force
```

## Mac

### `¥` を `\` に変更する
システム環境設定 → キーボード → 入力ソース

- "¥"キーで入力する文字を `\` に変更
- 入力モードの「英字」をチェック
- ABC キーボードを削除

### クォートが勝手に変換されないようにする
システム環境設定 → キーボード → ユーザ辞書

- 「スマート引用符とスマートダッシュを使用」のチェックを外す

### iTerm2 でメタキーを使いやすく
iTerm2 の環境設定からキーリマップの設定をする

* Profiles → Keys：Left option (⌥) key acts as で `Esc+` をチェック
* Profiles → Keys：Right option (⌥) key acts as で `Esc+` をチェック
* Keys → Remap Modifier Keys で Left ⌘ を Left option にリマップ
* Keys → Remap Modifier Keys で Left option(⌥) を Left ⌘ にリマップ
* Keys → Key Bindings で以下を "Do Not Remap Modifiers" に設定
  * Command-Tab : ウィンドウ切り替え
  * Command-Shift-Tab : ウィンドウ切り替え
  * Command-Space : Spotlight
