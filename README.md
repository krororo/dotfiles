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

ランチャーの設定で `env XMODIFIERS @im=none` を追加する。

### 背面ウィンドウのスクロールをできるようにする

[ウィンドウマネージャー（詳細）] -> [アクセシビリティ] の
[いずれかのボタンが押されている時にウィンドウを前面に出す]
からチェックを外す。

### Alt+Space を Emacs で使えるようにする

[ウィンドウマネージャー] -> [キーボード] で [ウィンドウ操作メニュー] の
ショートカットキーをクリアする。

## Mac

### `¥` を `\` に変更する
システム環境設定 → キーボード → 入力ソース

- "¥"キーで入力する文字を `\` に変更
- 入力モードの「英字」をチェック
- ABC キーボードを削除
