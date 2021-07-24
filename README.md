# dotfiles

## Installation

```console
$ curl -L https://raw.githubusercontent.com/krororo/dotfiles/master/install.sh | bash
```

## git のユーザ名、メールアドレスを設定

```console
$ git config -f ~/.config/git/config.local user.name <name>
$ git config -f ~/.config/git/config.local user.email <email>
```

## emacs で xim を無効化

ランチャーの設定で `env XMODIFIERS @im=none` を追加する。

## 背面ウィンドウのスクロールをできるようにする

[ウィンドウマネージャー（詳細）] -> [アクセシビリティ] の
[いずれかのボタンが押されている時にウィンドウを前面に出す]
からチェックを外す。

## xkeysnail を systemd で動かす

xkeysnail は pip3 を使ってインストールする。

uinput グループを作成

```console
$ sudo groupadd uinput
```

ユーザーを input, uinput グループに追加

```console
$ sudo gpasswd -a $(whoami) input
$ sudo gpasswd -a $(whoami) uinput
```

udev の設定

```console
$ cat /etc/udev/rules.d/40-udev-xkeysnail.rules
KERNEL=="uinput", GROUP="uinput"
```

systemd の設定

```console
$ sed -e "s;%username%;$(whoami);" ~/dotfiles/templates/xkeysnail.service > ~/.config/systemd/user/xkeysnail.service
$ systemctl --user daemon-reload
$ systemctl --user enable xkeysnail.service
$ systemctl --user start xkeysnail.service
```
