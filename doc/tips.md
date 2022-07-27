# Tips

## PostgreSQL の backup & restore

### backup

```
pg_dumpall -c | gzip -c > ~/all.dump.gz

dc exec -T postgres pg_dumpall -c -U postgres -h 127.0.0.1 | gzip > /path/to/pg_dumpall-$(date +'%Y%m%d').sql.gz
```

### restore

```
zcat all.dump.gz | psql -U postgres
```

## mysqldump & restore

```
mysqldump -u username -ppassword dbname | gzip > dumpfilename.sql.gz

zcat dumpfile.sql.gz | MYSQL_PWD=password mysql -u username database_name
```

mysqldump には PROCESS 権限が必要なので root ユーザーで実行、または --no-tablespaces(-y) オプションを付ける。
https://qiita.com/katzueno/items/29d812eb3aafdb23b0bb

こんな感じで mysql データベースの内容は消せそう。

```sh
sed -i.bak -e '/^-- Current Database: `mysql`/,/^USE `DB名`/d' mysqldump-xxxx.sql
```

以下は抜き出し。

```sh
sed -n '/Current Database: `DB名`/,/Current Database:/p' dumpdata.txt
```

GLOBAL INNNODB〜 を消さないと、 root アカウント以外でリストアできない

## Rails

### テストでも詳しいクエリログを出したい時

config/environments/test.rb に以下の設定を追加。

```ruby
  config.verbose_query_logs = true
```

## redis

### 接続確認

```
$ curl telnet://redis-server:6379
ping
+PONG
```

### キャッシュクリア

```
$ redis-cli -u redis://redis-server
redis-server:6379> FLUSHDB
redis-server:6379> (Ctrl+D)
```

### コマンド

- keys **pattern** : key 一覧取得
- type **key** : key のタイプを取得
- get **key** : string タイプの値を取得

## elisp で base64 decode

```lisp
(decode-coding-string
 (base64-decode-string "<string>")
 'utf-8)
```
