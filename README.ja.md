# typed-admin

注意: このフレームワークは現在アルファ版です、下位互換を壊す変更を繰り返します。

型から管理画面のインターフェースを殆ど自動的に実装できる（ようになる予定の）フレームワークです。

開発者は、実際にデータを扱う箇所のみを用意するだけで（例えばsqlを書く）、管理画面が完成します。


## 主なクラス

ざっくりと概念を解説します。詳細はhaddock。

### ToDetailField a
HTMLとして表示可能なa
- 基本的な型に対してtyped-adminで実装

### ToDetailField m a
HTML Formとして表示可能なa
- 基本的な型に対してtyped-adminで実装

### ToDetail m a
詳細表示可能なa(見出し（ラベル）も含めたhtmlとして表示可能)
- ToDetailField a及びその直積に対して導出可能

### ToForm m a
- HTML Form化可能なa
- ToDetailField a及びその直積に対して導出可能

### class (HasHeader a, ToDetail m a, ToForm m b) => ListConsole m a b

bから検索できて、一覧表示可能なa。

### class (ToDetail m a) => DetailConsole m a

詳細個別表示可能なa。

### class (ToDetail m a, ToForm m b, Subtype b a) => CreateConsole m a b

フォームの初期値、表示のみに使う値としてaを使い、bを作成可能。

### class (ToDetail m a, ToForm m b, Subtype b a) => EditConsole m a b

フォームの初期値、表示のみに使う値としてaを使い、bを更新可能。

## Router

*Consoleのインスタンスにした型を組み合わせてRouteを定義

```
route :: [Route IO]
route = 
  [ ListR "beers" (Proxy :: Proxy BeerSummary) (Proxy :: Proxy SearchBeerParam)
  , DetailR ("beers" </> var @Int64) (Proxy :: Proxy Beer.Beer)
  , CreateR "beers" (Proxy :: Proxy Beer.Beer) (Proxy :: Proxy Beer.Beer)
  , EditR ("beers" </> var @Int64) (Proxy :: Proxy Beer.Beer) (Proxy :: Proxy UpdateBeerParam)
  , ListR "stores" (Proxy :: Proxy Store.Store) (Proxy :: Proxy ())
  ]
```
*Identを持つ*Consoleは、Path中の変数とIdentが対応することを保証する
　(各Routeの第２引数は`Path c`型で`PathParam (Path c) (Ident a)`を制約する)。

## i18n
階層化できる辞書ファイルに対応

## 実行

adminにRouteを渡すと、Wai Middlewareができる。

```
adminMiddleware = admin route Prelude.id Nothing dic
```

## 今後の計画
- デザインの実装
  - Sematic UIかLightningを検討中
- 具体的にRDBMSをターゲットとし、各Consoleの実装を自動化する別ライブラリを用意
  - HRRに依存し、ListConsole.listで使うqueryなどを(ToForm a)から生成する
