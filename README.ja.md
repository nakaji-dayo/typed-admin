# typed-admin

注意: このフレームワークは現在アルファ版です、下位互換を壊す変更を繰り返します。

型から管理画面のインターフェースを殆ど自動的に実装できる（ようになる予定の）フレームワークです。

開発者は、実際にデータを扱う箇所のみを用意するだけで（例えばsqlを書く）、管理画面が完成します。


## 主なクラス

ざっくりと概念を解説します。詳細はhaddock。

### ToDetailField a
HTMLとして表示可能なa
- 基本的な方に対してはtyped-adminでサポートする

### ToDetail m a
詳細表示可能なa(見出し（ラベル）も含めたhtmlとして表示可能)
- 多くのケースでGenericsで導出可能

### ToForm m a
- HTML Form化可能なa
- 多くのケースでGenericsで導入可能

### class (HasHeader a, ToDetail m a, ToForm m b) => ListConsole m a b

bから検索できて、一覧表示可能なa。

#### Minimal complete definition

- list :: (Maybe b) -> Page -> m ([a])
  - 検索条件を受け取り、[a]を返す

### class (ToDetail m a) => DetailConsole m a

詳細個別表示可能なa。

#### Minimal complete definition

- detail :: Ident a -> m (Maybe a)

### class (ToDetail m a, ToForm m b, Subtype b a) => CreateConsole m a b

フォームの初期値、表示のみに使う値としてaを使い、bを作成可能。

### class (ToDetail m a, ToForm m b, Subtype b a) => EditConsole m a b

フォームの初期値、表示のみに使う値としてaを使い、bを更新可能。

## 使い方

*Consoleのインスタンスにした型を組み合わせて[Route]を定義

```
  [ ListR "beers" (Proxy :: Proxy BeerSummary) (Proxy :: Proxy SearchBeerParam)
  , DetailR ("beers" </> var @Int64) (Proxy :: Proxy Beer.Beer)
  , CreateR "beers" (Proxy :: Proxy Beer.Beer) (Proxy :: Proxy Beer.Beer)
  , EditR ("beers" </> var @Int64) (Proxy :: Proxy Beer.Beer) (Proxy :: Proxy UpdateBeerParam)
  , ListR "stores" (Proxy :: Proxy Store.Store) (Proxy :: Proxy ())
  ]
```
*Identを持つ*Consoleは、Path中の変数とIdentが対応することを保証する
　(各Routeの第２引数は`Path c`型で`PathParam (Path c) (Ident a)`を制約する)。

adminにRouteを渡すと、Wai Middlewareができる
```
run 3000 $ admin route Prelude.id Nothing (either (const Nothing) Just dic) application
```
