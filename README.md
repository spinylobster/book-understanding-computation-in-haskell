[「アンダースタンディング コンピュテーション」を Haskell でやってみた](https://qiita.com/spinylobster/items/6323f2ad37cdd96da7be)

## テストの実行について

普通に `stack test` を走らせると、 Chapter6 の FizzBuzz のコードでコンパイルが通りません。
テストの実行には [ghcid](https://github.com/ndmitchell/ghcid) を使ってください。

```bash
stack exec -- ghcid
```
