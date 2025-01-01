```
npm run deploy
```

```
wrangler d1 execute blog-d1 --file=./schema.sql --local
wrangler d1 execute blog-d1 --local --command="SELECT * FROM articles"
```

## how to run

```
$ bun install
$ bunx wrangler login
$ bunx run dev
```