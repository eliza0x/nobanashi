import { eq } from "drizzle-orm";
import { drizzle, DrizzleD1Database } from "drizzle-orm/d1";
import { Hono, Context } from "hono";
import { articles, article2tags, tags } from "../schema";

type Bindings = {
  DB: D1Database;
};

const app = new Hono<{ Bindings: Bindings }>();

app.get("/", (c) => c.text("buenos dias."));
app.get("/article", get_articles);
app.get("/article/:path", get_article);
app.post("/article", post_article);
// app.get("/plain_article/:path", get_plain_article); // このapiが必要なくなるようにしたい
app.get("/tags", get_tags);

function select_articles(db: DrizzleD1Database) {
  return db.select().from(articles).leftJoin(article2tags, eq(articles.id, article2tags.articleId)).leftJoin(tags, eq(article2tags.tagId, tags.id));
} 

async function get_articles(c: Context) {
  const db = drizzle(c.env.DB);
  const result = await select_articles(db).all();
  return c.json(result);
}

async function get_article(c: Context) {
  const path = c.req.param('path')
  const db = drizzle(c.env.DB);
  const result = await select_articles(db).where(eq(articles.path, path)).get();
  return c.json(result);
}

async function post_article(c: Context) {
  const params = await c.req.json<typeof articles.$inferSelect>();
  const db = drizzle(c.env.DB);
  const result = await db.insert(articles).values({
    path: params.path,
    title: params.title,
    update: params.update,
    description: params.description,
    body: params.body
  }).run();
  return c.json(result);
}

async function get_tags(c: Context) {
  const path = c.req.param('path')
  const db = drizzle(c.env.DB);
  const result = await db.select().from(tags).all();
  return c.json(result);
}

export default app;