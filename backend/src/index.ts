import { eq, and, inArray } from "drizzle-orm";
import { drizzle, DrizzleD1Database } from "drizzle-orm/d1";
import { Hono, Context } from "hono";
import { articles, article2tags, tags } from "../schema";
import { Article, ArticleInfo } from "../src/def";

type Bindings = {
  DB: D1Database;
};

const app = new Hono<{ Bindings: Bindings }>();

app.get("/", (c) => c.text("buenos dias."));
app.get("/article", get_articles);
app.get("/article/:path", get_article);
app.get("/tags", get_tags);
app.post("/admin/article", post_article);

function select_articles(db: DrizzleD1Database) {
  return db.select().from(articles).leftJoin(article2tags, eq(articles.id, article2tags.articleId)).leftJoin(tags, eq(article2tags.tagId, tags.id));
} 

async function upsert_article(db: DrizzleD1Database, article: Article) {
  const a = {
    path: article.path,
    title: article.title,
    update: article.update,
    description: article.description,
    body: article.body
  };
  const [article_id] = await db.insert(articles).values(a).onConflictDoUpdate({target: articles.path, set: a}).returning({id: articles.id});
  {
    const ts = article.tags.map(x => { return {tag: x} });
    await db.insert(tags).values(ts).onConflictDoNothing();
  }
  {
    const tag_ids = await db.select().from(tags).where(inArray(tags.tag, article.tags));
    const a2ts = tag_ids.map(x => ({articleId: article_id.id, tagId: x.id}));
    await db.delete(article2tags).where(eq(article2tags.articleId, article_id.id));
    await db.insert(article2tags).values(a2ts);
  }
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
  const params = await c.req.json<Article>();
  const db = drizzle(c.env.DB);
  upsert_article(db, params)
  return c.json({})
}

async function get_tags(c: Context) {
  const path = c.req.param('path')
  const db = drizzle(c.env.DB);
  const result = await db.select().from(tags).all();
  return c.json(result);
}

export default app;