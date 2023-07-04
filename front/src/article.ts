export type RawArticle = {
  path: string;
  title: string;
  update: string;
  description: string;
  tags: string[];
  body: string;
}

export type Article = {
  path: string;
  title: string;
  update: Date;
  description: string;
  tags: string[];
  body: string;
}

export function toArticle(raw_article: RawArticle): Article {
  return {
    path: raw_article.path,
    title: raw_article.title,
    update: new Date(raw_article.update), // UnixTime to Date
    description: raw_article.description,
    tags: raw_article.tags,
    body: raw_article.body
  }
}

export function toRawArticle(article: Article): RawArticle {
  return {
    path: article.path,
    title: article.title,
    update: article.update.toISOString(),
    description: article.description,
    tags: article.tags,
    body: article.body
  }
}