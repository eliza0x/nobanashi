import { integer, sqliteTable, text } from "drizzle-orm/sqlite-core";

export const articles = sqliteTable(
	"articles",
	{
		id: integer("id", { mode: "number" }).primaryKey({ autoIncrement: true }),
		path: text("path").notNull().unique(),
		title: text("title").notNull(),
		update: text("update").notNull(),
		description: text("description").notNull(),
		body: text("body").notNull(),
	},
	() => [],
);

export const article2tags = sqliteTable(
	"article2tags",
	{
		id: integer("id", { mode: "number" }).primaryKey({ autoIncrement: true }),
		articleId: integer("articles_id").notNull(),
		tagId: integer("tags_id").notNull(),
	},
	() => [],
);

export const tags = sqliteTable(
	"tags",
	{
		id: integer("id", { mode: "number" }).primaryKey({ autoIncrement: true }),
		tag: text("tag").notNull().unique(),
	},
	() => [],
);
