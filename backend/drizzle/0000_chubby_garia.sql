CREATE TABLE `article2tags` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`articles_id` integer NOT NULL,
	`tags_id` integer NOT NULL
);
--> statement-breakpoint
CREATE TABLE `articles` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`path` text NOT NULL,
	`title` text NOT NULL,
	`update` text NOT NULL,
	`description` text NOT NULL,
	`body` text NOT NULL
);
--> statement-breakpoint
CREATE TABLE `tags` (
	`id` integer PRIMARY KEY AUTOINCREMENT NOT NULL,
	`tag` text NOT NULL
);
