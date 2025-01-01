export interface ArticleInfo {
    path: string;
    title: string;
    update: string;
    description: string;
    tags: string[];
}

export interface Article extends ArticleInfo {
    body: string;
}

export const def="def"
