import axios from 'axios'
import { api_article_root, api_plain_article_root, api_root } from './config'
import { Article, toArticle, toRawArticle } from './article'

const default_user = 'user'

export function createArticle(article: Article, pass: string): Promise<any> {
  const url = api_article_root + article.path
  const raw_article = toRawArticle(article)
  return axios.post(url, raw_article, { auth: { username: default_user, password: pass } })
}

export async function getArticleInfo(): Promise<Article[]> {
  let resp = await axios.get(api_article_root);
  return resp.data
}

export async function getArticle(path: string): Promise<Article> {
  const resp = await axios.get(api_article_root + path);
  const raw = resp.data
  return toArticle(raw)
}

export async function getPlainArticle(path: string): Promise<Article> {
  const resp = await axios.get(api_plain_article_root + path);
  const raw = resp.data
  return toArticle(raw)
}

export async function postImage(category: string, image: File, fallback_name: string, pass: string) {
  const image_name = fallback_name.length > 0 ? fallback_name : image.name
  const i = await image.arrayBuffer()
  return axios.post(
    api_root + "image/" + category + "/" + image_name, i, 
    { headers: { "Content-Type": 'application/octet-stream' }, auth: { username: default_user, password: pass } })
}

export async function getImages(): Promise<string[]> {
  const ret = await axios.get(api_root + "image/")
  return ret.data;
}

export async function getAuthTest(pass: string) {
  const resp = await axios.get(api_root + 'auth_test/', { auth: { username: default_user, password: pass } }).catch((err => {
    console.log('err:', err)
  }))
  return resp
}