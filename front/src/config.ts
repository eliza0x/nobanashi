// export const api_root: string = 'http://localhost:8080/'
export const api_root: string = import.meta.env.VITE_BACKEND_URL
export const api_article_root: string = api_root + 'article/'
export const api_plain_article_root: string = api_root + 'plain_article/'
export const api_static_root: string = api_root + 'static/'
export const api_image_root: string = api_static_root + 'images/'