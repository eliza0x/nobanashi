import { createApp } from 'vue'
import { createRouter, createWebHistory } from 'vue-router'
import { createLogger } from 'vue-logger-plugin'

import App from './App.vue'
import Root from './page/Root.vue'
import Post from './page/Post.vue'
import PostList from './page/PostList.vue'
import Edit from './page/New.vue'
import New from './page/New.vue'
import NotFound from './page/404.vue'
import TagPageList from './page/TagList.vue'
import Upload from './page/Upload.vue'
import Contact from './page/Contact.vue'

// router
const routes = [
  { path: '/', name: 'Root', component: Root },
  { path: '/posts', name: 'PostList', component: PostList },
  { path: '/posts/new', name: 'New', component: New },
  { path: '/posts/:id/edit', name: 'Edit', component: Edit },
  { path: '/posts/:id', name: 'Post', component: Post },
  { path: '/tags/:id', name: 'TagPageList', component: TagPageList },
  { path: '/images', name: 'Upload', component: Upload },
  { path: '/contact', name: 'Contact', component: Contact },
  { path: '/:catchAll(.*)', name: '404', component: NotFound },
]
const router = createRouter({
  history: createWebHistory(),
  routes,
})

// logger
const logger = createLogger({
  enabled: true,
  level: 'debug'
})

const app = createApp(App)
app.use(router)
app.use(logger)
app.mount('#app');