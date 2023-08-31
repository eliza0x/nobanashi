<template>
  <ul id="article-list">
    <li v-for="article in props.article_list" :key="article.path" class="post">
      <a :href="'/posts/' + article.path" class="article">
        <div>
          <h2 class="post-title">
            <a :href="'/posts/' + article.path">{{ article.title }}</a>
          </h2>
        </div>
        <div>tags:
          <a v-for="tag in article.tags" :key="tag" :href="'/tags/'+tag" class="tag">{{ "#" + tag }}</a>
        </div>
        <p class="description">{{ article.description }}</p>
      </a>
    </li>
  </ul>
</template>

<script setup lang="ts">
import { ArticleInfo } from '../article';
const props = defineProps<{
  article_list: ArticleInfo[]
}>()
</script>

<style scoped>
.article {
  text-decoration: none;
}
.article:hover h2, .article:hover h2 a {
  color:#DF3A01;
}
.article:hover {
  color:#4a4a4a;
}
.post-title {
  margin-top: 15px;
  margin-bottom: 0px;
}
.post-title a {
  text-decoration: none;
}
.tag {
    display: inline;
    margin-left: 0.5rem;
    /* text-decoration: none; */
}
.description {
  margin-top: 0.5rem;
  margin-bottom: 0;
}
ul {
    margin: 0px;
    padding: 0px;
}
li {
    list-style-type: none;
}
li:not(:first-child) {
  margin-top: 3rem;
}
</style>
