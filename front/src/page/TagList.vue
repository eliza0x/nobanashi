<template>
  <main>
    <h1>{{ tag }} のページ</h1>
    <PostList :article_list="article_list"/>
  </main>
</template>

<script setup lang="ts">
import { ref, onMounted, Ref } from 'vue'
import PostList from '../components/PostList.vue'
import { useRoute } from 'vue-router';
import { getArticleInfo } from '../api';
import { ArticleInfo } from '../article';
import { useLogger } from 'vue-logger-plugin';

const route = useRoute()
const log = useLogger()

const tag: Ref<string> = ref("")
const article_list: Ref<ArticleInfo[]> = ref([])

onMounted(() => {
  const t = route.params.id
  if (typeof t === 'string') {
    tag.value = t;
  } else {
    log.error('tag is not string')
  }
  getArticleInfo().then((ret) => {
    article_list.value = ret.filter((val, _i, _arr) => {
      return val.tags.some((val, _i, _arr) => val == tag.value);
    });
  })
})
</script>

