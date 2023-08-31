<template>
  <PostList :article_list="article_list" />
</template>

<script setup lang="ts">
import { ref, onMounted, Ref } from 'vue';
import PostList from '../components/PostList.vue'
import { getArticleInfo } from '../api';
import { ArticleInfo } from '../article';

const article_list: Ref<ArticleInfo[]> = ref([])
onMounted(() => {
  getArticleInfo().then((ret) => {
    article_list.value = ret;
  })
})
</script>

