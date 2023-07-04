<template>
  <main>
    <PostList :article_list="article_list" />
    <a class="readmore" href="/posts">もっと読む</a>
    <a class="newpost" href="/posts/new">あたらしく書く</a>
  </main>
</template>
<script setup lang="ts">
import { Ref, onMounted, ref } from 'vue';
import { useRoute, useRouter } from 'vue-router';
import PostList from '../components/PostList.vue'
import { getArticleInfo } from '../api.js';
import { Article } from '../article';

const router = useRouter()

const article_list: Ref<Article[]> = ref([])

onMounted(() => {
  {
    // Vue2でブログを作った時にhash routingをしていたがやめたので、その時代のURLのためのフォールバック
    const hash = useRoute().hash
    const hash_posts_path = "#/posts/"
    // const hash_slides_path = "#/slides/"
    const need_redirect = hash.slice(0, hash_posts_path.length) === hash_posts_path
    if (need_redirect) {
      const redirect_path = hash.slice(hash_posts_path.length, hash.length)
      router.push({ name: 'Post', params: { id: redirect_path } })
    }
  }
  getArticleInfo().then((ret) => {
    article_list.value = ret.slice(0, 7);
  })
})
</script>
<style scoped>
.readmore {
  display: block;
  margin-top: 2rem;
}
.newpost {
  display: block;
  margin-top: 2rem;
}
</style>

