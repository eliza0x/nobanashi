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
import { ArticleInfo } from '../article';

const router = useRouter()

const article_list: Ref<ArticleInfo[]> = ref([])

function redirectWhenHashUsing(hash: string) {
    // Vue2でブログを作った時にhash routingをしていたがやめたので、その時代のURLのためのフォールバック
    const hash_posts_path = "#/posts/"
    // const hash_slides_path = "#/slides/" // slideのｈリダイレクトは将来で
    const need_redirect = hash.slice(0, hash_posts_path.length) === hash_posts_path
    if (need_redirect) {
      const redirect_path = hash.slice(hash_posts_path.length, hash.length)
      router.push({ name: 'Post', params: { id: redirect_path } })
    }
}

onMounted(() => {
  const route = useRoute()
  {
    const hash = route.hash
    redirectWhenHashUsing(hash)
  }
  {
    // XXXX.html のページは XXXX にリダイレクトする
    const path = route.fullPath
    console.log(path)
    console.log(path.slice(path.length-('.html'.length), path.length))
    const is_html = path.slice(path.length-('.html'.length), path.length) === '.html'
    if (is_html) {
      const redirect_path = path.slice(0, path.length-('.html'.length))
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

