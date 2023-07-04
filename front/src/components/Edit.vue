<template>
  <form>
    <p><input v-model="path" placeholder="path"/><input v-model="title" placeholder="title"/><input v-model="update" placeholder="update" type="date"/><input v-model="tags" placeholder="tags"/></p>
    <p><input v-model="description" placeholder="description"/></p>
    <p><input v-model="email" placeholder="email" type="email"><input v-model="password" placeholder="pass" type="password"/></p>
    <p><textarea cols="120" rows="15" v-model="body" /></p>
    <p><button @click="submitArticle" type="button">投稿</button></p>
  </form>
</template>

<script setup lang="ts">
import { Article } from '../article'
import { ref, onMounted, Ref } from 'vue';
import { createArticle, getPlainArticle } from '../api'
import { useRoute, useRouter } from 'vue-router';
import { useLogger } from 'vue-logger-plugin';

// todo: tag入力欄にドロップダウンメニューとか欲しいよね
// todo: 投稿完了後のtoastは必要

const log = useLogger()
const router = useRouter()

const today = new Date();
const path = ref("");
const title = ref("");
const update: Ref<Date> = ref(today);
const description = ref("");
const tags = ref("");
const body = ref("");
const email = ref("");
const password = ref("");

function submitArticle() {
  const ts: string[] = tags.value.split(',').map((x) => x.trim()).filter((val, _i, _arr) => val != "");
  const article: Article = { 
    path: path.value,
    title: title.value,
    update: new Date(update.value),
    description: description.value,
    tags: ts,
    body: body.value 
  };
  log.info('article:', article);
  createArticle(article, password.value).then(_ => {
    router.push('/posts/' + path.value)
  }).catch(_ => {
    log.warn("failed to post article")
  })
}
onMounted(() => {
  console.log('new mounted')

  const id = useRoute().params.id;
  if (typeof id === 'string') {
    path.value = id
    getPlainArticle(id).then(article => {
      path.value = article.path
      title.value = article.title
      update.value = new Date(article.update)
      description.value = article.description
      tags.value = article.tags.join(" ")
      body.value = article.body
    })
  }
})
</script>
 
<style scoped>
input, textarea {
  margin-left: 10px;
}
</style>
