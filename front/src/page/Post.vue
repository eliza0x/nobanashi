<template>
  <div>
    <header v-if='typeof article !== "undefined"'>
      <h1 class="title">{{ article.title }}</h1> 
      <nav>
        <a v-on:click="edit_path" class="edit">EDIT</a>
        <div class="tags">tags: <a class="tag" v-for="tag in article.tags" :key="tag" :href="'/tags/'+tag">#{{ tag }}</a></div>
        <time>date: {{ article.update.getFullYear() + "/" + article.update.getMonth() + "/" + article.update.getDate() }}</time>
      </nav>
    </header>
    <header v-else>
      <p>ﾖﾐｺﾐﾁｭｳ。。。</p>
    </header>
    <article id="html" v-html="html"></article>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, Ref } from 'vue';
import { useRoute, useRouter } from 'vue-router';
import { useLogger } from 'vue-logger-plugin'
import { Article } from '../article';
import { getArticle } from '../api.js'

const log = useLogger()
const router = useRouter()

const article: Ref<Article | undefined> = ref(undefined)
const html = ref("<p>loading...</p>")

function edit_path() {
  router.push({ name: 'Edit', params: { id: article.value?.path } })
}

onMounted(() => {
  const path = useRoute().params.id
  if (typeof path === 'string') {
    getArticle(path).then((ret) => {
      article.value = ret
      html.value = ret.body
      document.title = ret.title
    }).catch(() => {
      html.value = "<p>記事の取得に失敗しました</p>"
    })
  } else {
    log.error('path is not string')
  }
})
</script>

<style>
.tags {
margin-bottom: 0.3rem;
}
.tag {
  margin-right: 0.3rem;
}
.title {
  text-align: center;
}
.title::before {
  content: "";
}
.edit {
  display: flex;
  flex-direction: row-reverse;
}
img {
  width: 100%;
  height: auto;
}
time {
  display: block;
}

/* pandoc */
code {
  font-family: Menlo, Monaco, 'Lucida Console', Consolas, monospace;
  font-size: 85%;
  margin: 0;
}
pre {
  margin: 1em 0;
  overflow: auto;
}
pre code {
  padding: 0;
  overflow: visible;
  overflow-wrap: normal;
}
.sourceCode {
 background-color: transparent;
 overflow: visible;
}
/*
#TOC li {
  list-style: none;
}
#TOC ul {
  padding-left: 1.3em;
}
#TOC > ul {
  padding-left: 0;
}
#TOC a:not(:hover) {
  text-decoration: none;
}
*/
code{white-space: pre-wrap;}
span.smallcaps{font-variant: small-caps;}
/*
div.columns{display: flex; gap: min(4vw, 1.5em);}
div.column{flex: auto; overflow-x: auto;}
div.hanging-indent{margin-left: 1.5em; text-indent: -1.5em;}
ul.task-list{list-style: none;}
ul.task-list li input[type="checkbox"] {
  width: 0.8em;
  margin: 0 0.8em 0.2em -1.6em;
  vertical-align: middle;
}
*/
pre > code.sourceCode { white-space: pre; position: relative; }
pre > code.sourceCode > span { display: inline-block; line-height: 1.25; }
pre > code.sourceCode > span:empty { height: 1.2em; }
.sourceCode { overflow: visible; }
code.sourceCode > span { color: inherit; text-decoration: inherit; }
div.sourceCode { margin: 1em 0; }
pre.sourceCode { margin: 0; }
@media screen {
div.sourceCode { overflow: auto; }
}
@media print {
pre > code.sourceCode { white-space: pre-wrap; }
pre > code.sourceCode > span { text-indent: -5em; padding-left: 5em; }
}
pre.numberSource code
  { counter-reset: source-line 0; }
pre.numberSource code > span
  { position: relative; left: -4em; counter-increment: source-line; }
pre.numberSource code > span > a:first-child::before
  { content: counter(source-line);
    position: relative; left: -1em; text-align: right; vertical-align: baseline;
    border: none; display: inline-block;
    -webkit-touch-callout: none; -webkit-user-select: none;
    -khtml-user-select: none; -moz-user-select: none;
    -ms-user-select: none; user-select: none;
    padding: 0 4px; width: 4em;
    color: #aaaaaa;
  }
pre.numberSource { margin-left: 3em; border-left: 1px solid #aaaaaa;  padding-left: 4px; }
div.sourceCode
  {   }
@media screen {
pre > code.sourceCode > span > a:first-child::before { text-decoration: underline; }
}
code span.al { color: #ff0000; } /* Alert */
code span.an { color: #008000; } /* Annotation */
code span.at { } /* Attribute */
code span.bu { } /* BuiltIn */
code span.cf { color: #0000ff; } /* ControlFlow */
code span.ch { color: #008080; } /* Char */
code span.cn { } /* Constant */
code span.co { color: #008000; } /* Comment */
code span.cv { color: #008000; } /* CommentVar */
code span.do { color: #008000; } /* Documentation */
code span.er { color: #ff0000; font-weight: bold; } /* Error */
code span.ex { } /* Extension */
code span.im { } /* Import */
code span.in { color: #008000; } /* Information */
code span.kw { color: #0000ff; } /* Keyword */
code span.op { } /* Operator */
code span.ot { color: #ff4000; } /* Other */
code span.pp { color: #ff4000; } /* Preprocessor */
code span.sc { color: #008080; } /* SpecialChar */
code span.ss { color: #008080; } /* SpecialString */
code span.st { color: #008080; } /* String */
code span.va { } /* Variable */
code span.vs { color: #008080; } /* VerbatimString */
code span.wa { color: #008000; font-weight: bold; } /* Warning */
.display.math{display: block; text-align: center; margin: 0.5rem auto;}
</style>
