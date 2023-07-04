<template>
  <div class="centering">
    <Header id="head" />
    <RouterView id="main" />
    <Footer id="footer" />
  </div>
</template>

<script setup lang="ts">
import Header from './components/HeaderComponent.vue'
import Footer from './components/FooterComponent.vue'
import { RouterView } from 'vue-router';

// const state = inject(state_controller_key)
</script>

<style>
.centering {
  width: 95vw;
  margin-left: auto;
  margin-right: auto;
}
@media (min-width: 1020px) {
  .centering {
    width: 65vw;
    margin-left: auto;
    margin-right: auto;
  }
}
body a {
  color: #4a4a4a;
  text-decoration: underline;
} 
body a:hover {
  color: #DF3A01;
  text-decoration: none;
  border-bottom: none;
}
#main {
  font-family: serif;
}
#main h1, #main h2, #main h3, #main h4, #main h5 {
  font-family: sans-serif
}
h1::before, h2::before, h3::before, h4::before, h5::before {
  content: "# ";
}
</style>
