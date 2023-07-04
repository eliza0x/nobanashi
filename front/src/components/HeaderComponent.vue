<template>
    <header>
        <div class="logo">
          <a href="/">のばなしうさぎ</a>
        </div>
        <nav>
          <a href="/">Home</a>
          <a href="/posts">Posts</a>
          <a href="/contact">Contact</a>
        </nav>
    </header>
</template>
<script setup lang="ts">
</script>

<style scoped>
header {
  display: flex;
  justify-content: space-between;
  font-family: sans-serif;
  margin-bottom: 3rem;
}
.nav {
  text-align: right;
}
.logo {
  text-align: left;
}
nav a {
  margin-right: 5vw;
}
</style>
