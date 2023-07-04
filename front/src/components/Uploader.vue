<template>
  <div>
    <img v-for="image in uploaded_images" v-bind:src="api_image_root + image" />
  </div>
  <form>
    <p><input accept="image/*" multiple type="file" v-on:change="onChange" /></p>
  </form>
  <ul class="uploadees">
    <li v-for="file in files" class="uploadee">
        <img :src="file.blob_path">
        <input v-model="file.name"/>
    </li>
  </ul>
  <p>category: <input v-model="category"/> email: <input type="email"/> password: <input v-model="pass" type="password"/><button @click="submitImages" type="button">upload</button></p>
</template>

<script setup lang="ts">
import { Ref, onMounted, ref } from 'vue'
import { postImage, getImages } from '../api'
import { api_image_root } from '../config'

const uploaded_images: Ref<string[]> = ref([])
const files: Ref<{file: File, name: string, blob_path: string}[]> = ref([])
const pass: Ref<string> = ref("")
const category: Ref<string> = ref("")

function onChange(event: any) {
  let fs: {file: File, name: string, blob_path: string}[] = []
  for (const f of event.target.files) {
    const blob_path = window.URL.createObjectURL(f)
    fs.push({file: f, name: f.name, blob_path: blob_path})
  }
  files.value = fs
}
function syncUploadedImages() {
  getImages().then((is) => {
    if (category.value.length > 0) {
        uploaded_images.value = is 
    } else {
        uploaded_images.value = is.slice(0, 20)
    }
  })
}
async function submitImages() {
  for (const i of files.value) {
    postImage(category.value, i.file, i.name, pass.value).then((_resp) => {
      // console.log('resp: ', resp)
      syncUploadedImages()
    }).catch((err) => {
      console.log('err: ', err)
    })
  }
}
onMounted(() => {
    syncUploadedImages()
})
</script>
 
<style scoped>
img {
    height: 150px;
    width: auto;
    margin-right: 10px;
}
button {
  margin-left: 10px;
}
.uploadees {
    display: flex;
    flex-direction: row;
}
.uploadee {
    list-style: none;
    display: flex;
    flex-direction: column;
    margin-right: 10px;
    align-items: baseline;
}
</style>
