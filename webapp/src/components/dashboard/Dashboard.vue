<template lang="pug">
  v-container(grid-list-md)
    v-layout(row wrap)
      v-flex(xs6)
        .display-2.secondary--text Try it out!
        v-tabs(v-model="active" dark grow slider-color="accent" color="cyan")
          v-tab(:key="'rest'" ripple) Rest
          v-tab-item(:key="'rest'")
            dashboard-rest-form
          v-tab(:key="'stream'" ripple) Stream
          v-tab-item(:key="'stream'")
            dashboard-stream-form

      v-flex(xs6)

</template>

<script>
export default {
  name: 'Dashboard',
  data: () => ({
    active: 'rest'
  })
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style lang="stylus" scoped>
h1, h2 {
  font-weight: normal;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
