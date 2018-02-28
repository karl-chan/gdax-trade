<template lang="pug">
  v-container(grid-list-md)
    v-layout(row wrap)
      v-flex(xs6)
        .display-2.secondary--text Try it out!
        .v-tabs(v-model="active" color="cyan" dark slider-color="accent")
          .v-tab(ripple) Rest
          .v-tab(ripple) Stream
      v-flex(xs6)

</template>

<script>
export default {
  name: 'DashboardRestForm',
  data: () => ({

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
