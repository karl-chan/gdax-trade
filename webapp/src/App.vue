<template lang="pug">
  v-app
    v-navigation-drawer(app fixed clipped v-model="drawer")
      v-list(dense)
        template(v-for="item in items")
          v-list-tile(@click='' :key='item.text')
            v-list-tile-action
              v-icon {{ item.icon }}
            v-list-tile-content
              v-list-tile-title {{ item.text }}
    v-toolbar(app absolute clipped-left color="primary" dark)
      v-toolbar-side-icon(@click.native="drawer = !drawer")
      v-toolbar-title Gdax Trade Webapp
    v-content
      v-container(fluid)
        router-view
    v-footer(app)
</template>

<script>
export default {
  name: 'App',
  data: () => ({
    drawer: null,
    items: [{ icon: 'lightbulb_outline', text: 'Notes' }]
  })
}
</script>

<style lang="stylus">
@import '~vuetify/src/stylus/main';
</style>
