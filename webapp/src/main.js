// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import Vuetify from 'vuetify'
import App from '@/App'
import components from '@/components'
import router from '@/router'
import store from '@/store'
import colors from 'vuetify/es5/util/colors'

Vue.config.productionTip = false
Vue.use(Vuetify, {
  theme: {
    primary: colors.orange.base,
    secondary: colors.pink.lighten1,
    accent: colors.green.base
  }
})

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  components: { App, ...components },
  template: '<App/>'
})
