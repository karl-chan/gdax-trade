import Vue from 'vue'
import Vuex from 'vuex'
import config from '@/store/modules/config'
import dashboard from '@/store/modules/dashboard'

Vue.use(Vuex)

const debug = process.env.NODE_ENV !== 'production'

const store = new Vuex.Store({
  modules: {
    config,
    dashboard
  },
  strict: debug
})
store.dispatch('initialiseStaticData')

export default store
