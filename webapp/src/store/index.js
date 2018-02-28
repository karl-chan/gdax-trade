import Vue from 'vue'
import Vuex from 'vuex'
import dashboard from '@/store/modules/dashboard'
import staticData from '@/store/modules/staticData'

Vue.use(Vuex)

const debug = process.env.NODE_ENV !== 'production'

const store = new Vuex.Store({
  modules: {
    dashboard,
    staticData
  },
  strict: debug
})
store.dispatch('initialiseStaticData')

export default store
