import apiService from '@/services/api-service'
import { INITIALISE_STATIC_DATA, INITAILISE_REST_METHODS, INITIALISE_CHANNELS, INITIALISE_PRODUCTS } from '@/store/actions'
import { SET_CHANNELS, SET_PRODUCTS, SET_REST_METHODS } from '@/store/mutations'

// initial state
const state = {
  channels: [],
  products: [],
  restMethods: []
}

// getters
const getters = {
}

// actions
const actions = {
  [INITIALISE_STATIC_DATA] ({ dispatch }) {
    dispatch(INITIALISE_CHANNELS)
    dispatch(INITIALISE_PRODUCTS)
    dispatch(INITAILISE_REST_METHODS)
  },

  [INITIALISE_CHANNELS] ({ commit }) {
    apiService.getChannels()
      .then(channels => commit(SET_CHANNELS, { channels }))
      .catch(console.error)
  },

  [INITIALISE_PRODUCTS] ({ commit }) {
    apiService.getProducts()
      .then(products => commit(SET_PRODUCTS, { products }))
      .catch(console.error)
  },

  [INITAILISE_REST_METHODS] ({ commit }) {
    apiService.getRestMethods()
      .then(restMethods => commit(SET_REST_METHODS, { restMethods }))
      .catch(console.error)
  }
}

// mutations
const mutations = {
  [SET_CHANNELS] (state, { channels }) {
    state.channels = channels
  },
  [SET_PRODUCTS] (state, { products }) {
    state.products = products
  },
  [SET_REST_METHODS] (state, { methods }) {
    state.restMethods = methods
  }
}

export default {
  state,
  getters,
  actions,
  mutations
}
