import apiService from '@/services/api-service'
import { SET_DASHBOARD_REST_METHOD, SET_DASHBOARD_REST_ENDPOINT, SET_DASHBOARD_REST_PAYLOAD, SET_DASHBOARD_STREAM_PRODUCTS, SET_DASHBOARD_STREAM_CHANNELS, SET_DASHBOARD_DISPLAY_ERROR, SET_DASHBOARD_DISPLAY_JSON, CLEAR_DASHBOARD_DISPLAY, SET_DASHBOARD_ACTIVE_TAB, ADD_DASHBOARD_REST_RECENT_ENDPOINT } from '../mutations'
import { SUBMIT_DASHBOARD_REST_FORM, SUBMIT_DASHBOARD_STREAM_FORM } from '../actions'

// initial state
const state = {
  forms: {
    activeTabIndex: 0,
    rest: {
      method: 'GET',
      endpoint: undefined,
      payload: undefined,
      recentEndpoints: []
    },
    stream: {
      products: [],
      channels: []
    }
  },
  display: {
    description: undefined,
    error: undefined,
    pagination: undefined,
    json: undefined
  }
}

// getters
const getters = {

}

// actions
const actions = {
  [SUBMIT_DASHBOARD_REST_FORM] ({ commit }, pagination) {
    const method = state.forms.rest.method
    let endpoint = state.forms.rest.endpoint
    if (pagination) {
      endpoint += endpoint.includes('?') ? '&' : '?'
      if (pagination.before) {
        endpoint += 'before=' + pagination.before
      } else {
        endpoint += 'after=' + pagination.after
      }
    }
    const payload = state.forms.rest.payload
    const description = `<b>${method}</b> request to <b>${endpoint}</b>:`
    apiService.postDashboardRestForm(method, endpoint, payload)
      .then(({ before, after, body }) => {
        const pagination = before || after ? { before, after } : undefined
        commit(SET_DASHBOARD_DISPLAY_JSON, { description, pagination, json: body })
        commit(ADD_DASHBOARD_REST_RECENT_ENDPOINT, endpoint)
      })
      .catch(err => {
        commit(SET_DASHBOARD_DISPLAY_ERROR, { description, error: err.stack })
      })
  },
  [SUBMIT_DASHBOARD_STREAM_FORM] ({ commit }) {
    const products = state.forms.stream.products
    const channels = state.forms.stream.channels
    const description = `Streaming<br>products: <b>${products}</b><br>channels: <b>${channels}</b>`
    apiService.postDashboardStreamForm()
      .then(({ auth, endpoint }) => {
        const socket = new WebSocket(endpoint)
        socket.onopen = event => socket.send(JSON.stringify({
          type: 'subscribe',
          product_ids: products,
          channels: channels,
          signature: auth.signature,
          key: auth.key,
          passphrase: auth.passphrase,
          timestamp: auth.timestamp
        }))
      })
      .catch(err => {
        commit(SET_DASHBOARD_DISPLAY_ERROR, { description, error: err.stack })
      })
  }
}

// mutations
const mutations = {
  [SET_DASHBOARD_ACTIVE_TAB] (state, activeTabIndex) {
    state.forms.activeTabIndex = activeTabIndex
  },
  [SET_DASHBOARD_REST_METHOD] (state, restMethod) {
    state.forms.rest.method = restMethod
  },
  [SET_DASHBOARD_REST_ENDPOINT] (state, endpoint) {
    state.forms.rest.endpoint = endpoint
  },
  [SET_DASHBOARD_REST_PAYLOAD] (state, payload) {
    state.forms.rest.payload = payload
  },
  [ADD_DASHBOARD_REST_RECENT_ENDPOINT] (state, endpoint) {
    const index = state.forms.rest.recentEndpoints.indexOf(endpoint)
    if (index >= 0) {
      state.forms.rest.recentEndpoints.splice(index, 1)
    }
    state.forms.rest.recentEndpoints.unshift(endpoint)
  },
  [SET_DASHBOARD_STREAM_PRODUCTS] (state, products) {
    state.forms.stream.products = products
  },
  [SET_DASHBOARD_STREAM_CHANNELS] (state, channels) {
    state.forms.stream.channels = channels
  },
  [SET_DASHBOARD_DISPLAY_ERROR] (state, { description, error }) {
    state.display = { description, error, pagination: undefined, json: undefined }
  },
  [SET_DASHBOARD_DISPLAY_JSON] (state, { description, pagination, json }) {
    state.display = { description, error: undefined, pagination, json }
  },
  [CLEAR_DASHBOARD_DISPLAY] (state) {
    state.display = { description: undefined, error: undefined, pagination: undefined, json: undefined }
  }
}

export default {
  state,
  getters,
  actions,
  mutations
}
