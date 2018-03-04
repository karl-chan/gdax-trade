import axios from 'axios'
import { stringify } from 'qs'

// static data
const GET_CHANNELS_ENDPOINT = '/static-data/channels'
const GET_PRODUCTS_ENDPOINT = '/static-data/products'
const GET_REST_METHODS_ENDPOINT = '/static-data/rest-methods'

// dashboard
const POST_REST_FORM_ENDPOINT = '/rest'
const POST_STREAM_FORM_ENDPOINT = '/stream'

export default {
  getChannels: () => apiGet(GET_CHANNELS_ENDPOINT),
  getProducts: () => apiGet(GET_PRODUCTS_ENDPOINT),
  getRestMethods: () => apiGet(GET_REST_METHODS_ENDPOINT),
  postDashboardRestForm: (method, endpoint, payload) => apiPost(POST_REST_FORM_ENDPOINT, { method, endpoint, payload }),
  postDashboardStreamForm: () => apiPost(POST_STREAM_FORM_ENDPOINT)
}

const apiGet = (url) => {
  return axios
    .get(url, { baseURL: '/api' })
    .then(res => res.data)
}

const apiPost = (url, data) => {
  return axios
    .post(url, stringify(data), { baseURL: '/api' })
    .then(res => res.data)
}
