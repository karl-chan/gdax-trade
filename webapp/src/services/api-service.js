import axios from 'axios'

const GET_CHANNELS_ENDPOINT = '/static-data/channels'
const GET_PRODUCTS_ENDPOINT = '/static-data/products'
const GET_REST_METHODS_ENDPOINT = '/static-data/rest-methods'

export default {
  getChannels: () => apiGet(GET_CHANNELS_ENDPOINT),
  getProducts: () => apiGet(GET_PRODUCTS_ENDPOINT),
  getRestMethods: () => apiGet(GET_REST_METHODS_ENDPOINT)
}

const apiGet = (url) => {
  return axios
    .get(url, {baseURL: '/api'})
    .then(res => res.data)
}
