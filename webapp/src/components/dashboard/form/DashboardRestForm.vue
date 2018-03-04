<template lang="pug">
  v-form(v-model="valid" ref="form")
    v-radio-group(v-model="selectedMethod" row)
      v-radio(v-for="m in availableMethods" :key="m" :label="m" :value="m")
    v-select(v-model="selectedEndpoint" :items="recentEndpoints" label="Enter endpoint (e.g. /accounts)" :rules="[rules.endpoint]" autocomplete combobox)
    JsonEditor(v-if="selectedMethod === 'POST'" v-model="payload" label="(Optional) Payload")
    v-flex
      v-btn(:disabled="!valid" @click="submit" color="success") Submit
        v-icon(right) send
</template>

<script>
import { mapState } from 'vuex'
import JsonEditor from '@/components/json/JsonEditor'
import { SUBMIT_DASHBOARD_REST_FORM } from '@/store/actions'
import { SET_DASHBOARD_REST_METHOD, SET_DASHBOARD_REST_ENDPOINT, SET_DASHBOARD_REST_PAYLOAD } from '@/store/mutations'
export default {
  name: 'DashboardRestForm',
  components: {JsonEditor},
  data: function () {
    return {
      valid: false,
      rules: {
        endpoint: function (value) {
          return (value && value.startsWith('/')) || 'Endpoint should be absolute path (e.g. /accounts)'
        }
      }
    }
  },
  computed: {
    ...mapState({
      availableMethods: state => state.config.restMethods,
      recentEndpoints: state => state.dashboard.forms.rest.recentEndpoints
    }),
    selectedMethod: {
      get () {
        return this.$store.state.dashboard.forms.rest.method
      },
      set (method) {
        this.$store.commit(SET_DASHBOARD_REST_METHOD, method)
      }
    },
    selectedEndpoint: {
      get () {
        return this.$store.state.dashboard.forms.rest.endpoint
      },
      set (endpoint) {
        this.$store.commit(SET_DASHBOARD_REST_ENDPOINT, endpoint)
      }
    },
    payload: {
      get () {
        return this.$store.state.dashboard.forms.rest.payload
      },
      set (payload) {
        this.$store.commit(SET_DASHBOARD_REST_PAYLOAD, payload)
      }
    }
  },
  methods: {
    submit () {
      if (this.$refs.form.validate()) {
        this.$store.dispatch(SUBMIT_DASHBOARD_REST_FORM)
      }
    }
  }
}
</script>
