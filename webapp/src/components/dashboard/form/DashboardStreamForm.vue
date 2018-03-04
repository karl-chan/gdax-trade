<template lang="pug">
  v-container(grid-list-md)
   v-form(v-model="valid" ref="form")
    v-select(v-model="selectedProducts" label="Enter a product" :items="availableProducts" :rules="[rules.products]" multiple chips deletable-chips)
    .body-2 Select channels
    v-layout(row wrap)
      v-flex(xs12 md4 v-for="channel in availableChannels" :key="channel")
        v-checkbox(v-model="selectedChannels" :label="channel" :value="channel" hide-details)
    v-flex
      v-btn(:disabled="!valid" @click="submit" color="success") Submit
        v-icon(right) send

</template>

<script>
import { mapState } from 'vuex'
import { SUBMIT_DASHBOARD_STREAM_FORM } from '@/store/actions'
import { SET_DASHBOARD_STREAM_PRODUCTS, SET_DASHBOARD_STREAM_CHANNELS } from '@/store/mutations'
export default {
  name: 'DashboardStreamForm',
  data: function () {
    return {
      valid: false,
      rules: {
        products: function (value) {
          return !!value.length || 'At least one product must be selected'
        }
      }
    }
  },
  computed: {
    ...mapState({
      availableProducts: state => state.config.products,
      availableChannels: state => state.config.channels
    }),
    selectedProducts: {
      get () {
        return this.$store.state.dashboard.forms.stream.products
      },
      set (products) {
        this.$store.commit(SET_DASHBOARD_STREAM_PRODUCTS, products)
      }
    },
    selectedChannels: {
      get () {
        return this.$store.state.dashboard.forms.stream.channels
      },
      set (channels) {
        this.$store.commit(SET_DASHBOARD_STREAM_CHANNELS, channels)
      }
    }
  },
  methods: {
    submit () {
      if (this.$refs.form.validate()) {
        this.$store.dispatch(SUBMIT_DASHBOARD_STREAM_FORM)
      }
    }
  }
}
</script>
