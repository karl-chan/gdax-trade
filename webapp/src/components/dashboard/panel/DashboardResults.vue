<template lang="pug">
  v-container(grid-list-xl)
    v-layout(column)
      v-flex.display-2.secondary--text Results!
      v-flex.elevation-2.white
        JsonViewer(:description="description" :error="error" :pagination="pagination" :value="json"
          @paginateBefore="paginateBefore" @paginateAfter="paginateAfter")

</template>

<script>
import { mapState } from 'vuex'
import JsonViewer from '@/components/json/JsonViewer'
import { SUBMIT_DASHBOARD_REST_FORM } from '@/store/actions'
export default {
  name: 'DashboardResults',
  components: {JsonViewer},
  computed: {
    ...mapState({
      description: state => state.dashboard.display.description,
      error: state => state.dashboard.display.error,
      pagination: state => state.dashboard.display.pagination,
      json: state => state.dashboard.display.json
    })
  },
  methods: {
    paginateBefore (before) {
      this.$store.dispatch(SUBMIT_DASHBOARD_REST_FORM, {before})
    },
    paginateAfter (after) {
      this.$store.dispatch(SUBMIT_DASHBOARD_REST_FORM, {after})
    }
  }
}
</script>
