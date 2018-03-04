<template lang="pug">
  v-container(grid-list-xl)
    v-layout(column)
      v-flex.display-2.secondary--text Try it out!
      v-flex.elevation-2
        v-tabs(v-model="activeTabIndex" grow fixed-tabs)
          v-tab(key="0" ripple) Rest
          v-tab-item(:key="0")
            DashboardRestForm
          v-tab(key="1" ripple) Stream
          v-tab-item(:key="1")
            DashboardStreamForm
</template>

<script>
import DashboardRestForm from '@/components/dashboard/form/DashboardRestForm'
import DashboardStreamForm from '@/components/dashboard/form/DashboardStreamForm'
import { SET_DASHBOARD_ACTIVE_TAB } from '@/store/mutations'
export default {
  name: 'DashboardForm',
  components: {DashboardRestForm, DashboardStreamForm},
  computed: {
    activeTabIndex: {
      get () {
        return this.$store.state.dashboard.forms.activeTabIndex
      },
      set (activeTabIndex) {
        return this.$store.commit(SET_DASHBOARD_ACTIVE_TAB, activeTabIndex)
      }
    }
  }
}
</script>
