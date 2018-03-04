<template lang="pug">
  v-container.pa-0(fluid fill-height grid-list-xl)
    v-layout(row)
      v-flex(xs6)
        DashboardForm
      v-flex.grey.lighten-2(xs6)
        DashboardResults
</template>

<script>
import DashboardForm from '@/components/dashboard/panel/DashboardForm'
import DashboardResults from '@/components/dashboard/panel/DashboardResults'
export default {
  name: 'Dashboard',
  components: { DashboardForm, DashboardResults }
}
</script>
