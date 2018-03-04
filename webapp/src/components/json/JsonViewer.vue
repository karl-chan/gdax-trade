<template lang="pug">
  v-layout(column)
    v-flex.subheading(v-html="description")
    v-flex(v-if="pagination")
        v-btn(v-if="pagination.before" color="primary" @click="paginateBefore")
          v-icon(left dark) chevron_left
          | {{pagination.before}}
        v-btn(v-if="pagination.after" color="primary" @click="paginateAfter")
          | {{pagination.after}}
          v-icon(right dark) chevron_right

    v-flex.body-1.error--text(v-if="error") {{error}}
    v-flex
      JsonTree(v-if="value" :raw="value")

</template>

<script>
import JsonTree from 'vue-json-tree'
export default {
  name: 'JsonViewer',
  props: ['description', 'error', 'pagination', 'value'],
  components: {JsonTree},
  methods: {
    paginateBefore () {
      this.$emit('paginateBefore', this.pagination.before)
    },
    paginateAfter () {
      this.$emit('paginateAfter', this.pagination.after)
    }
  }
}
</script>
