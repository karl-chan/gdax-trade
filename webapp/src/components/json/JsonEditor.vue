<template lang="pug">
  v-text-field(:label="label" :rules="[rules.json]" :value="value" @input="updateValue($event)" placeholder="Enter JSON (e.g. {})" multi-line)
</template>

<script>
export default {
  name: 'JsonEditor',
  props: ['label', 'value'],
  data: function () {
    return {
      rules: {
        json: function (value) {
          if (!value) {
            return true
          }
          try {
            JSON.parse(value)
          } catch (err) {
            return 'Please enter valid JSON payload'
          }
          return true
        }
      }
    }
  },
  methods: {
    updateValue (newValue) {
      this.$emit('input', newValue)
    }
  }
}
</script>
