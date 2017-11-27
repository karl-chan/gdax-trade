<docs>
    <div id="docs" class="modal bottom-sheet">
        <iframe class="modal-content" src="https://docs.gdax.com/">
    </div>

    <style>
        #docs.modal.bottom-sheet {
            max-height: 100%
        }

        #docs.modal .modal-content {
            padding: 0
        }

        #docs iframe {
            width: 100%;
            height: 90vh;
        }
    </style>

    <script>
        this.on('mount', () => {
            $('#docs').modal();
        });
    </script>
</docs>