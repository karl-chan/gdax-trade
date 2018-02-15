<iframe-modal>
    <div id="{opts.modalId}" class="modal bottom-sheet">
        <div class="buttons">
            <a class="button btn-floating btn-large waves-effect waves-light teal" onclick={launch}>
                <i class="material-icons">launch</i>
            </a>
            <a class="button btn-floating btn-large waves-effect waves-light red modal-close">
                <i class="material-icons">clear</i>
            </a>
        </div>
        <iframe class="modal-content" src={opts.src}>
    </div>

    <style>
        iframe-modal .modal.bottom-sheet {
            max-height: 100%;
            overflow: visible;
        }

        iframe-modal .modal .buttons {
            position: absolute;
            top: -25px;
            right: 50px;
        }

        iframe-modal .modal .buttons .button {
            margin-left: 20px;
        }

        iframe-modal .modal .modal-content {
            padding: 0
        }

        iframe-modal .modal iframe {
            width: 100%;
            height: 90vh;
        }
    </style>

    <script>
        this.on('mount', () => {
            $(`#${this.opts.modalId}`).modal();
        });

        launch() {
            window.open(this.opts.src, '_blank');
        }
    </script>
</iframe-modal>