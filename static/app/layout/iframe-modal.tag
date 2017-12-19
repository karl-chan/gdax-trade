<iframe-modal>
    <div id="{opts.modalId}" class="modal bottom-sheet">
        <a class="btn-floating btn-large waves-effect waves-light teal launch-icon" onclick={launch}>
            <i class="material-icons">launch</i>
        </a>
        <iframe class="modal-content" src={opts.src}>
    </div>

    <style>
        iframe-modal .modal.bottom-sheet {
            max-height: 100%;
            overflow: visible;
        }

        iframe-modal .modal .launch-icon {
            position: absolute;
            top: -25px;
            right: 100px;
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