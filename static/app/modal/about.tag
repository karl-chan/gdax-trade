<about>
    <div id="about" class="modal">
        <div class="modal-content">
            <h4>About App</h4>
            <p class="valign-wrapper">
                <i class="material-icons copyright">copyright</i>
                Karl Chan 2017 -
            </p>
        </div>
        <div class="modal-footer">
            <a href="#!" class="modal-action modal-close btn purple lighten-1">Close</a>
        </div>
    </div>

    <style>
        .copyright {
            margin-right: 10px;
        }
    </style>

    <script>
        this.on('mount', () => {
            $('#about').modal();
        });
    </script>
</about>